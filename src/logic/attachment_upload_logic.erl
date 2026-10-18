-module(attachment_upload_logic).
%%%
% multipart 直传端点业务逻辑（POST /api/v1/attachment/upload）
%
% 与 presign→PUT 直传→confirm 链路互补：客户端（moya 小程序 wx.uploadFile、
% curl 调试）把文件 multipart/form-data 直接 POST 给本服务，服务端流式落
% 临时文件后从这里转入冻结的存储位置。confirm 仍是唯一落库真源——本端点只收文件，
% 失败可重传，幂等。
%
% 安全面（防越权写他人对象）：
%   1. object_key 第一段必须为 u<Uid>/（elib_oss:owner_of_key）
%   2. object_key 必须存在 presign 登记的 pending 行且 creator_user_id = Uid
%      （attach_pending_repo:status_lookup；mime/scope/位置/期限以登记为准）
%   3. mime 守卫与 presign 同口径：teaching scope 走
%      moya_attach_logic:check_mime 白名单，全局走 elib_oss:validate_file_type
%   4. size 上限对齐 elib_oss:max_file_size()（100MB）
% 实际字节先固定 receipt，再从同一私有文件句柄写入 provider，不能绕过 V5 pin。
% Actual bytes are pinned before I/O; hashing and the provider share one private inode.
%%%

-export([upload_multipart/5]).

-spec upload_multipart(
    integer(), binary(), binary(), file:filename_all(), non_neg_integer()
) ->
    {ok, map()}
    | {error,
        invalid_key
        | forbidden_key
        | object_not_found
        | invalid_file_type
        | file_too_large
        %% 服务端故障标签：handler 映射 5xx，与 4xx 业务拒绝区分
        | {db_error, term()}
        | {storage_error, term()}
        | term()}.
upload_multipart(Uid, ObjectKey, MimeType, FilePath, Size) ->
    case elib_oss:owner_of_key(ObjectKey) of
        {ok, Uid} ->
            case attach_pending_repo:status_lookup(ObjectKey) of
                {ok, #{<<"object_key">> := ObjectKey, <<"creator_user_id">> := Uid,
                       <<"scope">> := Scope} = Row} ->
                    guard_and_put(Uid, ObjectKey, MimeType, FilePath, Size, Scope, Row);
                {ok, #{<<"creator_user_id">> := _OtherUid}} ->
                    {error, forbidden_key};
                {ok, _} ->
                    {error, {db_error, invalid_pending_session}};
                {error, not_found} ->
                    %% 未 presign 登记过：object_key 不是本服务签发的，拒绝
                    {error, object_not_found};
                {error, R} ->
                    %% 非 not_found 的 pending 查询失败 = DB 故障，打标签供
                    %% handler 区分 5xx（避免与业务拒绝混为 400）
                    {error, {db_error, R}}
            end;
        {ok, _OtherUid} ->
            {error, forbidden_key};
        {error, invalid_key} ->
            {error, invalid_key}
    end.

%% MIME/会话守卫 → 冻结位置 → 私有 spool → 原子 pin → 受控 provider PUT。
-spec guard_and_put(
    integer(), binary(), binary(), file:filename_all(), non_neg_integer(), binary(), map()
) -> {ok, map()} | {error, term()}.
guard_and_put(Uid, ObjectKey, MimeType, FilePath, Size, Scope, Row) ->
    case mime_guard(Scope, MimeType) of
        ok ->
            case Size > elib_oss:max_file_size() of
                true ->
                    {error, file_too_large};
                false ->
                    prepare_upload(Uid, ObjectKey, MimeType, FilePath, Scope, Row)
            end;
        {error, _} = E ->
            E
    end.

prepare_upload(Uid, Key, Mime, Path, Scope, Row) ->
    case attachment_storage:frozen_location(Key, Row) of
        {ok, legacy_unknown} ->
            %% 历史未知位置不能用当前默认桶重新解释，须创建新上传会话。
            {error, historical_location_unverified};
        {ok, Identity} ->
            case session_context(Uid, Mime, Scope, Row) of
                {ok, Expiry} ->
                    case attachment_upload_spool:with_source({file, Path}, fun(Source) ->
                        %% Tag callback results separately from local spool failures.
                        {upload_result, pin_and_put(Key, Mime, Source, Identity, Expiry)}
                    end) of
                        {upload_result, Result} -> Result;
                        {error, Reason} -> {error, {storage_error, Reason}}
                    end;
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

session_context(Uid, Mime, Scope, #{
    <<"session_state">> := <<"registered">>,
    <<"client_upload_id">> := ClientId,
    <<"upload_registration">> := #{<<"subject">> := Subject,
        <<"declaration">> := #{<<"mime">> := DeclaredMime, <<"scope">> := DeclaredScope}},
    <<"upload_expires_at">> := ExpiresAt,
    <<"session_deadline">> := Deadline
}) when is_binary(ClientId), byte_size(ClientId) > 0 ->
    Expected = #{<<"kind">> => <<"user">>, <<"principal">> => integer_to_binary(Uid)},
    Expiry = min(unix_seconds(ExpiresAt), unix_seconds(Deadline)),
    case {Subject =:= Expected, Scope =:= DeclaredScope, Mime =:= DeclaredMime,
          erlang:system_time(second) < Expiry} of
        {false, _, _, _} -> {error, forbidden_key};
        {_, false, _, _} -> {error, scope_conflict};
        {_, _, false, _} -> {error, content_mismatch};
        {_, _, _, false} -> {error, session_not_renewable};
        {true, true, true, true} -> {ok, Expiry}
    end;
session_context(_, _, _, _) ->
    {error, session_not_renewable}.

unix_seconds(N) when is_integer(N) -> N;
unix_seconds(Value) ->
    case elib_dt:rfc3339_to(Value, second) of
        N when is_integer(N) -> N;
        _ -> 0
    end.

pin_and_put(Key, Mime, Source, Identity, Expiry) ->
    %% The handler's count is only a fast guard. The receipt and response use real bytes.
    %% handler 的计数只作快速拦截，receipt/响应大小来自实际保留的文件句柄。
    Size = attachment_upload_spool:size(Source),
    case Size > elib_oss:max_file_size() of
        true -> {error, file_too_large};
        false ->
            Receipt = #{<<"version">> => 1,
                <<"sha256">> => attachment_upload_spool:sha256(Source),
                <<"size_bytes">> => integer_to_binary(Size), <<"mime">> => Mime},
            case admission_result(attach_pending_repo:register_content_pin(Key, Receipt)) of
                ok ->
                    case provider_put(Key, Source, Mime, Identity, Expiry, Receipt) of
                        ok -> {ok, #{<<"object_key">> => Key,
                                     <<"mime_type">> => Mime, <<"size">> => Size}};
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end
    end.

provider_put(Key, Source, Mime, Identity, Expiry, Receipt) ->
    Recheck = fun() -> admission_result(
        attach_pending_repo:recheck_content_pin(Key, Receipt, Identity)) end,
    try
        Result = case maps:get(driver, Identity) of
            disk ->
                %% The live DB recheck runs after the physical-object OS lock.
                %% DB 复核必须发生于物理对象 OS 锁内，结束事务后才写字节。
                attachment_storage_disk:put_snapshot(Identity, Source, Expiry, Recheck);
            garage ->
                case Recheck() of
                    ok ->
                        Context = #{
                            issued_at => calendar:universal_time(),
                            expires_at => calendar:gregorian_seconds_to_datetime(
                                Expiry + 62167219200)},
                        attachment_storage_garage:put_snapshot(Identity, Source, Mime, Context);
                    {error, _} = Error -> Error
                end;
            _ -> {error, unsupported_driver}
        end,
        storage_result(Result)
    catch
        _:_ -> {error, {storage_error, put_failed}}
    end.

admission_result(ok) -> ok;
admission_result({error, not_found}) -> {error, object_not_found};
admission_result({error, Reason}) when Reason =:= content_mismatch;
    Reason =:= session_not_renewable; Reason =:= tombstone; Reason =:= location_changed ->
    {error, Reason};
admission_result({error, Reason}) -> {error, {db_error, Reason}}.

storage_result(ok) -> ok;
storage_result({error, {db_error, _}} = Error) -> Error;
storage_result({error, Reason} = Error) when Reason =:= content_mismatch;
    Reason =:= session_not_renewable; Reason =:= tombstone; Reason =:= location_changed;
    Reason =:= object_not_found; Reason =:= invalid_signature_context -> Error;
storage_result({error, Reason}) -> {error, {storage_error, Reason}}.

%% teaching scope 走教学白名单（与 presign 的 teaching_presign_guard 同款），
%% 其余 scope 走全局 ALLOWED_TYPES；query MIME 还必须等于会话的冻结声明，
%% 不信任 multipart part 自带的 Content-Type（客户端可能给错）。
-spec mime_guard(binary(), binary()) -> ok | {error, invalid_file_type}.
mime_guard(<<"teaching">>, MimeType) ->
    moya_attach_logic:check_mime(MimeType);
mime_guard(_Scope, MimeType) ->
    case elib_oss:validate_file_type(MimeType) of
        true -> ok;
        false -> {error, invalid_file_type}
    end.
