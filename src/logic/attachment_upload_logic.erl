-module(attachment_upload_logic).
%%%
% multipart 直传端点业务逻辑（POST /api/v1/attachment/upload）
%
% 与 presign→PUT 直传→confirm 链路互补：客户端（moya 小程序 wx.uploadFile、
% curl 调试）把文件 multipart/form-data 直接 POST 给本服务，服务端流式落
% 临时文件后从这里转入 Garage。confirm 仍是唯一落库真源——本端点只收文件，
% 失败可重传，幂等。
%
% 安全面（防越权写他人对象）：
%   1. object_key 第一段必须为 u<Uid>/（elib_oss:owner_of_key）
%   2. object_key 必须存在 presign 登记的 pending 行且 creator_user_id = Uid
%      （attach_pending_repo:get_by_key；mime/scope/bucket 以登记为准）
%   3. mime 守卫与 presign 同口径：teaching scope 走
%      teaching_attach_logic:check_mime 白名单，全局走 elib_oss:validate_file_type
%   4. size 上限对齐 elib_oss:max_file_size()（100MB）
% confirm 阶段仍会 HEAD 核实真实 size/mime，此处守卫是快速失败的前置闸。
%%%

-export([upload_multipart/5]).

-include("log.hrl").

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
            case attach_pending_repo:get_by_key(ObjectKey) of
                {ok, #{<<"creator_user_id">> := Uid, <<"bucket">> := Bucket, <<"scope">> := Scope}} ->
                    guard_and_put(Uid, ObjectKey, MimeType, FilePath, Size, Bucket, Scope);
                {ok, #{<<"creator_user_id">> := _OtherUid}} ->
                    {error, forbidden_key};
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

%% mime 守卫（presign 同口径）→ 大小复核 → 流式 PUT Garage
-spec guard_and_put(
    integer(), binary(), binary(), file:filename_all(), non_neg_integer(), binary(), binary()
) -> {ok, map()} | {error, term()}.
guard_and_put(_Uid, ObjectKey, MimeType, FilePath, Size, Bucket, Scope) ->
    case mime_guard(Scope, MimeType) of
        ok ->
            case Size > elib_oss:max_file_size() of
                true ->
                    {error, file_too_large};
                false ->
                    case elib_oss:put_object_from_file(Bucket, ObjectKey, FilePath, MimeType) of
                        ok ->
                            {ok, #{
                                <<"object_key">> => ObjectKey,
                                <<"mime_type">> => MimeType,
                                <<"size">> => Size
                            }};
                        {error, Reason} ->
                            ?ERROR_LOG([
                                "attachment_upload_logic put_object_from_file failed: ",
                                ObjectKey,
                                Reason
                            ]),
                            %% Garage 写失败 = 服务端存储故障，打标签映射 5xx
                            {error, {storage_error, Reason}}
                    end
            end;
        {error, _} = E ->
            E
    end.

%% teaching scope 走教学白名单（与 presign 的 teaching_presign_guard 同款），
%% 其余 scope 走全局 ALLOWED_TYPES；mime 以 presign 时声明（query 透传）为准，
%% 不信任 multipart part 自带的 Content-Type（客户端可能给错）。
-spec mime_guard(binary(), binary()) -> ok | {error, invalid_file_type}.
mime_guard(<<"teaching">>, MimeType) ->
    teaching_attach_logic:check_mime(MimeType);
mime_guard(_Scope, MimeType) ->
    case elib_oss:validate_file_type(MimeType) of
        true -> ok;
        false -> {error, invalid_file_type}
    end.
