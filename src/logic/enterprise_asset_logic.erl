-module(enterprise_asset_logic).

%%%
% EPGZ-04 INT-07/08 企业附件（Application 域 Garage presign -> PUT -> confirm）。
%
% 链路复用（零新逻辑桩）：presign 纯签名计算（elib_oss:presign_put_for_key
% 复用 lib 层；object key 企业域前缀 eoa/<org>/<app>/ 与个人 u<Uid>/ 物理
% 隔离）；confirm 复用 HEAD 核实模式（elib_oss:head_object，服务端真实值
% 覆盖客户端自报值——与 attach_logic:verify_and_save 同一范式），差异是
% ownership 校验维度从 uid 换成 (org, app)（scope 校验）+ confirm 与
% file.confirmed webhook 事件同事务原子。
%
% 固定非 E2EE：企业托管附件为服务端明文对象（attachment.cipher 恒 null），
% 不触个人附件加密链（attachment_cipher 00000052 的客户端加密语义）。
%%%

-export([presign_tx/3, confirm_tx/3]).

-include("log.hrl").

-define(PUT_EXPIRES, 3600).

%%%===================================================================
%%% INT-07 presign
%%%===================================================================

%% @doc 签发短期上传 URL。
%% Input（atom 键）：
%%   file_name  必填 binary 1..256
%%   mime_type  必填 binary（elib_oss 白名单预检；confirm 以 HEAD 真实值复核）
%%   size_bytes 可选 integer（>0 且 <= elib_oss:max_file_size() 预检）
%% 返回 {ok, #{file_id, object_key, put_url, expires_at}}。
-spec presign_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
presign_tx(Conn, Ctx, Input) when is_map(Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    FileName = maps:get(file_name, Input, undefined),
    MimeType = maps:get(mime_type, Input, undefined),
    SizeHint = maps:get(size_bytes, Input, undefined),
    case valid_name(FileName) andalso valid_mime(MimeType) andalso valid_size_hint(SizeHint) of
        true ->
            ObjectKey = enterprise_asset_repo:build_object_key(OrgId, AppId, FileName),
            Bucket = elib_oss:get_bucket(<<"enterprise">>),
            PutUrl = elib_oss:presign_put_for_key(Bucket, ObjectKey, MimeType, ?PUT_EXPIRES),
            %% pending 登记（迁移 54 生命周期）：失败不阻断签发（与 attach_logic
            %% 同口径——拿不到 URL 是功能中断，漏登记只影响孤儿回收）。
            _ =
                case
                    enterprise_asset_repo:pending_add_tx(Conn, ObjectKey, Bucket, <<"enterprise">>)
                of
                    {ok, inserted} ->
                        ok;
                    {error, Reason} ->
                        ?ERROR_LOG([
                            "enterprise_asset presign pending_add failed: ", ObjectKey, Reason
                        ])
                end,
            {ok, #{
                <<"object_key">> => ObjectKey,
                <<"put_url">> => PutUrl,
                <<"expires_at">> => erlang:system_time(second) + ?PUT_EXPIRES
            }};
        false ->
            {error, {<<"invalid_request">>, invalid_presign_input}}
    end;
presign_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

%%%===================================================================
%%% INT-08 confirm
%%%===================================================================

%% @doc HEAD/size/hash/scope 校验后确认。
%% Input（atom 键）：
%%   object_key    必填 binary（必须是本 org/app 域 eoa/ 前缀且已 presign 登记）
%%   file_hash256  可选 binary（64 位小写/大写 hex；SHA-256 完整性参考）
%% 返回 {ok, #{file_id, object_key, size, mime_type}}；
%% 失败 {error, {Code, Detail}}，超限/非法类型对象被删除（与 attach_logic 同口径）。
%%
%% 取舍说明：HEAD 核实在调用方事务内执行（网络 IO，上限 10s）——为换
%% attachment 行、pending 销账、file.confirmed 事件的原子性（一期一致性
%% 优先；attach_logic 个人链把 HEAD 放事务外是重试幂等语义，此处 confirm
%% 由幂等键保护重试，等价安全）。
-spec confirm_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
confirm_tx(Conn, Ctx, Input) when is_map(Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    ObjectKey = maps:get(object_key, Input, undefined),
    Hash = maps:get(file_hash256, Input, undefined),
    case
        is_binary(ObjectKey) andalso ObjectKey =/= <<>> andalso
            enterprise_asset_repo:owned_key(ObjectKey, OrgId, AppId) andalso
            valid_hash(Hash)
    of
        true ->
            confirm_owned(Conn, Ctx, ObjectKey, Hash);
        false ->
            {error, {<<"invalid_request">>, invalid_confirm_input}}
    end;
confirm_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

confirm_owned(Conn, Ctx, ObjectKey, Hash) ->
    case enterprise_asset_repo:pending_find_tx(Conn, ObjectKey) of
        {ok, _} ->
            confirm_head(Conn, Ctx, ObjectKey, Hash);
        {error, not_found} ->
            {error, {<<"invalid_request">>, not_presigned}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

confirm_head(Conn, Ctx, ObjectKey, Hash) ->
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    case elib_oss:head_object(Bucket, ObjectKey) of
        {error, not_found} ->
            {error, {<<"invalid_request">>, object_not_found}};
        {error, Reason} ->
            ?ERROR_LOG(["enterprise_asset confirm head failed: ", Bucket, ObjectKey, Reason]),
            {error, {<<"internal_error">>, Reason}};
        {ok, #{size := RealSize, content_type := RealType}} ->
            case RealSize > elib_oss:max_file_size() of
                true ->
                    _ =
                        try
                            elib_oss:delete_object(Bucket, ObjectKey)
                        catch
                            _:_ -> ok
                        end,
                    {error, {<<"invalid_request">>, file_too_large}};
                false ->
                    case elib_oss:validate_file_type(RealType) of
                        false ->
                            _ =
                                try
                                    elib_oss:delete_object(Bucket, ObjectKey)
                                catch
                                    _:_ -> ok
                                end,
                            {error, {<<"invalid_request">>, invalid_file_type}};
                        true ->
                            save_confirmed(Conn, Ctx, ObjectKey, Hash, RealSize, RealType)
                    end
            end
    end.

save_confirmed(Conn, Ctx, ObjectKey, Hash, RealSize, RealType) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    ScopeRef = enterprise_asset_repo:scope_ref(OrgId, AppId),
    Info = #{
        scope_ref => ScopeRef,
        origin_kind => <<"enterprise_application">>,
        origin_application_id => AppId
    },
    InfoMap =
        case Hash of
            undefined -> Info#{file_hash256 => <<>>};
            _ -> Info#{file_hash256 => Hash}
        end,
    case enterprise_asset_repo:confirm_save_tx(Conn, ObjectKey, RealType, RealSize, InfoMap) of
        {ok, AttId} ->
            ok = enterprise_asset_repo:pending_remove_tx(Conn, ObjectKey),
            %% file.confirmed 事件与转正同事务原子（订阅判定/SSRF guard 在
            %% emit 内；guard 拒绝只跳过事件不阻断 confirm——事件是旁路）。
            _ = enterprise_webhook_logic:emit_event_tx(
                Conn,
                Ctx,
                <<"file.confirmed">>,
                #{resource_type => <<"attachment">>, resource_id => AttId}
            ),
            {ok, #{
                <<"file_id">> => AttId,
                <<"object_key">> => ObjectKey,
                <<"size">> => RealSize,
                <<"mime_type">> => RealType
            }};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

valid_name(N) when is_binary(N), byte_size(N) > 0, byte_size(N) =< 256 ->
    filename:basename(N) =/= <<>>;
valid_name(_) ->
    false.

valid_mime(M) when is_binary(M), M =/= <<>> ->
    elib_oss:validate_file_type(M);
valid_mime(_) ->
    false.

valid_size_hint(undefined) ->
    true;
valid_size_hint(S) when is_integer(S), S > 0 ->
    S =< elib_oss:max_file_size();
valid_size_hint(_) ->
    false.

valid_hash(undefined) ->
    true;
valid_hash(H) when is_binary(H), byte_size(H) =:= 64 ->
    HexChars = "0123456789abcdefABCDEF",
    lists:all(fun(C) -> lists:member(C, HexChars) end, binary_to_list(H));
valid_hash(_) ->
    false.
