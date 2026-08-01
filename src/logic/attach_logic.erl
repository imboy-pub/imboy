-module(attach_logic).
-compile([nowarn_deprecated_catch]).
%%%
% 附件业务逻辑层（Garage S3 多桶直传链路 + 统一资源访问控制）
%
% 三条职责：
%   presign/5   按 scope 选桶 + 校验上传权 + 生成绑定 uid 的 presigned PUT URL
%   confirm/5   PUT 成功回调：写归属守卫 + 上传权校验 + HEAD 核实 + 落库(含 scope)
%   view_url/2  读鉴权 authorize/2（六分支）后，public 直读公开 URL，受限资源签短时 GET
%
% 归属两件独立的事：
%   写归属(上传权) = object_key 的 u<Uid>/ 前缀 + can_upload/3 的 scope 权限
%   读归属(访问权) = authorize/2 按 scope 分发（public/private/c2c/group/channel/moment）
% 详见 docs/architecture/resource-access-control.md。
%%%

-export([presign/5, confirm/5, view_url/2]).

-ifdef(TEST).
%% 纯函数，导出仅为可直接验收「不做套件协商」这一 fail-closed 语义
-export([normalize_cipher/1]).
-endif.

-include("log.hrl").

%% 上传 URL 有效期：1 小时（足够单文件上传）
-define(PUT_EXPIRES, 3600).
%% 下载 URL 有效期：受限资源统一 600s（不分级，见设计文档第 8 节）
-define(GET_EXPIRES, 600).

%% ===================================================================
%% presign：选桶 + 上传权校验 + 生成 PUT URL
%% ===================================================================

-spec presign(integer(), binary(), binary(), binary(), binary() | undefined) ->
    {ok, map()} | {error, invalid_file_type | forbidden | upload_not_supported}.
presign(Uid, FileName, MimeType, Scope, ScopeRef) ->
    case elib_oss:validate_file_type(MimeType) of
        false ->
            {error, invalid_file_type};
        true ->
            case can_upload(Uid, Scope, ScopeRef) of
                ok ->
                    ObjectKey = elib_oss:build_object_key(Uid, Scope, ScopeRef, FileName),
                    Bucket = elib_oss:get_bucket(Scope),
                    PutUrl = elib_oss:presign_put_for_key(
                        Bucket, ObjectKey, MimeType, ?PUT_EXPIRES
                    ),
                    %% #20：登记待确认。不登记的话，"PUT 上去但从不 confirm"
                    %% 的对象在库里没有任何行，attachment_repo 的孤儿清理
                    %% （只扫 attachment 表）永远看不见它，空间收不回来。
                    %% 登记失败不阻断签发 —— 拿不到 URL 是功能故障，
                    %% 漏登记只是这一个对象暂时收不回，代价不对等。
                    _ =
                        case attachment_ds:pending_add(ObjectKey, Bucket, Scope, Uid) of
                            ok ->
                                ok;
                            {error, PendReason} ->
                                ?ERROR_LOG([
                                    "attach_logic presign pending_add failed: ",
                                    ObjectKey,
                                    PendReason
                                ])
                        end,
                    Base = #{
                        <<"put_url">> => PutUrl,
                        <<"object_key">> => ObjectKey,
                        <<"expires_at">> => erlang:system_time(second) + ?PUT_EXPIRES
                    },
                    {ok, maybe_public_url(Scope, ObjectKey, Base)};
                {error, _} = E ->
                    E
            end
    end.

%% ===================================================================
%% confirm：写归属守卫 + 上传权校验 + HEAD 核实 + 落库
%% ===================================================================

-spec confirm(integer(), binary(), binary(), binary() | undefined, map()) ->
    {ok, map()}
    | {error,
        forbidden_key
        | forbidden
        | upload_not_supported
        | invalid_key
        | object_not_found
        | file_too_large
        | invalid_file_type
        | term()}.
confirm(Uid, ObjectKey, Scope, ScopeRef, Meta) ->
    case elib_oss:owner_of_key(ObjectKey) of
        {ok, Uid} ->
            case can_upload(Uid, Scope, ScopeRef) of
                ok ->
                    verify_and_save(Uid, ObjectKey, Scope, ScopeRef, Meta);
                {error, _} = E ->
                    E
            end;
        {ok, _Other} ->
            {error, forbidden_key};
        {error, R} ->
            {error, R}
    end.

%% @doc 服务端 HEAD 核实真实性后落库（真实值覆盖客户端自报值）
-spec verify_and_save(integer(), binary(), binary(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
verify_and_save(Uid, ObjectKey, Scope, ScopeRef, Meta) ->
    Bucket = elib_oss:get_bucket(Scope),
    case elib_oss:head_object(Bucket, ObjectKey) of
        {error, not_found} ->
            {error, object_not_found};
        {error, R} ->
            {error, R};
        {ok, #{size := RealSize, content_type := RealType}} ->
            case RealSize > elib_oss:max_file_size() of
                true ->
                    _ = (catch elib_oss:delete_object(Bucket, ObjectKey)),
                    {error, file_too_large};
                false ->
                    case elib_oss:validate_file_type(RealType) of
                        false ->
                            _ = (catch elib_oss:delete_object(Bucket, ObjectKey)),
                            {error, invalid_file_type};
                        true ->
                            do_save(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType)
                    end
            end
    end.

-spec do_save(
    integer(), binary(), binary(), binary() | undefined, map(), non_neg_integer(), binary()
) ->
    {ok, map()} | {error, term()}.
do_save(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType) ->
    case normalize_cipher(maps:get(<<"cipher">>, Meta, undefined)) of
        {error, unsupported_cipher} ->
            %% fail-closed：不做套件协商。落库成 null 会把密文对象**标成明文**，
            %% 那才是真正危险的降级——日后回迁盘点会漏掉它，读取侧也会当明文直读。
            {error, unsupported_cipher};
        Cipher ->
            do_save_1(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType, Cipher)
    end.

%% 只接受 null（明文，含全部旧客户端）与冻结的唯一套件名。
%% 与客户端 AttachmentDescriptor.supportedCipher 保持同一个值。
normalize_cipher(undefined) -> null;
normalize_cipher(null) -> null;
normalize_cipher(<<>>) -> null;
normalize_cipher(<<"AES-256-GCM">>) -> <<"AES-256-GCM">>;
normalize_cipher(_) -> {error, unsupported_cipher}.

do_save_1(Uid, ObjectKey, Scope, ScopeRef, Meta, RealSize, RealType, Cipher) ->
    Attach = #{
        %% file_hash256（SHA-256）仅作完整性参考，不作安全边界。
        %% 双读兼容：新客户端传 file_hash256，旧客户端过渡期仍传 md5。
        %% ⚠️ E2EE-061 拍板 ①：cipher 非 NULL 时这里存的是**密文**哈希，
        %% 明文哈希只在客户端加密的 attachment_descriptor 内，永不到达服务端。
        <<"file_hash256">> => maps:get(
            <<"file_hash256">>, Meta, maps:get(<<"md5">>, Meta, <<>>)
        ),
        %% 密文判别位（迁移 000050）。null = 明文对象，旧客户端语义完全不变。
        <<"cipher">> => Cipher,
        %% mime_type/size 一律采用服务端 HEAD 核实的真实值
        <<"mime_type">> => RealType,
        <<"name">> => filename:basename(ObjectKey),
        %% path/url 存 ObjectKey（受限读取经 view_url 签发，public 直读公开 URL）
        <<"path">> => ObjectKey,
        <<"url">> => ObjectKey,
        <<"size">> => RealSize,
        <<"scope">> => Scope,
        <<"scope_ref">> => normalize_ref(ScopeRef)
    },
    Now = elib_dt:now(),
    try
        ok = elib_pg:with_tx(fun(Conn) ->
            attachment_ds:save(Conn, Now, Uid, [Attach])
        end),
        %% #20：转正后销账。放在事务**之后**——销早了若落库失败，
        %% 对象既不在 attachment 表也不在 pending 表，就永久收不回了。
        %% 销账失败也不会误删：attach_pending_repo:list_expired/1 带
        %% `NOT EXISTS (attachment.path = object_key)` 守卫，已转正的对象
        %% 清理器一律不碰。但仍要留痕，残留行会一直被扫到。
        _ =
            case attachment_ds:pending_remove(ObjectKey) of
                ok ->
                    ok;
                {error, PendReason} ->
                    ?ERROR_LOG([
                        "attach_logic confirm pending_remove failed: ",
                        ObjectKey,
                        PendReason
                    ])
            end,
        {ok, maybe_public_url(Scope, ObjectKey, #{<<"object_key">> => ObjectKey})}
    catch
        Class:Reason ->
            ?ERROR_LOG(["attach_logic confirm save failed: ", Class, Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% 上传权校验 can_upload/3（confirm/presign 共用）
%% ===================================================================

-spec can_upload(integer(), binary(), binary() | undefined) ->
    ok | {error, forbidden | upload_not_supported}.
can_upload(_Uid, <<"public">>, _ScopeRef) ->
    ok;
can_upload(_Uid, <<"private">>, _ScopeRef) ->
    ok;
can_upload(Uid, <<"c2c">>, ScopeRef) ->
    case conv_key_vo:c2c_members(ScopeRef) of
        {ok, {A, B}} when Uid =:= A; Uid =:= B -> ok;
        _ -> {error, forbidden}
    end;
can_upload(Uid, <<"group">>, ScopeRef) ->
    case to_int(ScopeRef) of
        {ok, Gid} ->
            case group_member_ds:is_member(Gid, Uid) of
                true -> ok;
                false -> {error, forbidden}
            end;
        error ->
            {error, forbidden}
    end;
%% channel 上传侧本期未上线，dispatch 占位（读鉴权 authorize/2 已就位）
can_upload(_Uid, <<"channel">>, _ScopeRef) ->
    {error, upload_not_supported};
%% moment：任何登录用户可为自己的动态上传媒体（scope_ref 发帖时未知，
%% 上传放行；真正可见性在读时由 authorize_moment 按帖子 ACL 卡）。
%% 发帖后 moment_logic:create_post 会把这些附件 scope_ref 回填成 momentId。
can_upload(_Uid, <<"moment">>, _ScopeRef) ->
    ok;
can_upload(_Uid, _Scope, _ScopeRef) ->
    {error, forbidden}.

%% ===================================================================
%% 读取鉴权 view_url/2 + authorize/2（六分支全就位）
%% ===================================================================

-spec view_url(integer(), binary()) -> {ok, binary()} | {error, forbidden}.
view_url(Uid, ObjectKey) ->
    case attachment_ds:find_by_path(ObjectKey) of
        {ok, Rec} ->
            case authorize(Uid, Rec) of
                true -> sign_view(Rec, ObjectKey);
                false -> {error, forbidden}
            end;
        {error, not_found} ->
            {error, forbidden};
        {error, Reason} ->
            %% fail-closed：归属查询失败时拒绝签发（不降级放行），
            %% 避免存储/DB 故障成为越权下载入口。
            ?ERROR_LOG([
                "attach_logic:view_url lookup failed (deny), uid=",
                Uid,
                " key=",
                ObjectKey,
                " reason=",
                Reason
            ]),
            {error, forbidden}
    end.

%% @doc 鉴权通过后出 URL：public 直读公开 URL（不签名），受限资源签短时 GET。
-spec sign_view(map(), binary()) -> {ok, binary()}.
sign_view(#{<<"scope">> := <<"public">>}, <<"u", _/binary>> = ObjectKey) ->
    {ok, elib_oss:public_url_for_key(ObjectKey)};
sign_view(Rec, <<"/", _/binary>> = ObjectKey) ->
    %% 旧 go-fastdfs 历史附件：path 以 / 开头，非 Garage object_key（Garage key 恒为 u<Uid>/）。
    %% Garage 无此对象，签 GET 必 404 → 回退返回存储 url（经 nginx 反代可达）；
    %% url 缺失则用 path 本身（相对 fastdfs URL）。
    Url =
        case maps:get(<<"url">>, Rec, null) of
            null -> ObjectKey;
            <<>> -> ObjectKey;
            U -> U
        end,
    {ok, Url};
sign_view(Rec, ObjectKey) ->
    Scope = maps:get(<<"scope">>, Rec, <<"private">>),
    Bucket = elib_oss:get_bucket(Scope),
    {ok, elib_oss:presign_get_for_key(Bucket, ObjectKey, ?GET_EXPIRES)}.

%% @doc 读归属鉴权分发：Rec = #{scope, scope_ref, creator_user_id}
-spec authorize(integer(), map()) -> boolean().
authorize(Uid, Rec) ->
    authorize(maps:get(<<"scope">>, Rec, <<"private">>), Uid, Rec).

-spec authorize(binary(), integer(), map()) -> boolean().
authorize(<<"public">>, _Uid, _Rec) ->
    true;
authorize(<<"private">>, Uid, Rec) ->
    Uid =:= maps:get(<<"creator_user_id">>, Rec, 0);
authorize(<<"c2c">>, Uid, Rec) ->
    case conv_key_vo:c2c_members(scope_ref(Rec)) of
        {ok, {A, B}} -> Uid =:= A orelse Uid =:= B;
        _ -> false
    end;
authorize(<<"group">>, Uid, Rec) ->
    case to_int(scope_ref(Rec)) of
        {ok, Gid} -> group_member_ds:is_member(Gid, Uid);
        error -> false
    end;
authorize(<<"channel">>, Uid, Rec) ->
    case to_int(scope_ref(Rec)) of
        {ok, ChannelId} -> channel_subscription_ds:is_subscribed(ChannelId, Uid);
        error -> false
    end;
authorize(<<"moment">>, Uid, Rec) ->
    case to_int(scope_ref(Rec)) of
        {ok, MomentId} -> authorize_moment(Uid, MomentId);
        error -> false
    end;
authorize(_Scope, _Uid, _Rec) ->
    false.

%% @doc 朋友圈动态：先取 Post 再按可见性规则判定（复用 moment_ds 现成 ACL）
-spec authorize_moment(integer(), integer()) -> boolean().
authorize_moment(Uid, MomentId) ->
    case moment_ds:get_post(MomentId) of
        Post when is_map(Post), map_size(Post) > 0 ->
            moment_ds:can_view_post(Uid, Post);
        _ ->
            false
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

-spec maybe_public_url(binary(), binary(), map()) -> map().
maybe_public_url(<<"public">>, ObjectKey, Map) ->
    Map#{<<"public_url">> => elib_oss:public_url_for_key(ObjectKey)};
maybe_public_url(_Scope, _ObjectKey, Map) ->
    Map.

-spec scope_ref(map()) -> binary() | undefined.
scope_ref(Rec) ->
    case maps:get(<<"scope_ref">>, Rec, undefined) of
        null -> undefined;
        V -> V
    end.

-spec to_int(binary() | integer() | undefined) -> {ok, integer()} | error.
to_int(I) when is_integer(I) ->
    {ok, I};
to_int(B) when is_binary(B) ->
    try
        {ok, binary_to_integer(B)}
    catch
        _:_ -> error
    end;
to_int(_) ->
    error.

-spec normalize_ref(binary() | undefined) -> binary() | null.
normalize_ref(undefined) -> null;
normalize_ref(<<>>) -> null;
normalize_ref(R) -> R.
