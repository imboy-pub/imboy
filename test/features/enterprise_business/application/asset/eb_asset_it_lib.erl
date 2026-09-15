%%% @doc EB-07 企业附件闭环的**集成夹具库**（test-only）。
%%%
%%% 只放「可复用的事实」：合成租户/消息/hold/成员/经办夹具、经装配解析的端口、
%%% 走公共 facade 的调用封装、只读探针、以及断言小工具。Acceptance 场景本身在
%%% `eb_asset_it_scenarios`（一个 Acceptance 一个函数），账本也在那里 —— 保持
%%% 单向依赖：scenarios → lib。
%%%
%%% 边界（硬约束）：只用**合成**租户（随机 TSID）与**本地替身**对象存储；
%%% 不触真实账号 / 联系方式 / 生产资源；不声明真实 Garage 验收通过
%%% （口径固定 `adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`）。
-module(eb_asset_it_lib).

-export([
    new_scope/0,
    with_scope/2,
    key_ref/1,
    store/0,
    asset/0,
    clock/0,
    id/0,
    auth/0,
    tenant/1,
    facade_presign/3,
    facade_confirm/4,
    facade_content/3,
    facade_content/5,
    upload_and_confirm/3,
    seed_pending/4,
    seed_message/2,
    seed_hold/2,
    seed_foreign_expired_pending/2,
    add_member/2,
    assignment_for/5,
    suspend_member/2,
    asset_status/3,
    asset_hash/3,
    asset_row/3,
    message_retain_until/3,
    message_row_count/3,
    object_present/3,
    object_present_key/3,
    object_key/3,
    key_prefix/2,
    personal_attachment_rows/1,
    expect_error_match/2,
    expect_db_error/4,
    no_storage_reference/4,
    forbidden_keys/1,
    sha256_hex/1,
    now_sec/0,
    reason/1,
    fmt/1
]).

%% ===================================================================
%% 合成租户 / 密钥
%% ===================================================================

%% @doc 一套合成租户（随机 TSID；`eb_pg_test_fixture:new_scope/0`）。
%%
%% 额外种入 `eb_key_ref`：**整套流水必须共用同一把企业托管主密钥**（presign 用哪把、
%% PUT/confirm 就必须用哪把）。每次调用现生成新密钥会让 `Crypto:open/3` 的
%% `key_version` 校验失败，把「凭证不可解」误报成「凭证非法」。
-spec new_scope() -> map().
new_scope() ->
    Scope = eb_pg_test_fixture:new_scope(),
    Scope#{eb_key_ref => eb_pg_test_fixture:key_ref()}.

-spec with_scope(map(), fun((map()) -> T)) -> T.
with_scope(Scope, Fun) ->
    try
        Fun(Scope)
    after
        _ = eb_pg_test_fixture:cleanup(Scope)
    end.

%% @doc 该合成租户的企业托管主密钥引用（32 字节随机；**非**生产密钥）。
-spec key_ref(map()) -> map().
key_ref(Scope) when is_map(Scope) ->
    maps:get(eb_key_ref, Scope).

%% ===================================================================
%% 端口（一律经装配解析：证明用例层消费的是 EB-03R 的装配事实）
%% ===================================================================

store() -> resolve(store).
asset() -> resolve(asset).
clock() -> resolve(clock).
id() -> resolve(id).
auth() -> resolve(auth).

resolve(Key) ->
    {ok, Mod} = eb_infra_ports:resolve(Key),
    Mod.

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

%% ===================================================================
%% 公共 API 调用（走 facade，与最终调用面一致）
%% ===================================================================

facade_presign(Scope, Actor, Opts) ->
    enterprise_business_facade:request_presign(
        maps:get(org_id, Scope),
        base_params(Scope, Actor, Opts)
    ).

facade_confirm(Scope, Actor, UploadRef, ConversationId) ->
    enterprise_business_facade:confirm_asset(
        maps:get(org_id, Scope),
        (base_params(Scope, Actor, #{}))#{
            upload_ref => UploadRef,
            conversation_id => ConversationId
        }
    ).

facade_content(Scope, Actor, AssetId) ->
    {Org, Ws} = tenant(Scope),
    facade_content(Scope, Actor, AssetId, Org, Ws).

facade_content(Scope, Actor, AssetId, OrgId, WorkspaceId) ->
    Params = (base_params(Scope, Actor, #{}))#{
        asset_id => AssetId,
        workspace_id => WorkspaceId
    },
    enterprise_business_facade:content_stream(OrgId, Params).

base_params(Scope, Actor, Opts) ->
    maps:merge(
        #{
            workspace_id => maps:get(workspace_id, Scope),
            actor_user_id => Actor,
            key_ref => key_ref(Scope),
            upload_ttl_seconds => 900
        },
        Opts
    ).

%% ===================================================================
%% 上传 / 确认流水（presign → PUT → confirm）
%% ===================================================================

%% @doc 走完整流水：presign → put_object → confirm；返回 `{ok, AssetId, UploadRef}`。
upload_and_confirm(Scope, Actor, Marker) ->
    Conv = maps:get(conversation_id, Scope),
    Payload = <<Marker/binary, "-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
    {ok, Presign} = facade_presign(Scope, Actor, #{
        conversation_id => Conv,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => sha256_hex(Payload)
    }),
    AssetId = maps:get(asset_id, Presign),
    Ref = maps:get(upload_ref, Presign),
    {ok, _} = put_payload(Scope, Actor, Ref, Payload),
    case facade_confirm(Scope, Actor, Ref, Conv) of
        {ok, Confirmed} ->
            case maps:get(status, Confirmed, undefined) of
                active -> {ok, AssetId, Ref};
                Other -> {error, {confirm_status, Other}}
            end;
        Other ->
            {error, {confirm_failed, Other}}
    end.

%% @doc 建一个 pending 资产；`Age = stale` 时把 `created_at` 推到 2 小时前（夹具动作）。
seed_pending(Scope, Actor, Marker, Age) ->
    {Org, Ws} = tenant(Scope),
    Conv = maps:get(conversation_id, Scope),
    Payload = <<Marker/binary, "-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
    {ok, Presign} = facade_presign(Scope, Actor, #{
        conversation_id => Conv,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => sha256_hex(Payload)
    }),
    AssetId = maps:get(asset_id, Presign),
    {ok, _} = put_payload(Scope, Actor, maps:get(upload_ref, Presign), Payload),
    case Age of
        stale ->
            ok = eb_pg_test_fixture:exec(
                <<
                    "UPDATE enterprise_asset SET created_at = now() - interval '2 hours'"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
                >>,
                [Org, Ws, AssetId]
            );
        fresh ->
            ok
    end,
    AssetId.

%% @doc 客户端按 presigned PUT 写入私有桶（本地替身下即 `eb_asset_app:put_object/2`）。
put_payload(Scope, Actor, UploadRef, Payload) ->
    {Org, Ws} = tenant(Scope),
    eb_asset_app:put_object(Org, #{
        workspace_id => Ws,
        actor_user_id => Actor,
        conversation_id => maps:get(conversation_id, Scope),
        upload_ref => UploadRef,
        payload => Payload,
        key_ref => key_ref(Scope)
    }).

%% ===================================================================
%% 消息 / hold / 成员 / 经办夹具（合成行）
%% ===================================================================

%% @doc 建一条合成 canonical 消息（retain_until = now + 1095 天，对齐 EB-06 的合成时钟）。
seed_message(Scope, ContactId) ->
    {Org, Ws} = tenant(Scope),
    MsgId = eb_pg_test_fixture:id(),
    Retain = now_sec() + (1095 * 86400),
    ok = eb_pg_test_fixture:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,"
            "  client_msg_id,retention_days,retain_until,visibility)"
            " VALUES ($1,$2,$3,$4,'contact',$5,$6,1095,to_timestamp($7),'visible')"
        >>,
        [
            MsgId,
            Org,
            Ws,
            maps:get(conversation_id, Scope),
            ContactId,
            <<"eb07-msg-", (integer_to_binary(MsgId))/binary>>,
            Retain
        ]
    ),
    MsgId.

%% 形状约束 `ck_erh_scope_shape`（00000117:148-153）：scope_type = message 时
%% **只能**带 scope_message_id（带 scope_conversation_id 会被 23514 拒绝）。
seed_hold(Scope, MsgId) ->
    {Org, Ws} = tenant(Scope),
    HoldId = eb_pg_test_fixture:id(),
    {ok, _} = (store()):insert_hold(Org, Ws, #{
        id => HoldId,
        scope_type => <<"message">>,
        scope_message_id => MsgId,
        reason_code => <<"eb07-synthetic-hold">>,
        actor_user_id => maps:get(owner_user_id, Scope)
    }),
    HoldId.

%% @doc 在**另一个**合成租户里种一个「超时 pending」资产 + 对象（跨租户候选）。
%%
%% 刻意不经用例层：用例层只在本作用域内工作，这里要造的是**别人的**行，
%% 目的是证明 cleanup 的候选集被超量供给时也不会越界（A05）。
%% 该租户（OtherOrg/OtherWs）同属本夹具的合成 scope，不是真实租户。
seed_foreign_expired_pending(Scope, Payload) ->
    Org = maps:get(other_org_id, Scope),
    Ws = maps:get(other_workspace_id, Scope),
    AssetId = eb_pg_test_fixture:id(),
    ok = eb_asset_object_stub:put(object_key(Org, Ws, AssetId), Payload, #{}),
    ok = eb_pg_test_fixture:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,status,"
            "  created_at)"
            " VALUES ($1,$2,$3,$4,$5,'text/plain',$6,'pending_confirm',"
            "         now() - interval '2 hours')"
        >>,
        [AssetId, Org, Ws, object_key(Org, Ws, AssetId), sha256_hex(Payload), byte_size(Payload)]
    ),
    AssetId.

add_member(Org, UserId) ->
    case
        eb_pg_test_fixture:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'member','active') ON CONFLICT DO NOTHING"
            >>,
            [Org, UserId]
        )
    of
        ok -> ok;
        Other -> {error, {add_member_failed, Other}}
    end.

assignment_for(Org, Ws, UserId, IdentityId, FunctionKey) ->
    case
        (store()):insert_assignment(Org, Ws, #{
            id => eb_pg_test_fixture:id(),
            business_identity_id => IdentityId,
            function_key => FunctionKey,
            user_id => UserId,
            assigned_by => UserId
        })
    of
        {ok, _} -> ok;
        Other -> {error, {insert_assignment_failed, Other}}
    end.

%% @doc 合成 suspend（EB-08 的 `suspend_member` 用例在 wb-a1 尚不存在，故夹具直改事实行）。
suspend_member(Org, UserId) ->
    case
        eb_pg_test_fixture:exec(
            <<
                "UPDATE organization_member SET status='suspended'"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, UserId]
        )
    of
        ok -> ok;
        Other -> {error, {suspend_failed, Other}}
    end.

%% ===================================================================
%% 只读探针
%% ===================================================================

asset_status(Org, Ws, AssetId) ->
    case
        eb_pg_test_fixture:scalar(
            <<
                "SELECT status FROM enterprise_asset"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, AssetId]
        )
    of
        undefined -> {error, not_found};
        Bin when is_binary(Bin) -> to_atom(Bin);
        Other -> Other
    end.

asset_hash(Org, Ws, AssetId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT object_hash FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, AssetId]
    ).

asset_row(Org, Ws, AssetId) ->
    eb_asset_store:fetch_asset(Org, Ws, AssetId).

message_retain_until(Org, Ws, MsgId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT extract(epoch from retain_until)::bigint FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

message_row_count(Org, Ws, MsgId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

object_present(Org, Ws, AssetId) ->
    object_present_key(Org, Ws, object_key(Org, Ws, AssetId)).

object_present_key(Org, Ws, Key) ->
    case eb_asset_object_stub:get(Key, key_prefix(Org, Ws)) of
        {ok, _} -> true;
        _ -> false
    end.

%% 对象 key 由实现派生（调用方不可传入）；这里是测试侧的**只读**引用。
object_key(Org, Ws, AssetId) ->
    eb_pg_asset_meta:object_key(Org, Ws, AssetId).

key_prefix(Org, Ws) ->
    eb_asset_object_stub:key_prefix(Org, Ws).

%% @doc 个人 private attachment 表在该批合成 user 上的行数（硬约束 5 的机械判据）。
personal_attachment_rows(Scope) ->
    Users = [
        maps:get(owner_user_id, Scope),
        maps:get(actor_user_id, Scope),
        maps:get(peer_user_id, Scope)
    ],
    case
        eb_pg_test_fixture:scalar(
            <<"SELECT count(*) FROM attachment WHERE creator_user_id = ANY($1)">>,
            [Users]
        )
    of
        0 -> ok;
        Other -> {error, {personal_attachment_rows_created, Other}}
    end.

to_atom(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        A -> A
    catch
        _:_ -> Bin
    end.

%% ===================================================================
%% 断言小工具
%% ===================================================================

expect_error_match({error, Pattern}, Fun) ->
    case Fun() of
        {error, Pattern} -> ok;
        {error, Other} -> {error, {wrong_error, expected, Pattern, got, Other}};
        Other -> {error, {expected_error, Pattern, got, Other}}
    end.

%% @doc 断言一条 SQL 被 DB 守卫拒绝：SQLSTATE 与**约束名**都要对上（逐条点名守卫）。
%%
%% 经 `elib_pg:execute/2` 执行并读 `#error{}`；错误归一复用既有
%% `eb_pg_store_sql:normalize_error/1`（读既有事实，不另立一套判定）。
expect_db_error(State, Constraint, Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, _Count} ->
            {error, {expected_db_error, State, Constraint, but_succeeded}};
        {ok, _Count, _Rows} ->
            {error, {expected_db_error, State, Constraint, but_succeeded}};
        {error, Reason} ->
            case classify_db_error(Reason) of
                {sql, State, Constraint} ->
                    ok;
                {sql, OtherState, OtherConstraint} ->
                    {error,
                        {wrong_db_error, expected, {State, Constraint}, got,
                            {OtherState, OtherConstraint}}};
                Other ->
                    {error, {unexpected_db_error, Other}}
            end
    end.

classify_db_error(Reason) ->
    try eb_pg_store_sql:normalize_error(Reason) of
        Normalized -> Normalized
    catch
        _:_ -> {raw, Reason}
    end.

%% @doc 深扫：不得出现任何 storage 引用（URL scheme / 对象 key / 敏感键名）。
%%
%% 判据分两层：
%%   1. 值层：`"://"`（base64 字母表不含 `:`，故不透明 token 不会假红）与实现派生的
%%      对象 key 字面量；
%%   2. 键名层：`object_key` / `endpoint` / `bucket` / `url` 等一律禁止出现在返回体里。
no_storage_reference(Term, Org, Ws, AssetId) ->
    Key = object_key(Org, Ws, AssetId),
    Bin = iolist_to_binary(io_lib:format("~p", [Term])),
    Bad =
        [{scheme_in_value, <<"://">>} || binary:match(Bin, <<"://">>) =/= nomatch] ++
            [{object_key_in_value, Key} || binary:match(Bin, Key) =/= nomatch] ++
            forbidden_keys(Term),
    case Bad of
        [] -> ok;
        _ -> {error, {storage_reference_present, Bad}}
    end.

forbidden_keys(Term) when is_map(Term) ->
    Banned = [
        object_key,
        storage_ref,
        url,
        uri,
        endpoint,
        bucket,
        presigned_url,
        presign_url,
        storage_url,
        object_url,
        path,
        garage_url
    ],
    Own = [{forbidden_key, K} || K <- maps:keys(Term), lists:member(K, Banned)],
    Own ++ lists:append([forbidden_keys(V) || V <- maps:values(Term)]);
forbidden_keys(Term) when is_list(Term) ->
    lists:append([forbidden_keys(V) || V <- Term]);
forbidden_keys(Term) when is_tuple(Term) ->
    forbidden_keys(tuple_to_list(Term));
forbidden_keys(_Other) ->
    [].

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

now_sec() ->
    erlang:system_time(second).

%% @doc 打印用：把任意原因压成单行 UTF-8 短文本（日志可读，且不会把日志撑爆）。
%%
%% 返回 **UTF-8 二进制**：调用方必须用 `~ts` 打印。若 `~0p` 的输出不是合法 UTF-8
%% （例如原因里带了非 UTF-8 的裸字节），退回逐字节转义的 ASCII 形式，避免把无效
%% 字节塞进日志。
reason(Reason) ->
    Bin = iolist_to_binary(io_lib:format("~0p", [Reason])),
    Safe =
        case unicode:characters_to_binary(Bin, utf8, utf8) of
            Utf8 when is_binary(Utf8) -> Utf8;
            _ -> iolist_to_binary(io_lib:format("~0w", [Reason]))
        end,
    case byte_size(Safe) > 400 of
        true -> <<(binary:part(Safe, 0, 400))/binary, "...">>;
        false -> Safe
    end.

%% @doc 描述文本 → UTF-8 二进制（供 `~ts` 打印）。
fmt(Text) ->
    Bin = iolist_to_binary(Text),
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Utf8 when is_binary(Utf8) -> Utf8;
        _ -> iolist_to_binary(io_lib:format("~0w", [Bin]))
    end.
