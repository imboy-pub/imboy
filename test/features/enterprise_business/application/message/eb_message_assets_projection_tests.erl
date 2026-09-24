%%% @doc CS-BE-01：客服/enterprise 历史消息**资产投影**验收套件（真 scratch DB）。
%%%
%%% 冻结契约：历史消息响应必须投影 `assets:[{id,mime,size_bytes,file_name,status}]`；
%%% 纯文本消息 `assets = []`；投影中**禁止**出现 `object_key/upload_url/token`。
%%%
%%% 覆盖：
%%%   * 纯文本消息 assets=[]（键存在且为空列表——不是缺键）；
%%%   * 附件消息字段完整（恰好五键白名单、值逐项核对、active-only、按 id 升序、
%%%     多消息互不串附件）；
%%%   * 跨租户结果集零泄漏（另一 Org 的资产绝不出现在本 Org 投影；跨 Workspace
%%%     读返回空页）；
%%%   * POST 回显（canonical 单事务）：发送后立即读回绑定资产；重放回显同形状；
%%%   * 查询数不随消息数线性增加（meck passthrough 计数：3 条 vs 12 条消息的
%%%     一页读，fetch_many 次数**相等**且恒为 2——1 次消息 + 1 次批量资产）；
%%%   * 语句形状静态门（双租户键 / 无 OFFSET / active-only / 不选 object_key）。
%%%
%%% 时间由 eb_system_clock 注入；合成租户由 eb_pg_test_fixture 随机 TSID 隔离。
-module(eb_message_assets_projection_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(FROZEN_ASSET_KEYS, [file_name, id, mime, size_bytes, status]).

assets_projection_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun plain_text_messages_project_empty_assets/0},
        {timeout, 90, fun attachment_message_projects_frozen_whitelist/0},
        {timeout, 90, fun cross_tenant_assets_never_leak/0},
        {timeout, 90, fun post_append_echoes_bound_assets/0},
        {timeout, 90, fun post_replay_echoes_same_assets/0},
        {timeout, 90, fun query_count_is_constant_per_page/0},
        {timeout, 60, fun frozen_sql_shape_gates/0}
    ];
cases(Other) ->
    erlang:error({csbe01_assets_projection_db_unavailable, Other}).

%% ===================================================================
%% 1. 纯文本：assets = []（键存在，不是缺键）
%% ===================================================================

plain_text_messages_project_empty_assets() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Ids = [insert_msg(Scope, <<"csbe01-plain-", N>>, due_at()) || N <- lists:seq(1, 3)],
        {ok, Rows} = list_messages(Org, Ws, Conv, #{limit => 50}),
        ?assertEqual(Ids, [maps:get(id, R) || R <- Rows]),
        lists:foreach(
            fun(Row) ->
                ?assertEqual([], maps:get(assets, Row, missing_assets_key))
            end,
            Rows
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 2. 附件消息：五键白名单 / 值逐项 / active-only / 升序 / 不串消息
%% ===================================================================

attachment_message_projects_frozen_whitelist() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        {ok, MsgA} = append_plain(Scope, <<"csbe01-att-a">>),
        {ok, MsgB} = append_plain(Scope, <<"csbe01-att-b">>),
        Base = ?FIX:id(),
        %% MsgA：两个 active（带 file_name）+ 两个 deleted（软删，必须被排除）
        A1 = Base,
        ADeleted1 = Base + 1,
        ADeleted2 = Base + 2,
        ANoName = Base + 3,
        ok = seed_bound_asset(Org, Ws, Conv, MsgA, A1, #{file_name => <<"发票.pdf"/utf8>>}),
        ok = seed_bound_asset(Org, Ws, Conv, MsgA, ADeleted1, #{
            file_name => <<"已删1.bin"/utf8>>, status => <<"deleted">>
        }),
        ok = seed_bound_asset(Org, Ws, Conv, MsgA, ADeleted2, #{
            file_name => <<"已删2.bin"/utf8>>, status => <<"deleted">>
        }),
        %% MsgB：一个 active 且**未声明** file_name（NULL 归一为 null 原子——
        %% jsone 对 null 出 JSON null；undefined 会被编成 "undefined" 字符串）
        ok = seed_bound_asset(Org, Ws, Conv, MsgB, ANoName, #{file_name => null}),
        {ok, Rows} = list_messages(Org, Ws, Conv, #{limit => 50}),
        ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
        RowA = maps:get(MsgA, ById),
        RowB = maps:get(MsgB, ById),
        %% MsgA 只剩 active（deleted 被排除），按 id 升序
        AssetsA = maps:get(assets, RowA, missing_assets_key),
        ?assertEqual([A1], [maps:get(id, A) || A <- AssetsA]),
        [OnlyA1] = AssetsA,
        ?assertEqual(?FROZEN_ASSET_KEYS, lists:sort(maps:keys(OnlyA1))),
        ?assertEqual(A1, maps:get(id, OnlyA1)),
        ?assertEqual(<<"image/png">>, maps:get(mime, OnlyA1)),
        ?assertEqual(2048, maps:get(size_bytes, OnlyA1)),
        ?assertEqual(<<"发票.pdf"/utf8>>, maps:get(file_name, OnlyA1)),
        ?assertEqual(active, maps:get(status, OnlyA1)),
        %% MsgB：未声明 file_name ⇒ null 原子（键仍在，值空）
        [OnlyB] = maps:get(assets, RowB, missing_assets_key),
        ?assertEqual(ANoName, maps:get(id, OnlyB)),
        ?assertEqual(null, maps:get(file_name, OnlyB)),
        %% 禁项三字段在整个投影里零出现（结构断言，不是字符串碰运气）
        lists:foreach(
            fun(Row) ->
                ?assertNot(leaks_forbidden_keys(maps:get(assets, Row, [])))
            end,
            Rows
        )
    after
        ?FIX:cleanup(Scope)
    end.

leaks_forbidden_keys(Assets) when is_list(Assets) ->
    lists:any(
        fun(Asset) ->
            Forbidden = [object_key, upload_url, token, upload_ref, object_hash],
            lists:any(fun(K) -> maps:is_key(K, Asset) end, Forbidden)
        end,
        Assets
    );
leaks_forbidden_keys(_Other) ->
    true.

%% ===================================================================
%% 3. 跨租户：另一 Org 的资产绝不出现；跨 Workspace 读返回空页
%% ===================================================================

cross_tenant_assets_never_leak() ->
    ScopeA = ?FIX:new_scope(),
    ScopeB = ?FIX:new_scope(),
    try
        {OrgA, WsA} = tenant(ScopeA),
        {OrgB, WsB} = tenant(ScopeB),
        ConvA = maps:get(conversation_id, ScopeA),
        ConvB = maps:get(conversation_id, ScopeB),
        {ok, MsgA} = append_plain(ScopeA, <<"csbe01-xa">>),
        {ok, MsgB} = append_plain(ScopeB, <<"csbe01-xb">>),
        AssetA = ?FIX:id(),
        AssetB = ?FIX:id(),
        ok = seed_bound_asset(OrgA, WsA, ConvA, MsgA, AssetA, #{file_name => <<"A.txt">>}),
        ok = seed_bound_asset(OrgB, WsB, ConvB, MsgB, AssetB, #{file_name => <<"B.txt">>}),
        %% A 的历史只含 A 的资产（id 级隔离，不是只比条数）
        {ok, RowsA} = list_messages(OrgA, WsA, ConvA, #{limit => 50}),
        AllA = lists:append([maps:get(assets, R, []) || R <- RowsA]),
        ?assertEqual([AssetA], [maps:get(id, A) || A <- AllA]),
        ?assertNot(lists:member(AssetB, [maps:get(id, A) || A <- AllA])),
        %% 跨 Workspace 读（B 的 Ws 查 A 的 conv）：空页（键集语义既有行为不变）
        {ok, Cross} = list_messages(OrgA, WsB, ConvA, #{limit => 50}),
        ?assertEqual([], Cross),
        %% B 侧对称复核
        {ok, RowsB} = list_messages(OrgB, WsB, ConvB, #{limit => 50}),
        AllB = lists:append([maps:get(assets, R, []) || R <- RowsB]),
        ?assertEqual([AssetB], [maps:get(id, A) || A <- AllB])
    after
        ?FIX:cleanup(ScopeB),
        ?FIX:cleanup(ScopeA)
    end.

%% ===================================================================
%% 4/5. POST 回显：canonical 单事务读回绑定资产；重放同形状
%% ===================================================================

post_append_echoes_bound_assets() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Base = ?FIX:id(),
        U1 = Base,
        U2 = Base + 1,
        %% 预置 active+unbound（contact 上传：uploaded_by_user_id NULL）
        ok = seed_unbound_asset(Org, Ws, Conv, U1, <<"截图.png"/utf8>>),
        ok = seed_unbound_asset(Org, Ws, Conv, U2, <<"日志.txt"/utf8>>),
        {ok, Result} = append_with_assets(Scope, <<"csbe01-echo-1">>, [U1, U2]),
        ?assertEqual(false, maps:get(replayed, Result)),
        Message = maps:get(message, Result),
        Assets = maps:get(assets, Message, missing_assets_key),
        %% 绑定即回显：升序、五键白名单、file_name 原样
        ?assertEqual([U1, U2], [maps:get(id, A) || A <- Assets]),
        lists:foreach(
            fun(Asset) -> ?assertEqual(?FROZEN_ASSET_KEYS, lists:sort(maps:keys(Asset))) end,
            Assets
        ),
        ?assertEqual(<<"截图.png"/utf8>>, maps:get(file_name, hd(Assets))),
        ?assertEqual(active, maps:get(status, hd(Assets))),
        ?assertNot(leaks_forbidden_keys(Assets)),
        %% 回显与历史读同一形状（同一 read 源）
        {ok, Rows} = list_messages(Org, Ws, Conv, #{limit => 50}),
        Row = hd([R || R <- Rows, maps:get(id, R) =:= maps:get(message_id, Result)]),
        ?assertEqual(Assets, maps:get(assets, Row))
    after
        ?FIX:cleanup(Scope)
    end.

post_replay_echoes_same_assets() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        U1 = ?FIX:id(),
        ok = seed_unbound_asset(Org, Ws, Conv, U1, <<"重放.doc"/utf8>>),
        Params = #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"csbe01-replay-1">>,
            sender_type => contact,
            contact_id => maps:get(contact_id, Scope),
            asset_ids => [U1],
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        },
        {ok, First} = eb_message_app:append_message(Org, Params),
        {ok, Replay} = eb_message_app:append_message(Org, Params),
        ?assertEqual(true, maps:get(replayed, Replay)),
        ?assertEqual(
            maps:get(assets, maps:get(message, First)),
            maps:get(assets, maps:get(message, Replay))
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 6. 查询数不随消息数线性增加（meck passthrough 计数）
%% ===================================================================

query_count_is_constant_per_page() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        [insert_msg(Scope, <<"csbe01-small-", N>>, due_at()) || N <- lists:seq(1, 3)],
        [insert_msg(Scope, <<"csbe01-big-", N>>, due_at()) || N <- lists:seq(1, 9)],
        Big = 3 + 9,
        ?assertEqual(Big, count_messages(Org, Ws, Conv)),
        meck:new(eb_pg_exec, [passthrough]),
        meck:expect(eb_pg_exec, fetch_many, fun(Sql, Params, Fields) ->
            meck:passthrough([Sql, Params, Fields])
        end),
        try
            Before1 = meck:num_calls(eb_pg_exec, fetch_many, '_'),
            {ok, Page1} = list_messages(Org, Ws, Conv, #{limit => 3}),
            Delta1 = meck:num_calls(eb_pg_exec, fetch_many, '_') - Before1,
            {ok, Page2} = list_messages(Org, Ws, Conv, #{limit => Big}),
            Delta2 = meck:num_calls(eb_pg_exec, fetch_many, '_') - Before1 - Delta1,
            %% 页大小不同（3 vs 12 条、含/不含附件行混布）⇒ 查询次数**相等**：
            %% 1 次消息键集查询 + 1 次资产批量 IN 查询，与页内消息数无关。
            ?assertEqual(Delta1, Delta2),
            ?assertEqual(2, Delta1),
            ?assertEqual(3, length(Page1)),
            ?assertEqual(Big, length(Page2))
        after
            meck:unload(eb_pg_exec)
        end
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 7. 语句形状静态门（双租户键 / 无 OFFSET / active-only / 不选 object_key）
%% ===================================================================

frozen_sql_shape_gates() ->
    Statements = eb_pg_message_ext:sql_statements(),
    AssetSqls = [
        Sql
     || Sql <- Statements, binary:match(Sql, <<"FROM enterprise_asset">>) =/= nomatch
    ],
    ?assertEqual(1, length(AssetSqls)),
    [AssetSql] = AssetSqls,
    lists:foreach(
        fun(Sql) ->
            Upper = string:uppercase(binary_to_list(Sql)),
            ?assertEqual(nomatch, string:find(Upper, "OFFSET")),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"organization_id">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"workspace_id">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"$1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"$2">>))
        end,
        Statements
    ),
    %% 资产投影语句：active-only、批量 IN、绝不选 object_key
    ?assertNotEqual(nomatch, binary:match(AssetSql, <<"status = 'active'">>)),
    ?assertNotEqual(nomatch, binary:match(AssetSql, <<"ANY($3::bigint[])">>)),
    ?assertEqual(nomatch, binary:match(AssetSql, <<"object_key">>)),
    %% canonical 事务内单消息读同款门
    TxSql = eb_pg_store_sql:sql(fetch_assets_by_message),
    ?assertNotEqual(nomatch, binary:match(TxSql, <<"status = 'active'">>)),
    ?assertEqual(nomatch, binary:match(TxSql, <<"object_key">>)),
    ?assertNotEqual(nomatch, binary:match(TxSql, <<"message_id = $3">>)),
    %% 归一化规格：冻结五键 + 分组键 message_id（装配后剥离）
    Fields = eb_pg_message_ext:asset_projection_fields(),
    ?assertEqual(
        [id, message_id, mime, size_bytes, file_name, status],
        [K || {K, _, _} <- Fields]
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

now_secs() ->
    eb_system_clock:now().

due_at() ->
    now_secs() + 86400.

list_messages(Org, Ws, ConversationId, Extra) ->
    eb_message_app:list_messages(
        Org, maps:merge(#{workspace_id => Ws, conversation_id => ConversationId}, Extra)
    ).

count_messages(Org, Ws, ConversationId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND conversation_id=$3"
        >>,
        [Org, Ws, ConversationId]
    ).

%% 纯文本消息（canonical 事务真链，consent/policy 齐备的 fixture scope）。
append_plain(Scope, ClientMsgId) ->
    Org = maps:get(org_id, Scope),
    {ok, #{message_id := MsgId}} = eb_message_app:append_message(Org, #{
        workspace_id => maps:get(workspace_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<"csbe01-plain-body">>,
        sender_type => contact,
        contact_id => maps:get(contact_id, Scope),
        key_ref => ?FIX:key_ref(1),
        accepted_at => now_secs()
    }),
    {ok, MsgId}.

%% 附件消息（空正文 + asset_ids，canonical 绑定）。
append_with_assets(Scope, ClientMsgId, AssetIds) ->
    eb_message_app:append_message(maps:get(org_id, Scope), #{
        workspace_id => maps:get(workspace_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<>>,
        sender_type => contact,
        contact_id => maps:get(contact_id, Scope),
        asset_ids => AssetIds,
        key_ref => ?FIX:key_ref(1),
        accepted_at => now_secs()
    }).

%% 直接走 store 的 append_message/3 造消息（与 canonical 事务共用 sender 合同；
%% 与 eb06 套件同款）。返回 MsgId。
insert_msg(Scope, ClientMsgId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    MsgId = ?FIX:id(),
    Aad = #{
        organization_id => Org, workspace_id => Ws, conversation_id => Conv, message_id => MsgId
    },
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"csbe01-body">>, ?FIX:key_ref(1)),
    {ok, _Row} = eb_pg_store:append_message(Org, Ws, #{
        id => MsgId,
        conversation_id => Conv,
        client_msg_id => ClientMsgId,
        sender_type => <<"contact">>,
        sender_contact_id => maps:get(contact_id, Scope),
        sender_business_identity_id => null,
        actor_user_id => null,
        body_cipher => maps:get(cipher, Sealed),
        key_version => maps:get(key_version, Sealed),
        aad_hash => maps:get(aad_hash, Sealed),
        content_hash => binary:encode_hex(crypto:hash(sha256, maps:get(cipher, Sealed))),
        policy_id => maps:get(policy_id, Scope),
        policy_version => 1,
        retention_days => 1095,
        retain_until => RetainUntil
    }),
    MsgId.

%% 已绑定资产行（直插 SQL；触发器要求 message_id 非空 ⇒ retain_until 非空
%% 且 >= 消息的 retain_until——取消息 retain_until 本值）。
seed_bound_asset(Org, Ws, Conv, MsgId, AssetId, Opts) ->
    RetainMs = message_retain_ms(Org, Ws, MsgId),
    seed_asset_row(Org, Ws, Conv, MsgId, AssetId, RetainMs, Opts).

%% 未绑定（active+unbound，contact 上传：uploaded_by_user_id NULL；retain 可空）。
seed_unbound_asset(Org, Ws, Conv, AssetId, FileName) ->
    seed_asset_row(Org, Ws, Conv, null, AssetId, null, #{file_name => FileName}).

seed_asset_row(Org, Ws, Conv, MsgId, AssetId, RetainMs, Opts) ->
    Status = maps:get(status, Opts, <<"active">>),
    FileName = maps:get(file_name, Opts, null),
    ObjectKey = iolist_to_binary([
        "enterprise/",
        integer_to_binary(Org),
        "/",
        integer_to_binary(Ws),
        "/",
        integer_to_binary(AssetId)
    ]),
    ?FIX:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id, organization_id, workspace_id, conversation_id, message_id,"
            "  uploaded_by_user_id, object_key, object_hash, mime, size_bytes, status,"
            "  file_name, retain_until, version)"
            " VALUES ($1,$2,$3,$4,$5,NULL,$6,$7,$8,$9,$10,$11,"
            "         CASE WHEN $12::bigint IS NULL THEN NULL"
            "              ELSE to_timestamp($12::bigint/1000) END,"
            "         1)"
        >>,
        [
            AssetId,
            Org,
            Ws,
            Conv,
            MsgId,
            ObjectKey,
            eb_asset_content:sha256_hex(<<"csbe01-fixture">>),
            <<"image/png">>,
            2048,
            Status,
            FileName,
            RetainMs
        ]
    ).

message_retain_ms(Org, Ws, MsgId) ->
    Seconds = ?FIX:scalar(
        <<
            "SELECT (extract(epoch from retain_until) * 1000)::bigint"
            "  FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ),
    is_integer(Seconds) andalso Seconds > 0 orelse erlang:error({bad_message_retain, Seconds}),
    Seconds.
