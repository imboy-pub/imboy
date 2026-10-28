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
        {timeout, 90, fun enterprise_replay_echoes_same_assets/0},
        {timeout, 90, fun one_invalid_asset_all_rollback/0},
        {timeout, 90, fun second_asset_bind_failure_rolls_back_first/0},
        {timeout, 90, fun cross_org_and_workspace_bind_rejected/0},
        {timeout, 90, fun wrong_owner_bind_rejected/0},
        {timeout, 90, fun cs_wrong_owner_bind_rejected/0},
        {timeout, 60, fun query_count_is_constant_per_page/0},
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
        InitialVersion = asset_version(Org, Ws, U1),
        {ok, First} = eb_message_app:append_message(Org, Params),
        BoundVersion = asset_version(Org, Ws, U1),
        ?assertEqual(InitialVersion + 1, BoundVersion),
        {ok, Replay} = eb_message_app:append_message(Org, Params),
        ?assertEqual(BoundVersion, asset_version(Org, Ws, U1)),
        ?assertEqual(false, maps:get(replayed, First)),
        ?assertEqual(true, maps:get(replayed, Replay)),
        ?assertEqual(maps:get(message_id, First), maps:get(message_id, Replay)),
        ?assertEqual(1, message_count(Org, Ws, <<"csbe01-replay-1">>)),
        ?assertEqual(0, unbound_count(Org, Ws, U1)),
        ?assertEqual(1, bound_count(Org, Ws, U1, maps:get(message_id, First))),
        ?assertEqual([U1], [
            maps:get(id, Asset)
         || Asset <-
                maps:get(assets, maps:get(message, First))
        ]),
        ?assertEqual(
            maps:get(assets, maps:get(message, First)),
            maps:get(assets, maps:get(message, Replay))
        )
    after
        ?FIX:cleanup(Scope)
    end.

enterprise_replay_echoes_same_assets() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        U1 = ?FIX:id(),
        ok = seed_unbound_asset(
            Org,
            Ws,
            Conv,
            U1,
            <<"enterprise-replay.txt">>,
            #{uploader => Actor}
        ),
        Params = #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"csbe03-enterprise-replay-1">>,
            sender_type => business_identity,
            identity_id => maps:get(sales_identity_id, Scope),
            actor_user_id => Actor,
            asset_ids => [U1],
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        },
        InitialVersion = asset_version(Org, Ws, U1),
        {ok, First} = eb_message_app:append_message(Org, Params),
        BoundVersion = asset_version(Org, Ws, U1),
        ?assertEqual(InitialVersion + 1, BoundVersion),
        {ok, Replay} = eb_message_app:append_message(Org, Params),
        ?assertEqual(BoundVersion, asset_version(Org, Ws, U1)),
        ?assertEqual(false, maps:get(replayed, First)),
        ?assertEqual(true, maps:get(replayed, Replay)),
        ?assertEqual(maps:get(message_id, First), maps:get(message_id, Replay)),
        ?assertEqual(1, message_count(Org, Ws, <<"csbe03-enterprise-replay-1">>)),
        ?assertEqual(0, unbound_count(Org, Ws, U1)),
        ?assertEqual(1, bound_count(Org, Ws, U1, maps:get(message_id, First))),
        ?assertEqual([U1], [
            maps:get(id, Asset)
         || Asset <-
                maps:get(assets, maps:get(message, First))
        ]),
        ?assertEqual(
            maps:get(assets, maps:get(message, First)),
            maps:get(assets, maps:get(message, Replay))
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 5b. W3-03 绑定资格与全回滚（02 §4/§5：墓碑拒新增引用；无效资产全回滚；
%%     跨组织/跨 Workspace/非上传人拒绝——错误形状与「不存在」不区分）
%% ===================================================================

one_invalid_asset_all_rollback() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Base = ?FIX:id(),
        U1 = Base,
        U2 = Base + 1,
        Actor = maps:get(actor_user_id, Scope),
        ok = seed_unbound_asset(Org, Ws, Conv, U1, <<"好资产.png"/utf8>>, #{uploader => Actor}),
        ok = seed_unbound_asset(Org, Ws, Conv, U2, <<"墓碑资产.png"/utf8>>, #{uploader => Actor}),
        %% U2 已进删除意图：02 §5「删除意图先提交 → 不再允许新增引用」
        ok =
            ?FIX:exec(
                <<
                    "UPDATE enterprise_asset SET lifecycle_state='delete_intent'"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
                    " AND lifecycle_state='live'"
                >>,
                [Org, Ws, U2]
            ),
        Version1 = asset_version(Org, Ws, U1),
        Version2 = asset_version(Org, Ws, U2),
        {error, _} = append_enterprise_with_assets(Scope, <<"csbe03-rollback-1">>, [U1, U2]),
        %% canonical 单事务整体回滚：消息不存在、两个资产均未绑定
        ?assertEqual(
            false,
            ?FIX:scalar(
                <<
                    "SELECT EXISTS (SELECT 1 FROM enterprise_message"
                    " WHERE organization_id=$1 AND workspace_id=$2"
                    " AND conversation_id=$3 AND client_msg_id=$4)"
                >>,
                [Org, Ws, Conv, <<"csbe03-rollback-1">>]
            )
        ),
        ?assertEqual(1, unbound_count(Org, Ws, U1)),
        ?assertEqual(1, unbound_count(Org, Ws, U2)),
        ?assertEqual(Version1, asset_version(Org, Ws, U1)),
        ?assertEqual(Version2, asset_version(Org, Ws, U2)),
        %% 墓碑资产单独也不可绑；好资产可正常绑定（资格只挡墓碑）
        {error, _} = append_enterprise_with_assets(Scope, <<"csbe03-rollback-2">>, [U2]),
        {ok, Valid} = append_enterprise_with_assets(Scope, <<"csbe03-rollback-3">>, [U1]),
        ?assertEqual(1, bound_count(Org, Ws, U1, maps:get(message_id, Valid))),
        ?assertEqual(Version1 + 1, asset_version(Org, Ws, U1)),
        ?assertEqual(1, unbound_count(Org, Ws, U2)),
        ?assertEqual(Version2, asset_version(Org, Ws, U2))
    after
        ?FIX:cleanup(Scope)
    end.

second_asset_bind_failure_rolls_back_first() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        First = ?FIX:id(),
        Second = First + 1,
        Actor = maps:get(actor_user_id, Scope),
        ok = seed_unbound_asset(Org, Ws, Conv, First, <<"first.png">>, #{uploader => Actor}),
        ok = seed_unbound_asset(Org, Ws, Conv, Second, <<"second.png">>, #{uploader => Actor}),
        %% The second real UPDATE overflows after the first bind, without persistent DDL.
        ok = ?FIX:exec(
            <<
                "UPDATE enterprise_asset SET version=2147483647"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, Second]
        ),
        FirstVersion = asset_version(Org, Ws, First),
        SecondVersion = asset_version(Org, Ws, Second),
        ?assertEqual(2147483647, SecondVersion),
        ?assertMatch(
            {error, {sql, <<"22003">>, _}},
            append_enterprise_with_assets(Scope, <<"csbe03-mid-bind-fault">>, [First, Second])
        ),
        ?assertEqual(0, message_count(Org, Ws, <<"csbe03-mid-bind-fault">>)),
        ?assertEqual(1, unbound_count(Org, Ws, First)),
        ?assertEqual(1, unbound_count(Org, Ws, Second)),
        ?assertEqual(FirstVersion, asset_version(Org, Ws, First)),
        ?assertEqual(SecondVersion, asset_version(Org, Ws, Second))
    after
        ?FIX:cleanup(Scope)
    end.

cross_org_and_workspace_bind_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Foreign1 = ?FIX:id(),
        %% 他 org 资产（conversation 属其自身租户：FK 复合键 org/ws/conv）
        ok = seed_asset_row(
            OtherOrg,
            OtherWs,
            null,
            null,
            Foreign1,
            null,
            #{file_name => <<"他org.png"/utf8>>}
        ),
        ForeignVersion = asset_version(OtherOrg, OtherWs, Foreign1),
        {error, not_found} = append_with_assets(Scope, <<"csbe03-xorg-1">>, [Foreign1]),
        %% 零副作用：消息不存在、外部资产仍 unbound
        ?assertEqual(0, message_count(Org, Ws, <<"csbe03-xorg-1">>)),
        ?assertEqual(1, unbound_count(OtherOrg, OtherWs, Foreign1)),
        ?assertEqual(ForeignVersion, asset_version(OtherOrg, OtherWs, Foreign1)),
        %% wrong_workspace：请求 workspace 与 org 不一致（workspace 属另一 org）
        %% ——tenant 归属门在触库前拒绝
        {error, _} =
            eb_message_app:append_message(Org, #{
                workspace_id => OtherWs,
                conversation_id => Conv,
                client_msg_id => <<"csbe03-xws-1">>,
                body => <<>>,
                sender_type => contact,
                contact_id => maps:get(contact_id, Scope),
                asset_ids => [Foreign1],
                key_ref => ?FIX:key_ref(1),
                accepted_at => now_secs()
            }),
        ?assertEqual(0, message_count(Org, OtherWs, <<"csbe03-xws-1">>)),
        ?assertEqual(1, unbound_count(OtherOrg, OtherWs, Foreign1)),
        ?assertEqual(ForeignVersion, asset_version(OtherOrg, OtherWs, Foreign1))
    after
        ?FIX:cleanup(Scope)
    end.

message_count(Org, Ws, ClientMsgId) ->
    ?FIX:scalar(
        <<
            "SELECT COUNT(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND client_msg_id=$3"
        >>,
        [Org, Ws, ClientMsgId]
    ).

wrong_owner_bind_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        U1 = ?FIX:id(),
        %% 他人上传的资产（member 消息只可绑本人上传）
        ok = seed_unbound_asset(
            Org,
            maps:get(workspace_id, Scope),
            Conv,
            U1,
            <<"他人上传.png"/utf8>>,
            #{uploader => Peer}
        ),
        {error, _} =
            eb_message_app:append_message(Org, #{
                workspace_id => maps:get(workspace_id, Scope),
                conversation_id => Conv,
                client_msg_id => <<"csbe03-owner-1">>,
                body => <<>>,
                sender_type => business_identity,
                identity_id => maps:get(sales_identity_id, Scope),
                actor_user_id => Actor,
                asset_ids => [U1],
                key_ref => ?FIX:key_ref(1),
                accepted_at => now_secs()
            }),
        ?assertEqual(1, unbound_count(Org, maps:get(workspace_id, Scope), U1))
    after
        ?FIX:cleanup(Scope)
    end.

cs_wrong_owner_bind_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        AssetId = ?FIX:id(),
        ok = seed_unbound_asset(
            Org,
            Ws,
            Conv,
            AssetId,
            <<"member-owned.png">>,
            #{uploader => Peer}
        ),
        Version = asset_version(Org, Ws, AssetId),
        {error, not_found} = append_with_assets(Scope, <<"csbe03-contact-owner-1">>, [AssetId]),
        ?assertEqual(0, message_count(Org, Ws, <<"csbe03-contact-owner-1">>)),
        ?assertEqual(1, unbound_count(Org, Ws, AssetId)),
        ?assertEqual(Version, asset_version(Org, Ws, AssetId))
    after
        ?FIX:cleanup(Scope)
    end.

unbound_count(Org, Ws, AssetId) ->
    ?FIX:scalar(
        <<
            "SELECT COUNT(*) FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            " AND message_id IS NULL"
        >>,
        [Org, Ws, AssetId]
    ).

bound_count(Org, Ws, AssetId, MessageId) ->
    ?FIX:scalar(
        <<
            "SELECT COUNT(*) FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            " AND message_id=$4"
        >>,
        [Org, Ws, AssetId, MessageId]
    ).

asset_version(Org, Ws, AssetId) ->
    ?FIX:scalar(
        <<
            "SELECT version FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, AssetId]
    ).

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

append_enterprise_with_assets(Scope, ClientMsgId, AssetIds) ->
    eb_message_app:append_message(maps:get(org_id, Scope), #{
        workspace_id => maps:get(workspace_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<>>,
        sender_type => business_identity,
        identity_id => maps:get(sales_identity_id, Scope),
        actor_user_id => maps:get(actor_user_id, Scope),
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

%% 未绑定 + 指定上传人（W3-03 wrong_owner 负例：member 消息只绑本人上传）。
seed_unbound_asset(Org, Ws, Conv, AssetId, FileName, Opts) ->
    seed_asset_row(
        Org,
        Ws,
        Conv,
        null,
        AssetId,
        null,
        maps:merge(#{file_name => FileName}, Opts)
    ).

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
            " VALUES ($1,$2,$3,$4,$5,$13,$6,$7,$8,$9,$10,$11,"
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
            RetainMs,
            maps:get(uploader, Opts, null)
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
