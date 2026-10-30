%%% W6-01 D 组 bind 面（W3-03：cs-bind 8 + enterprise-bind 4 合同叶；本
%%% driver 遍 4+2=6 叶）。
%%%
%%% cs-bind：cs_session_app 真实会话入口（open_session/append_session_message
%%% ——widget/contact 消息唯一写入路径，收敛 enterprise canonical append）——
%%% 同 client_msg_id 重放单绑定零新增；他 org / 跨 workspace / 他人上传一律
%%% 拒绝且零存储细节披露。enterprise-bind：eb_message_app canonical append
%%% business_identity 重放单绑定；任一资产墓碑 => 单事务整体回滚（消息与全部
%%% 绑定）。
%%%
%%% DB 定位：直连 V5_INTTEST_W404_DB 指向的 attv5_<slot>_w601 库（gate 已
%%% 建库+迁移，本套件不重复 provision）；种子 enterprise_asset 行的 storage
%%% 四元组按 V5_PROBE_DRIVER 从 run-owned registry 解析，逐叶以 frozen_location
%%% 证明身份真实（identity_resolved / cross_driver_rejected），并经 sanctioned
%%% driver 真实 PUT/GET 探针对象（probe_sha256 / probe_readback_ok）。借出
%%% 连接（eunit_setup_with_db）在成功与失败路径均归还（eunit_cleanup_db）。
%%% 每单元 eb_pg_test_fixture:cleanup（本 scope TSID 过滤），双跑幂等。
-module(attachment_lifecycle_v5_w601_w3_bind_tests).

-export([specs/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SUP, attachment_lifecycle_v5_w601_w3_support).
-define(FIX, eb_pg_test_fixture).
-define(C_CS, <<"W3-03/cs-bind">>).
-define(C_ENT, <<"W3-03/enterprise-bind">>).
%% 与 ?SUP:seed_identity 的解析常量同值（Erlang 宏不跨模块，bind 本地重复
%% 声明）：registry namespace/container 必须与 support 的 ?NS_*/?CONTAINER_
%% DISK 一致，否则 seed 四元组解析 {error, unknown} → 各叶首步 badmatch。
-define(NS_DISK, <<"w601-w3-disk">>).
-define(NS_GARAGE, <<"w601-w3-garage">>).
-define(CONTAINER_DISK, <<"w601c">>).

w601_w3_bind_test_() ->
    Driver = ?SUP:driver(),
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs(Driver)]
    end}.

%% ------------------------------------------------------------------ setup

setup() ->
    Db = bind_db(),
    ok = ensure_loaded(imboy),
    PriorPgConf = application:get_env(imboy, pg_conf),
    ok = override_pg_database(Db),
    PriorRegistry = application:get_env(imboy, attachment_storage_registry),
    PriorGarage = application:get_env(imboy, garage),
    %% 借出连接（eunit_setup_with_db）：成功与失败路径均归还（P2 eunit_cleanup_db）。
    Conn =
        case eunit_runner:eunit_setup_with_db() of
            {ok, C} ->
                C;
            {error, DbReason} ->
                restore(undefined, undefined, PriorPgConf),
                erlang:error({no_go, w601_w3_bind_db_unavailable, DbReason})
        end,
    try
        Garage = garage_env(),
        ok = application:set_env(imboy, garage, Garage),
        Base = disk_base(),
        ok = application:set_env(
            imboy, attachment_storage_registry, registry(Base, Garage)
        ),
        case attachment_storage:validate_registry() of
            ok -> ok;
            Other -> erlang:error({w601_w3_registry_not_executable, Other})
        end,
        ?assertEqual(Db, current_database()),
        #{
            db => Db,
            conn => Conn,
            base => Base,
            prior_pg_conf => PriorPgConf,
            prior_registry => PriorRegistry,
            prior_garage => PriorGarage
        }
    catch
        Class:Reason:Stack ->
            eunit_runner:eunit_cleanup_db(Conn),
            restore(PriorRegistry, PriorGarage, PriorPgConf),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{
    conn := Conn, base := Base, prior_pg_conf := PPC, prior_registry := PR, prior_garage := PG
}) ->
    restore(PR, PG, PPC),
    _ = file:del_dir_r(Base),
    eunit_runner:eunit_cleanup_db(Conn),
    ok.

restore(PR, PG, PPC) ->
    case PR of
        {ok, V} -> ok = application:set_env(imboy, attachment_storage_registry, V);
        undefined -> ok = application:unset_env(imboy, attachment_storage_registry)
    end,
    case PG of
        {ok, G} -> ok = application:set_env(imboy, garage, G);
        undefined -> ok = application:unset_env(imboy, garage)
    end,
    case PPC of
        {ok, C} -> ok = application:set_env(imboy, pg_conf, C);
        undefined -> ok
    end,
    ok.

ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok
    end.

%% attv5_<slot>_w601（gate 已建库）；V5_RUN_DIR 在场时校验 slot 归属，缺席
%% （普通干跑）直接拒绝——bind 面无独立 marker 供给，fail-closed。
bind_db() ->
    case os:getenv("V5_INTTEST_W404_DB") of
        Name when is_list(Name), Name =/= "" ->
            case {os:getenv("V5_RUN_DIR"), os:getenv("V5_SLOT")} of
                {RunDir, Slot} when
                    is_list(RunDir), RunDir =/= "", is_list(Slot), Slot =/= ""
                ->
                    case re:run(Name, "^attv5_" ++ Slot ++ "_[a-z0-9_]+$", [{capture, none}]) of
                        match -> list_to_binary(Name);
                        nomatch -> erlang:error({no_go, w601_w3_bind_db_unbound, Name})
                    end;
                _ ->
                    erlang:error({no_go, w601_w3_bind_slot_unverified})
            end;
        _ ->
            erlang:error({no_go, w601_w3_bind_db_missing})
    end.

override_pg_database(Db) ->
    case application:get_env(imboy, pg_conf) of
        {ok, #{start_mfa := {epgsql, connect, [Opts]}} = Pg0} ->
            Pg = Pg0#{start_mfa := {epgsql, connect, [Opts#{database := binary_to_list(Db)}]}},
            ok = application:set_env(imboy, pg_conf, Pg),
            ok;
        Other ->
            erlang:error({no_go, w601_w3_pg_conf_shape, Other})
    end.

current_database() ->
    {ok, [#{<<"db">> := Db}]} = elib_pg:query(<<"SELECT current_database() AS db">>, []),
    Db.

garage_env() ->
    Keys = [
        "W202_GARAGE_ENDPOINT",
        "W202_GARAGE_BUCKET",
        "W202_GARAGE_ACCESS_KEY",
        "W202_GARAGE_SECRET_KEY"
    ],
    Values = [os:getenv(K) || K <- Keys],
    case lists:all(fun(V) -> is_list(V) andalso V =/= "" end, Values) of
        false ->
            erlang:error({no_go, garage_env_missing, lists:zip(Keys, Values)});
        true ->
            [EP, Bucket, AK, SK] = [list_to_binary(V) || V <- Values],
            #{endpoint => EP, bucket => Bucket, access_key => AK, secret_key => SK}
    end.

disk_base() ->
    Parent =
        case os:getenv("V5_DISK_ROOT") of
            false -> temp_parent();
            "" -> temp_parent();
            Dir -> Dir
        end,
    Base = filename:join(
        Parent,
        "w601-w3-bind-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = filelib:ensure_dir(filename:join(Base, "placeholder")),
    ok = file:change_mode(Base, 8#700),
    Base.

temp_parent() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.

registry(Base, Garage) ->
    %% N3：namespace/container 与 ?SUP:seed_identity 的解析（?NS_* /
    %% ?CONTAINER_DISK 同值宏）对齐——attachment_storage:resolve/1 按
    %% namespace 精确匹配，registry 自立名称会使 seed 四元组解析失败
    %%（{error, unknown} → badmatch，6 叶全崩）。registry 根键名保留 bind
    %% 标记，endpoint 仍指向 bind 自有目录；bind 与 race/hold 各用独立步级
    %% 库，同 namespace 不同 root 不构成跨套件冲突。
    %% 生产 attachment_storage:registry/0 只认 version=1 信封（缺 version
    %% 一律替换为空 registry → validate_registry 报 empty_roots），按
    %% w601_w4_context:registry/2 同款形状包装（套件无裸读 roots 路径）。
    #{
        version => 1,
        roots => #{
            <<"w601-w3-bind-disk-root">> => #{
                driver => <<"disk">>,
                namespace => ?NS_DISK,
                aliases => [],
                endpoint => list_to_binary("file://" ++ Base),
                container => ?CONTAINER_DISK
            },
            <<"w601-w3-bind-garage-root">> => #{
                driver => <<"garage">>,
                namespace => ?NS_GARAGE,
                aliases => [],
                endpoint => maps:get(endpoint, Garage),
                container => maps:get(bucket, Garage)
            }
        },
        upload_namespaces => #{}
    }.

%% ------------------------------------------------------------ leaf matrix

%% Spec = {Case, Scenario, Driver, Domain, AcId, Idx}
specs(Driver) ->
    CS = [
        {?C_CS, Sc, Driver, <<"enterprise">>, <<"AC04">>, Idx}
     || {Idx, Sc} <- indexed([
            <<"same_message_replay">>,
            <<"wrong_org">>,
            <<"wrong_workspace">>,
            <<"wrong_owner">>
        ])
    ],
    ENT = [
        {?C_ENT, Sc, Driver, <<"enterprise">>, <<"AC04">>, 4 + Idx}
     || {Idx, Sc} <- indexed([<<"same_message_replay">>, <<"one_invalid_asset_all_rollback">>])
    ],
    CS ++ ENT.

leaf_unit(S, {Case, Sc, Driver, Dom, Ac, _Idx} = Spec) ->
    Entry = leaf_entry(Case),
    Desc = binary_to_list(
        iolist_to_binary([short_case(Case), "/", Driver, "/", Sc, "/", Dom])
    ),
    {Desc,
        {timeout, 240, fun() ->
            try
                Measured = scenario(S, Spec),
                ?SUP:emit({Case, Entry, Driver, Sc, Dom, Ac}, Measured)
            after
                ?SUP:probe_cleanup_after()
            end
        end}}.

short_case(Case) ->
    [_, Short] = binary:split(Case, <<"/">>),
    Short.

leaf_entry(?C_CS) -> <<"cs-bind">>;
leaf_entry(?C_ENT) -> <<"enterprise-bind">>.

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_CS, <<"same_message_replay">>, Driver, _, _, _}) ->
    cs_replay(S, Driver);
scenario(S, {?C_CS, <<"wrong_org">>, Driver, _, _, _}) ->
    cs_wrong_org(S, Driver);
scenario(S, {?C_CS, <<"wrong_workspace">>, Driver, _, _, _}) ->
    cs_wrong_workspace(S, Driver);
scenario(S, {?C_CS, <<"wrong_owner">>, Driver, _, _, _}) ->
    cs_wrong_owner(S, Driver);
scenario(S, {?C_ENT, <<"same_message_replay">>, Driver, _, _, _}) ->
    ent_replay(S, Driver);
scenario(S, {?C_ENT, <<"one_invalid_asset_all_rollback">>, Driver, _, _, _}) ->
    ent_rollback(S, Driver).

%% cs 域真实会话入口（cs_session_app:open_session —— widget/contact 会话
%% 真源，queued session 持久化经 cs_pg_store）。
open_cs_session(Scope, Opts) ->
    Org = maps:get(org_id, Scope),
    cs_session_app:open_session(Org, #{
        workspace_id => maps:get(workspace_id, Opts, maps:get(workspace_id, Scope)),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Opts, maps:get(conversation_id, Scope)),
        at => now_secs()
    }).

%% cs 域 contact 消息 append 参数（sender_shape 经 session contact 裁决）。
cs_append_params(Session, Scope, ClientMsgId, AssetIds) ->
    #{
        workspace_id => maps:get(workspace_id, Scope),
        session_id => maps:get(id, Session),
        contact_id => maps:get(contact_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<>>,
        asset_ids => AssetIds,
        key_ref => ?FIX:key_ref(1),
        accepted_at => now_secs()
    }.

%% cs-bind same_message_replay：cs_session_app 真实入口（widget 会话 →
%% enterprise canonical append）——同 client_msg_id 重放单绑定零新增；回显
%% 同形状。
cs_replay(S, Driver) ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        U1 = ?FIX:id(),
        Key = object_key(Org, Ws, U1),
        Quad = ?SUP:seed_identity(S, Driver, Key),
        ok = seed_unbound_asset(S, Org, Ws, Conv, U1, <<"重放.doc"/utf8>>, null, Quad),
        Probe = ?SUP:probe_io(Key, Quad),
        {ok, Session} = open_cs_session(Scope, #{}),
        Params = cs_append_params(Session, Scope, <<"w601w3-cs-replay-1">>, [U1]),
        InitialVersion = asset_version(Org, Ws, U1),
        {ok, First} = cs_session_app:append_session_message(Org, Params),
        BoundVersion = asset_version(Org, Ws, U1),
        ?assertEqual(InitialVersion + 1, BoundVersion),
        {ok, Replay} = cs_session_app:append_session_message(Org, Params),
        ?assertEqual(BoundVersion, asset_version(Org, Ws, U1)),
        ?assertEqual(false, maps:get(replayed, First)),
        ?assertEqual(true, maps:get(replayed, Replay)),
        ?assertEqual(maps:get(message_id, First), maps:get(message_id, Replay)),
        ?assertEqual(1, message_count(Org, Ws, <<"w601w3-cs-replay-1">>)),
        ?assertEqual(0, unbound_count(Org, Ws, U1)),
        ?assertEqual(1, bound_count(Org, Ws, U1, maps:get(message_id, First))),
        Proof = ?SUP:identity_proof(Key, Quad),
        maps:merge(
            maps:merge(
                #{
                    entry_point_call => <<"cs_session_app:append_session_message/2">>,
                    initial_version => InitialVersion,
                    bound_version => BoundVersion,
                    first_replayed => false,
                    replay_replayed => true,
                    message_id_equal => true,
                    message_count => 1,
                    unbound_count => 0,
                    bound_count => 1
                },
                Probe
            ),
            Proof
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% cs-bind wrong_org：他 org 资产绑定被拒且零存储细节披露（no asset storage
%% details —— 返回 not_found，外部资产 unbound、version 不变）。
cs_wrong_org(S, Driver) ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Foreign = ?FIX:id(),
        ForeignKey = object_key(OtherOrg, OtherWs, Foreign),
        Quad = ?SUP:seed_identity(S, Driver, ForeignKey),
        ok = seed_asset_row(
            S,
            OtherOrg,
            OtherWs,
            null,
            null,
            Foreign,
            null,
            #{file_name => <<"他org.png"/utf8>>},
            Quad
        ),
        Probe = ?SUP:probe_io(ForeignKey, Quad),
        ForeignVersion = asset_version(OtherOrg, OtherWs, Foreign),
        {ok, Session} = open_cs_session(Scope, #{}),
        {error, not_found} = cs_session_app:append_session_message(
            Org, cs_append_params(Session, Scope, <<"w601w3-xorg-1">>, [Foreign])
        ),
        ?assertEqual(0, message_count(Org, Ws, <<"w601w3-xorg-1">>)),
        ?assertEqual(1, unbound_count(OtherOrg, OtherWs, Foreign)),
        ?assertEqual(ForeignVersion, asset_version(OtherOrg, OtherWs, Foreign)),
        Proof = ?SUP:identity_proof(ForeignKey, Quad),
        maps:merge(
            maps:merge(
                #{
                    entry_point_call => <<"cs_session_app:append_session_message/2">>,
                    foreign_asset_id => Foreign,
                    foreign_message_count => 0,
                    foreign_unbound_count => 1,
                    foreign_version_unchanged => true,
                    foreign_version => ForeignVersion
                },
                Probe
            ),
            Proof
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% cs-bind wrong_workspace：请求 workspace 与 org 不一致——cs_session_app
%% 真实入口的 tenant 归属门在触库前拒绝（open_session 本身被拒），零消息、
%% 外部资产 unbound。
cs_wrong_workspace(S, Driver) ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Foreign = ?FIX:id(),
        ForeignKey = object_key(OtherOrg, OtherWs, Foreign),
        Quad = ?SUP:seed_identity(S, Driver, ForeignKey),
        ok = seed_asset_row(
            S,
            OtherOrg,
            OtherWs,
            null,
            null,
            Foreign,
            null,
            #{file_name => <<"跨ws.png"/utf8>>},
            Quad
        ),
        Probe = ?SUP:probe_io(ForeignKey, Quad),
        ForeignVersion = asset_version(OtherOrg, OtherWs, Foreign),
        {error, _} = open_cs_session(Scope, #{workspace_id => OtherWs}),
        ?assertEqual(0, message_count(Org, OtherWs, <<"w601w3-xws-1">>)),
        ?assertEqual(1, unbound_count(OtherOrg, OtherWs, Foreign)),
        ?assertEqual(ForeignVersion, asset_version(OtherOrg, OtherWs, Foreign)),
        Proof = ?SUP:identity_proof(ForeignKey, Quad),
        maps:merge(
            maps:merge(
                #{
                    entry_point_call => <<"cs_session_app:open_session/2">>,
                    foreign_asset_id => Foreign,
                    cross_ws_message_count => 0,
                    foreign_unbound_count => 1,
                    foreign_version_unchanged => true,
                    foreign_version => ForeignVersion
                },
                Probe
            ),
            Proof
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% cs-bind wrong_owner：member 消息只可绑本人上传——他人上传资产拒绝，
%% 错误形状与「不存在」不区分（零存储细节披露）。
cs_wrong_owner(S, Driver) ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        AssetId = ?FIX:id(),
        Key = object_key(Org, Ws, AssetId),
        Quad = ?SUP:seed_identity(S, Driver, Key),
        ok = seed_unbound_asset(S, Org, Ws, Conv, AssetId, <<"member-owned.png">>, Peer, Quad),
        Probe = ?SUP:probe_io(Key, Quad),
        Version = asset_version(Org, Ws, AssetId),
        {ok, Session} = open_cs_session(Scope, #{}),
        {error, not_found} = cs_session_app:append_session_message(
            Org, cs_append_params(Session, Scope, <<"w601w3-contact-owner-1">>, [AssetId])
        ),
        ?assertEqual(0, message_count(Org, Ws, <<"w601w3-contact-owner-1">>)),
        ?assertEqual(1, unbound_count(Org, Ws, AssetId)),
        ?assertEqual(Version, asset_version(Org, Ws, AssetId)),
        Proof = ?SUP:identity_proof(Key, Quad),
        maps:merge(
            maps:merge(
                #{
                    entry_point_call => <<"cs_session_app:append_session_message/2">>,
                    asset_id => AssetId,
                    uploader => <<"peer">>,
                    message_count => 0,
                    unbound_count => 1,
                    version_unchanged => true,
                    version => Version
                },
                Probe
            ),
            Proof
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% enterprise-bind same_message_replay：business_identity 形态同 client_msg_id
%% 与 asset_ids 重放 => 单绑定。
ent_replay(S, Driver) ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        U1 = ?FIX:id(),
        Key = object_key(Org, Ws, U1),
        Quad = ?SUP:seed_identity(S, Driver, Key),
        ok = seed_unbound_asset(S, Org, Ws, Conv, U1, <<"enterprise-replay.txt">>, Actor, Quad),
        Probe = ?SUP:probe_io(Key, Quad),
        Params = #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"w601w3-enterprise-replay-1">>,
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
        ?assertEqual(1, message_count(Org, Ws, <<"w601w3-enterprise-replay-1">>)),
        ?assertEqual(0, unbound_count(Org, Ws, U1)),
        ?assertEqual(1, bound_count(Org, Ws, U1, maps:get(message_id, First))),
        Proof = maps:merge(?SUP:identity_proof(Key, Quad), Probe),
        maps:merge(
            #{
                entry_point_call => <<"eb_message_app:append_message/2">>,
                initial_version => InitialVersion,
                bound_version => BoundVersion,
                first_replayed => false,
                replay_replayed => true,
                message_id_equal => true,
                message_count => 1,
                unbound_count => 0,
                bound_count => 1,
                sender_type => <<"business_identity">>
            },
            Proof
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% enterprise-bind one_invalid_asset_all_rollback：U2 墓碑（删除意图先提交
%% => 不再允许新增引用）=> canonical 单事务整体回滚：消息不存在、两个资产
%% 均未绑定、version 均不变；墓碑单独也不可绑，好资产可正常绑定。
ent_rollback(S, Driver) ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Base = ?FIX:id(),
        U1 = Base,
        U2 = Base + 1,
        K1 = object_key(Org, Ws, U1),
        K2 = object_key(Org, Ws, U2),
        Quad1 = ?SUP:seed_identity(S, Driver, K1),
        Quad2 = ?SUP:seed_identity(S, Driver, K2),
        ok = seed_unbound_asset(S, Org, Ws, Conv, U1, <<"好资产.png"/utf8>>, Actor, Quad1),
        ok = seed_unbound_asset(S, Org, Ws, Conv, U2, <<"墓碑资产.png"/utf8>>, Actor, Quad2),
        Probe1 = ?SUP:probe_io(K1, Quad1),
        Probe2 = ?SUP:probe_io(K2, Quad2),
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
        {error, _} = append_enterprise(Scope, <<"w601w3-rollback-1">>, [U1, U2]),
        ?assertEqual(
            false,
            ?FIX:scalar(
                <<
                    "SELECT EXISTS (SELECT 1 FROM enterprise_message"
                    " WHERE organization_id=$1 AND workspace_id=$2"
                    " AND conversation_id=$3 AND client_msg_id=$4)"
                >>,
                [Org, Ws, Conv, <<"w601w3-rollback-1">>]
            )
        ),
        ?assertEqual(1, unbound_count(Org, Ws, U1)),
        ?assertEqual(1, unbound_count(Org, Ws, U2)),
        ?assertEqual(Version1, asset_version(Org, Ws, U1)),
        ?assertEqual(Version2, asset_version(Org, Ws, U2)),
        %% 墓碑资产单独也不可绑；好资产可正常绑定（资格只挡墓碑）
        {error, _} = append_enterprise(Scope, <<"w601w3-rollback-2">>, [U2]),
        {ok, Valid} = append_enterprise(Scope, <<"w601w3-rollback-3">>, [U1]),
        ?assertEqual(1, bound_count(Org, Ws, U1, maps:get(message_id, Valid))),
        ?assertEqual(Version1 + 1, asset_version(Org, Ws, U1)),
        ?assertEqual(1, unbound_count(Org, Ws, U2)),
        ?assertEqual(Version2, asset_version(Org, Ws, U2)),
        Proof1 = ?SUP:identity_proof(K1, Quad1),
        Proof2 = ?SUP:identity_proof(K2, Quad2),
        maps:merge(
            maps:merge(
                #{
                    entry_point_call => <<"eb_message_app:append_message/2">>,
                    tombstone_asset_id => U2,
                    message_absent => true,
                    unbound_count_u1 => 1,
                    unbound_count_u2 => 1,
                    version_u1_unchanged => true,
                    version_u2_unchanged => true,
                    tombstone_solo_rejected => true,
                    valid_asset_bind_ok => true,
                    bound_count_after_valid => 1,
                    version_u1_after => Version1 + 1
                },
                maps:merge(Proof1, Proof2)
            ),
            maps:merge(Probe1, Probe2)
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ---------------------------------------------------------------- 种子 / 断言

object_key(Org, Ws, AssetId) ->
    iolist_to_binary([
        "w601w3/",
        integer_to_binary(Org),
        "/",
        integer_to_binary(Ws),
        "/",
        integer_to_binary(AssetId)
    ]).

%% 未绑定 active 资产（storage 四元组按 driver 从 run-owned registry 解析；
%% w401 seed_eb_asset 四元组化先例）。
seed_unbound_asset(S, Org, Ws, Conv, AssetId, FileName, Uploader, Quad) ->
    seed_asset_row(
        S,
        Org,
        Ws,
        Conv,
        null,
        AssetId,
        null,
        #{file_name => FileName, uploader => Uploader},
        Quad
    ).

seed_asset_row(
    _S, Org, Ws, Conv, MsgId, AssetId, RetainMs, Opts, Quad
) ->
    FileName = maps:get(file_name, Opts, null),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id, organization_id, workspace_id, conversation_id, message_id,"
            "  uploaded_by_user_id, object_key, object_hash, mime, size_bytes, status,"
            "  file_name, retain_until, version,"
            "  storage_driver, storage_namespace, storage_container, storage_locator_sha256)"
            " VALUES ($1,$2,$3,$4,$5,$12,$6,$7,$8,$9,'active',$10,"
            "         CASE WHEN $11::bigint IS NULL THEN NULL"
            "              ELSE to_timestamp($11::bigint/1000) END,"
            "         1,$13,$14,$15,$16)"
            " ON CONFLICT (id) DO NOTHING"
        >>,
        [
            AssetId,
            Org,
            Ws,
            Conv,
            MsgId,
            object_key(Org, Ws, AssetId),
            eb_asset_content:sha256_hex(<<"w601w3-fixture">>),
            <<"image/png">>,
            2048,
            FileName,
            RetainMs,
            maps:get(uploader, Opts, null),
            maps:get(<<"storage_driver">>, Quad),
            maps:get(<<"storage_namespace">>, Quad),
            maps:get(<<"storage_container">>, Quad),
            maps:get(<<"storage_locator_sha256">>, Quad)
        ]
    ),
    ok.

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

now_secs() -> eb_system_clock:now().

append_enterprise(Scope, ClientMsgId, AssetIds) ->
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

message_count(Org, Ws, ClientMsgId) ->
    ?FIX:scalar(
        <<
            "SELECT COUNT(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND client_msg_id=$3"
        >>,
        [Org, Ws, ClientMsgId]
    ).

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

indexed(List) ->
    lists:zip(lists:seq(1, length(List)), List).
