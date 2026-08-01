-module(red_packet_scope_tests).

%%%===================================================================
%%% @doc B-11：红包会话作用域 —— 非该群成员领取返回「无权操作」。
%%%
%%% ⚠️ 判据前提原本不成立：red_packet 表此前**没有任何群/会话绑定字段**，
%%%    客户端 send 也只传 amount/count/type/greeting，服务端无从判定。
%%%    迁移 00000056 补了 scope_type/scope_id 后判据才可实现。
%%%    未绑定作用域的红包（旧数据/旧客户端）沿用旧行为放行 —— 这条越权面在
%%%    客户端全量升级前依然存在，是待办不是已完成。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

-define(SENDER, 5001).
-define(MEMBER, 5002).
-define(OUTSIDER, 5003).
-define(GID, 700).

setup() ->
    meck:new(red_packet_repo, [no_link, passthrough]),
    meck:new(group_member_ds, [no_link, passthrough]),
    meck:new(wallet_ds, [no_link, passthrough]),
    meck:expect(wallet_ds, ensure_wallet, fun(_Uid) -> #{<<"id">> => 1} end),
    meck:expect(red_packet_repo, grab, fun(_Id, _Uid) -> {ok, 100} end),
    meck:expect(group_member_ds, is_member, fun
        (?GID, ?MEMBER) -> true;
        (?GID, ?SENDER) -> true;
        (_, _) -> false
    end),
    ok.

cleanup(_) ->
    catch meck:unload(wallet_ds),
    catch meck:unload(group_member_ds),
    catch meck:unload(red_packet_repo),
    ok.

stub_packet(Packet) ->
    meck:expect(red_packet_repo, find_by_id, fun(_Id) -> Packet end).

group_packet() ->
    #{
        <<"sender_uid">> => ?SENDER,
        <<"scope_type">> => <<"C2G">>,
        <<"scope_id">> => ?GID
    }.

open_scope_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun outsider_cannot_open_group_packet/0,
        fun member_can_open_group_packet/0,
        fun unbound_packet_keeps_legacy_behavior/0,
        fun c2c_packet_only_for_two_parties/0
    ]}.

%% B-11 判据本体：非该群成员领取 → 无权操作，且**不得**走到 grab（不能占名额）
outsider_cannot_open_group_packet() ->
    stub_packet(group_packet()),
    ?assertEqual(
        {error, <<"无权操作：您不是该群成员"/utf8>>},
        red_packet_logic:open(1, ?OUTSIDER)
    ),
    ?assertEqual(0, meck:num_calls(red_packet_repo, grab, '_')).

member_can_open_group_packet() ->
    stub_packet(group_packet()),
    ?assertEqual({ok, 100}, red_packet_logic:open(1, ?MEMBER)),
    ?assertEqual(1, meck:num_calls(red_packet_repo, grab, '_')).

%% 旧数据（scope 为 NULL）必须仍能领 —— 否则上线当天所有在途红包全部领不了
unbound_packet_keeps_legacy_behavior() ->
    stub_packet(#{<<"sender_uid">> => ?SENDER, <<"scope_type">> => null}),
    ?assertEqual({ok, 100}, red_packet_logic:open(1, ?OUTSIDER)).

%% 单聊红包只有收发双方可领
c2c_packet_only_for_two_parties() ->
    stub_packet(#{
        <<"sender_uid">> => ?SENDER,
        <<"scope_type">> => <<"C2C">>,
        <<"scope_id">> => ?MEMBER
    }),
    ?assertEqual({ok, 100}, red_packet_logic:open(1, ?MEMBER)),
    ?assertEqual({ok, 100}, red_packet_logic:open(1, ?SENDER)),
    ?assertMatch({error, _}, red_packet_logic:open(1, ?OUTSIDER)).

%% ===================================================================
%% 发送侧
%% ===================================================================

send_scope_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun sender_must_be_group_member/0,
        fun require_scope_flag_rejects_unbound/0
    ]}.

%% 发送者自己不在群里 → 拒绝（否则等于往一个自己都不在的群塞钱）
sender_must_be_group_member() ->
    ?assertEqual(
        {error, <<"无权操作：您不是该群成员"/utf8>>},
        red_packet_logic:send(
            ?OUTSIDER, <<"fixed">>, 200, 1, <<"hi">>, #{
                scope_type => <<"C2G">>, scope_id => ?GID
            }
        )
    ).

%% 开关打开后，不带作用域的请求（旧客户端）被拒 —— 客户端全量升级前不能打开
require_scope_flag_rejects_unbound() ->
    meck:new(config_ds, [no_link, passthrough]),
    try
        meck:expect(config_ds, env, fun
            (red_packet_require_scope, _) -> true;
            (K, D) -> meck:passthrough([K, D])
        end),
        ?assertMatch(
            {error, _},
            red_packet_logic:send(?SENDER, <<"fixed">>, 200, 1, <<"hi">>, #{})
        )
    after
        catch meck:unload(config_ds)
    end.
