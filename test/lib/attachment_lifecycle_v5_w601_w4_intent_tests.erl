%%% @doc W6-01 E 组：W4-01/intent-rollback（48 叶 = 12 入口 × 4 故障，全
%%% garage driver），schema_phase=integrated。
%%%
%%% 场景逻辑整体复用已封印的 W4-03 执行器 attachment_lifecycle_v5_w403_
%%% predecessor_intent:run/3（oracle 与源 case 逐字一致：意图失败绝不触达
%%% 存储、入口不误报成功、探针字节零变化、零队列入行）。driver 边界由
%%% w403_predecessor_support 的 additive garage 分支承载：探针对象真实写入
%%% W202 Garage 桶，失败注入后 HEAD+GET 证明字节未变、真实列表快照相等、
%%% marker DB 回读证明未入队。
-module(attachment_lifecycle_v5_w601_w4_intent_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").

-define(C, <<"W4-01/intent-rollback">>).
-define(X, attachment_lifecycle_v5_w601_w4_context).
-define(S, attachment_lifecycle_v5_w403_predecessor_support).

w601_w4_intent_test_() ->
    {setup, fun() -> ?X:setup(<<"V5_INTTEST_W601W4INT">>) end, fun ?X:cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> scenario(St, S) end) || S <- specs()]
    end}.

specs() ->
    [
        {?C, Entry, <<"garage">>, Fault, Domain}
     || {Entry, Domain} <- entrypoints(),
        Fault <- [
            <<"sql_rollback">>,
            <<"intent_insert_error">>,
            <<"audit_insert_error">>,
            <<"commit_response_lost">>
        ]
    ].

entrypoints() ->
    [
        {<<"reject-size">>, <<"ordinary">>},
        {<<"reject-mime">>, <<"ordinary">>},
        {<<"reject-teaching">>, <<"ordinary">>},
        {<<"orphan-cleanup">>, <<"ordinary">>},
        {<<"pending-cleanup">>, <<"ordinary">>},
        {<<"account-deletion">>, <<"ordinary">>},
        {<<"teaching-unbound">>, <<"ordinary">>},
        {<<"app-confirm-reject">>, <<"ordinary">>},
        {<<"app-retention-purge">>, <<"ordinary">>},
        {<<"eb-private-delete">>, <<"enterprise">>},
        {<<"eb-pending-cleanup">>, <<"enterprise">>},
        {<<"eb-purge-drain">>, <<"enterprise">>}
    ].

%% 逐叶：support driver 上下文切 garage（仅本叶测试进程），intent 执行器的
%% 探针写入/清单/完好断言全部落在真实 Garage；断言全过后由 ?X:leaf emit。
scenario(St, {_C, Entry, _Driver, Fault, _Domain}) ->
    #{conn := Conn, base := Base} = St,
    ?S:set_driver(garage),
    try
        Measured =
            attachment_lifecycle_v5_w403_predecessor_intent:run(
                Entry, binary_to_atom(Fault, utf8), #{conn => Conn, root => Base}
            ),
        Measured#{probe_storage => <<"real-garage-bucket">>}
    after
        ?S:reset_driver()
    end.
