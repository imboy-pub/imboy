%%% @doc friend_agg 关系状态机 eunit 测试（零 mock，T3.3）。
%%% 验证 申请/通过/拒绝/拉黑/解除拉黑/删除 六类转换的不变量与领域事件，
%%% 状态机：none -> pending -> friends；任意 -> blocked -> none。
-module(friend_agg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FROM, <<"100">>).
-define(TO, <<"200">>).

fresh() -> friend_agg:new(?FROM, ?TO).

%% ---- 访问器 / 初始态 ----

new_starts_none_test() ->
    F = fresh(),
    ?assertEqual(none, friend_agg:status(F)),
    ?assertEqual(?FROM, friend_agg:from(F)),
    ?assertEqual(?TO, friend_agg:to(F)).

%% ---- request（申请）----

request_from_none_test() ->
    {ok, F1, Evts} = friend_agg:request(fresh()),
    ?assertEqual(pending, friend_agg:status(F1)),
    ?assertEqual([{friend_requested, ?FROM, ?TO}], Evts).

request_when_pending_rejected_test() ->
    {ok, F1, _} = friend_agg:request(fresh()),
    ?assertEqual({error, already_requested}, friend_agg:request(F1)).

request_when_friends_rejected_test() ->
    {ok, F1, _} = friend_agg:request(fresh()),
    {ok, F2, _} = friend_agg:accept(F1),
    ?assertEqual({error, already_friends}, friend_agg:request(F2)).

request_when_blocked_rejected_test() ->
    {ok, F1, _} = friend_agg:block(fresh()),
    ?assertEqual({error, blocked}, friend_agg:request(F1)).

%% ---- accept（通过）----

accept_pending_becomes_friends_test() ->
    {ok, F1, _} = friend_agg:request(fresh()),
    {ok, F2, Evts} = friend_agg:accept(F1),
    ?assertEqual(friends, friend_agg:status(F2)),
    ?assertEqual([{friend_accepted, ?FROM, ?TO}], Evts).

accept_without_pending_rejected_test() ->
    ?assertEqual({error, no_pending_request}, friend_agg:accept(fresh())).

%% ---- reject（拒绝申请）----

reject_pending_back_to_none_test() ->
    {ok, F1, _} = friend_agg:request(fresh()),
    {ok, F2, Evts} = friend_agg:reject(F1),
    ?assertEqual(none, friend_agg:status(F2)),
    ?assertEqual([{friend_rejected, ?FROM, ?TO}], Evts).

reject_without_pending_rejected_test() ->
    ?assertEqual({error, no_pending_request}, friend_agg:reject(fresh())).

%% ---- block（拉黑）----

block_from_none_test() ->
    {ok, F1, Evts} = friend_agg:block(fresh()),
    ?assertEqual(blocked, friend_agg:status(F1)),
    ?assertEqual([{friend_blocked, ?FROM, ?TO}], Evts).

block_friends_test() ->
    {ok, F1, _} = friend_agg:request(fresh()),
    {ok, F2, _} = friend_agg:accept(F1),
    {ok, F3, Evts} = friend_agg:block(F2),
    ?assertEqual(blocked, friend_agg:status(F3)),
    ?assertEqual([{friend_blocked, ?FROM, ?TO}], Evts).

block_idempotent_no_event_test() ->
    {ok, F1, _} = friend_agg:block(fresh()),
    {ok, F2, Evts} = friend_agg:block(F1),
    ?assertEqual(blocked, friend_agg:status(F2)),
    ?assertEqual([], Evts).

%% ---- unblock（解除拉黑）----

unblock_back_to_none_test() ->
    {ok, F1, _} = friend_agg:block(fresh()),
    {ok, F2, Evts} = friend_agg:unblock(F1),
    ?assertEqual(none, friend_agg:status(F2)),
    ?assertEqual([{friend_unblocked, ?FROM, ?TO}], Evts).

unblock_when_not_blocked_rejected_test() ->
    ?assertEqual({error, not_blocked}, friend_agg:unblock(fresh())).

%% ---- remove（删除好友）----

remove_friends_back_to_none_test() ->
    {ok, F1, _} = friend_agg:request(fresh()),
    {ok, F2, _} = friend_agg:accept(F1),
    {ok, F3, Evts} = friend_agg:remove(F2),
    ?assertEqual(none, friend_agg:status(F3)),
    ?assertEqual([{friend_removed, ?FROM, ?TO}], Evts).

remove_when_not_friends_rejected_test() ->
    ?assertEqual({error, not_friends}, friend_agg:remove(fresh())).

%% ---- rehydrate（持久化重建）----

rehydrate_status_1_is_friends_test() ->
    F = friend_agg:rehydrate(#{
        <<"from_user_id">> => ?FROM,
        <<"to_user_id">> => ?TO,
        <<"status">> => 1
    }),
    ?assertEqual(friends, friend_agg:status(F)).

rehydrate_unknown_status_is_none_test() ->
    F = friend_agg:rehydrate(#{
        <<"from_user_id">> => ?FROM,
        <<"to_user_id">> => ?TO,
        <<"status">> => 0
    }),
    ?assertEqual(none, friend_agg:status(F)).
