-module(msg_send_rate_limit_tests).

%%%===================================================================
%%% @doc 消息级限流接进 C2C/C2G 发送主路径的接线测试。
%%%
%%% 直接把某 uid 真实打到自动禁言态，再走 c2c/3 与 c2g/3 入口，断言被限流
%%% 拒绝且不崩：C2C 返回 {reply, rate_limited}，C2G 投 C2G_ERROR code=429。
%%% 禁言分支在 friend/group 校验之前返回，无需 mock 下游 DB。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    msg_rate_logic:init_table(),
    ok.

cleanup(_) ->
    catch ets:delete_all_objects(msg_rate_counter),
    catch ets:delete_all_objects(msg_rate_muted),
    ok.

%% 把 Uid 打到自动禁言态（超过 60 条/分钟阈值）
mute(Uid) ->
    lists:foreach(
        fun(_) -> msg_rate_logic:check_and_record(Uid) end,
        lists:seq(1, 61)
    ),
    true = msg_rate_logic:is_muted(Uid),
    ok.

%% C2C：被限流的发送者 → {reply, rate_limited}，不触达下游好友校验
c2c_rate_limited_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        Uid = 810001,
        ok = mute(Uid),
        Result = msg_c2c_logic:c2c(<<"m1">>, Uid, #{<<"to">> => <<"9">>}),
        [
            ?_assertMatch({reply, _}, Result),
            ?_assertEqual(<<"rate_limited">>, action_of(Result))
        ]
    end}.

%% C2G：被限流的发送者 → 投 C2G_ERROR code=429，入口返回 ok（不崩连接）
c2g_rate_limited_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        Uid = 820001,
        ok = mute(Uid),
        Result = msg_c2g_logic:c2g(<<"m1">>, Uid, #{<<"to">> => <<"9">>}),
        %% c2g 用 self() ! {reply, _} 投递错误，入口返回 ok
        Reply =
            receive
                {reply, R} -> R
            after 1000 -> timeout
            end,
        [
            ?_assertEqual(ok, Result),
            ?_assertEqual(<<"C2G_ERROR">>, maps:get(<<"type">>, Reply, undefined)),
            ?_assertEqual(429, maps:get(<<"code">>, Reply, undefined))
        ]
    end}.

%% 从 {reply, Msg} 取顶层 action（S2C v2.0 形态）
action_of({reply, Msg}) when is_map(Msg) ->
    maps:get(<<"action">>, Msg, undefined);
action_of(_) ->
    undefined.
