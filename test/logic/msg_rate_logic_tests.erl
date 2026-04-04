-module(msg_rate_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%% @doc msg_rate_logic 模块测试

%% @doc 确保测试前初始化 ETS 表
setup() ->
    msg_rate_logic:init_table(),
    ok.

cleanup(_) ->
    %% 清空 ETS 表
    catch ets:delete_all_objects(msg_rate_counter),
    catch ets:delete_all_objects(msg_rate_muted),
    ok.

%% @doc 正常消息通过
normal_message_pass_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [?_assertEqual(ok, msg_rate_logic:check_and_record(1001))]
     end}.

%% @doc 未被禁言的用户
not_muted_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [?_assertEqual(false, msg_rate_logic:is_muted(2001))]
     end}.

%% @doc 超过警告阈值（30 条/分钟）
warning_threshold_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         Uid = 3001,
         %% 发送 30 条消息，不触发警告
         lists:foreach(fun(_) ->
             msg_rate_logic:check_and_record(Uid)
         end, lists:seq(1, 30)),
         %% 第 31 条应触发警告
         Result = msg_rate_logic:check_and_record(Uid),
         [?_assertMatch({warning, _}, Result)]
     end}.

%% @doc 超过禁言阈值（60 条/分钟）
mute_threshold_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         Uid = 4001,
         %% 发送 60 条消息
         lists:foreach(fun(_) ->
             msg_rate_logic:check_and_record(Uid)
         end, lists:seq(1, 60)),
         %% 第 61 条应触发禁言
         Result = msg_rate_logic:check_and_record(Uid),
         [?_assertEqual({error, muted}, Result),
          ?_assertEqual(true, msg_rate_logic:is_muted(Uid))]
     end}.

%% @doc 被禁言后继续发消息被拒绝
muted_user_rejected_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         Uid = 5001,
         %% 发送 61 条消息触发禁言
         lists:foreach(fun(_) ->
             msg_rate_logic:check_and_record(Uid)
         end, lists:seq(1, 61)),
         %% 后续消息应被拒绝
         Result = msg_rate_logic:check_and_record(Uid),
         [?_assertEqual({error, muted}, Result)]
     end}.

%% @doc 禁言时间到期自动解除
mute_expiry_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         Uid = 6001,
         %% 手动设置一个已过期的禁言记录
         ets:insert(msg_rate_muted, {Uid, erlang:system_time(millisecond) - 1000}),
         %% 禁言应已过期
         [?_assertEqual(false, msg_rate_logic:is_muted(Uid))]
     end}.

%% @doc 手动解除禁言
manual_unmute_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         Uid = 7001,
         %% 手动设置禁言（未来时间）
         ets:insert(msg_rate_muted, {Uid, erlang:system_time(millisecond) + 300000}),
         ?assertEqual(true, msg_rate_logic:is_muted(Uid)),
         %% 手动解除
         ok = msg_rate_logic:unmute(Uid),
         [?_assertEqual(false, msg_rate_logic:is_muted(Uid))]
     end}.

%% @doc unmute 不存在的用户不报错
unmute_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [?_assertEqual(ok, msg_rate_logic:unmute(99999))]
     end}.
