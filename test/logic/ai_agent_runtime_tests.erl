-module(ai_agent_runtime_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc ai_agent_runtime EUnit 测试（Phase 1 T1.3）
%%% 覆盖：启动即把启用中 agent 注册进 syn 在线态；DB 出错时优雅跳过不崩。
%%%===================================================================

runtime_registers_active_agents_in_syn_test_() ->
    {setup,
        fun() ->
            safe_meck(ai_agent_repo),
            safe_meck(imboy_syn),
            meck:expect(ai_agent_repo, active_ids, 0, {ok, [101, 202]}),
            meck:expect(imboy_syn, join, 4, ok),
            {ok, Pid} = ai_agent_runtime:start_link(),
            Pid
        end,
        fun(Pid) ->
            gen_server:stop(Pid),
            meck:unload(imboy_syn),
            meck:unload(ai_agent_repo)
        end,
        fun(_Pid) ->
            [
                {"启用中 agent 全部注册在线",
                    ?_test(begin
                        %% online_ids 是 call，会在 init 的 refresh(info 先入队)之后返回
                        Ids = lists:sort(ai_agent_runtime:online_ids()),
                        ?assertEqual([101, 202], Ids),
                        ?assert(
                            meck:called(imboy_syn, join, [101, <<"ai">>, '_', <<"ai_runtime">>])
                        ),
                        ?assert(
                            meck:called(imboy_syn, join, [202, <<"ai">>, '_', <<"ai_runtime">>])
                        )
                    end)}
            ]
        end}.

runtime_survives_db_error_test_() ->
    {setup,
        fun() ->
            safe_meck(ai_agent_repo),
            safe_meck(imboy_syn),
            safe_meck(elib_log),
            meck:expect(ai_agent_repo, active_ids, 0, {error, db_down}),
            meck:expect(imboy_syn, join, 4, ok),
            meck:expect(elib_log, internal_log, 5, ok),
            {ok, Pid} = ai_agent_runtime:start_link(),
            Pid
        end,
        fun(Pid) ->
            gen_server:stop(Pid),
            meck:unload(elib_log),
            meck:unload(imboy_syn),
            meck:unload(ai_agent_repo)
        end,
        fun(_Pid) ->
            [
                {"DB 出错时不注册且进程存活",
                    ?_test(begin
                        ?assertEqual([], ai_agent_runtime:online_ids()),
                        ?assertNot(meck:called(imboy_syn, join, '_'))
                    end)}
            ]
        end}.

%% @doc 兄弟套件/先前崩溃遗留的 meck_proc 未卸载时（already_started），
%% 先卸载再重新 meck，避免 setup 必红（CI-00 run11）。
safe_meck(Mod) ->
    case meck:new(Mod, [passthrough, non_strict]) of
        ok ->
            ok;
        {error, {already_started, _}} ->
            catch meck:unload(Mod),
            meck:new(Mod, [passthrough, non_strict])
    end.
