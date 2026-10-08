-module(ai_agent_runtime_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc ai_agent_runtime EUnit 测试（Phase 1 T1.3）
%%% 覆盖：启动即把启用中 agent 注册进 syn 在线态；DB 出错时优雅跳过不崩。
%%%
%%% 结构约束：meck_proc 按模块名全局注册，EUnit 的多个 test_ generator
%%% 是并发执行的——两个 setup 同时 meck:new 同一模块必撞 already_started
%%%（run11/12 实测）。故两用例合并进单个 foreach：fixture 内串行，每条
%%% 目独立 setup/cleanup。受控实例要求重新走 init，而 app 常驻后
%%% ai_agent_runtime 是 imboy_sup 的 transient child——setup 先优雅停掉
%%% 常驻实例（transient 不会被 sup 自动重启），cleanup 负责还回去。
%%%===================================================================

runtime_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"启用中 agent 全部注册在线", fun registers_active_agents/0},
        {"DB 出错时不注册且进程存活", fun survives_db_error/0}
    ]}.

setup() ->
    Takeover = takeover_runtime(),
    safe_meck(ai_agent_repo),
    safe_meck(imboy_syn),
    safe_meck(elib_log),
    Takeover.

cleanup(Takeover) ->
    %% 兜底停掉用例内自建的受控实例（正常路径用例 after 段已停）
    case whereis(ai_agent_runtime) of
        Pid when is_pid(Pid) -> catch gen_server:stop(Pid, normal, 5000);
        undefined -> ok
    end,
    catch meck:unload(ai_agent_repo),
    catch meck:unload(imboy_syn),
    catch meck:unload(elib_log),
    restore_runtime(Takeover).

registers_active_agents() ->
    meck:expect(ai_agent_repo, active_ids, 0, {ok, [101, 202]}),
    meck:expect(imboy_syn, join, 4, ok),
    {ok, Pid} = ai_agent_runtime:start_link(),
    try
        %% online_ids 是 call，会在 init 的 refresh(info 先入队)之后返回
        Ids = lists:sort(ai_agent_runtime:online_ids()),
        ?assertEqual([101, 202], Ids),
        ?assert(
            meck:called(imboy_syn, join, [101, <<"ai">>, Pid, <<"ai_runtime">>])
        ),
        ?assert(
            meck:called(imboy_syn, join, [202, <<"ai">>, Pid, <<"ai_runtime">>])
        )
    after
        catch gen_server:stop(Pid)
    end,
    ok.

survives_db_error() ->
    meck:expect(ai_agent_repo, active_ids, 0, {error, db_down}),
    meck:expect(imboy_syn, join, 4, ok),
    meck:expect(elib_log, internal_log, 5, ok),
    {ok, Pid} = ai_agent_runtime:start_link(),
    try
        ?assertEqual([], ai_agent_runtime:online_ids()),
        ?assertNot(meck:called(imboy_syn, join, ['_', '_', Pid, '_']))
    after
        catch gen_server:stop(Pid)
    end,
    ok.

%% @doc 测试目标 = init 注册行为，须重新走 init；sup child 为 transient，
%% 优雅停掉后不会被自动重启，cleanup 负责把常驻实例还回去。
takeover_runtime() ->
    case whereis(ai_agent_runtime) of
        undefined ->
            undefined;
        OldPid ->
            gen_server:stop(OldPid, normal, 5000),
            OldPid
    end.

restore_runtime(undefined) ->
    %% 没有被接管的实例时，不创建跨用例存活的后台进程。
    ok;
restore_runtime(_OldPid) ->
    case whereis(ai_agent_runtime) of
        undefined ->
            %% 等待恢复完成，下一次 setup 先停实例再安装 mock。
            Parent = self(),
            Ref = make_ref(),
            spawn(fun() ->
                Result =
                    try ai_agent_runtime:start_link() of
                        {ok, Pid} ->
                            unlink(Pid),
                            ok;
                        Other ->
                            Other
                    catch
                        Class:Reason -> {Class, Reason}
                    end,
                Parent ! {Ref, Result}
            end),
            receive
                {Ref, ok} -> ok;
                {Ref, Error} -> error({runtime_restore_failed, Error})
            after 5000 -> error(runtime_restore_timeout)
            end;
        _ ->
            ok
    end.

%% @doc 兄弟套件/先前崩溃遗留的 meck_proc 未卸载时，先卸载再重新 meck。
%% 注意 meck_proc:start/2 对已注册模块是内部 badmatch raise（不是
%% {error, _} 返回值），case 接不住，必须 try/catch（run11 实测）。
safe_meck(Mod) ->
    try meck:new(Mod, [passthrough, non_strict]) of
        ok -> ok
    catch
        _:_ ->
            catch meck:unload(Mod),
            ok = meck:new(Mod, [passthrough, non_strict])
    end.
