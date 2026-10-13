%% 测试 fixture 仅回收自身创建的组件，完整 app 的命名组件由原 owner 保留。
-module(cache_fixture_helper).
-export([
    acquire/2, release/1, invalidate/1, cleanup/1, restore_env/1, setup/2, mock/2, on_failure/1
]).
acquire(Name, Start) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> {Pid, borrowed};
        undefined ->
            case Start() of
                {ok, Pid} ->
                    unlink(Pid),
                    track(component, {Pid, owned}),
                    {Pid, owned};
                {error, {already_started, Pid}} ->
                    {Pid, borrowed}
            end
    end.
release({_Pid, borrowed}) ->
    ok;
release({Pid, owned}) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.
invalidate(Keys) ->
    [ok = imboy_cache:invalidate(Key) || Key <- Keys],
    ok.

%% 失败 setup 未返回 fixture 时也必须回收已取得的自身资源并恢复环境。
setup(Saved, Start) ->
    put(cache_fixture_setup, []),
    try
        Start()
    catch
        Class:Reason:Stack ->
            Acquired = erase(cache_fixture_setup),
            Actions =
                [fun() -> erlang:raise(Class, Reason, Stack) end] ++
                    [
                        case Item of
                            {component, C} -> fun() -> release(C) end;
                            {mock, M} -> fun() -> meck:unload(M) end;
                            {callback, F} -> F
                        end
                     || Item <- Acquired
                    ],
            try
                cleanup(Actions)
            after
                restore_env(Saved)
            end
    after
        erase(cache_fixture_setup)
    end.
on_failure(Fun) -> track(callback, Fun).
mock(Module, Options) ->
    Result = meck:new(Module, Options),
    track(mock, Module),
    Result.
track(Kind, Item) ->
    case get(cache_fixture_setup) of
        undefined ->
            ok;
        Items ->
            put(cache_fixture_setup, [{Kind, Item} | Items]),
            ok
    end.
%% 每个清理动作均执行，原首个错误仍向上传播；不能以吞异常伪装清理成功。
cleanup(Actions) -> cleanup(Actions, none).
cleanup([], none) ->
    ok;
cleanup([], {Class, Reason, Stack}) ->
    erlang:raise(Class, Reason, Stack);
cleanup([Action | Rest], First) ->
    Next =
        try
            Action(),
            First
        catch
            Class:Reason:Stack ->
                case First of
                    none -> {Class, Reason, Stack};
                    _ -> First
                end
        end,
    cleanup(Rest, Next).
restore_env(Saved) ->
    [
        case Value of
            undefined -> application:unset_env(imboy, Key);
            {ok, V} -> application:set_env(imboy, Key, V)
        end
     || {Key, Value} <- Saved
    ],
    ok.

-include_lib("eunit/include/eunit.hrl").
cleanup_failure_restores_env_test() ->
    Saved = saved_env(),
    try
        application:set_env(imboy, dsync_enabled, cleanup_changed),
        ?assertError(
            invalidate_failure,
            try
                cleanup([
                    fun() -> error(invalidate_failure) end,
                    fun() ->
                        put(cleanup_release_attempted, true),
                        error(release_failure)
                    end
                ])
            after
                restore_env(Saved)
            end
        ),
        ?assertEqual(Saved, saved_env()),
        ?assertEqual(true, get(cleanup_release_attempted))
    after
        restore_env(Saved),
        erase(cleanup_release_attempted)
    end.
setup_failure_preserves_borrowed_component_test() ->
    Saved = saved_env(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    true = register(cache_fixture_helper_test_actor, Pid),
    try
        ?assertError(
            setup_failure,
            setup(Saved, fun() ->
                application:set_env(imboy, dsync_enabled, setup_changed),
                {Pid, borrowed} = acquire(
                    cache_fixture_helper_test_actor,
                    fun() -> error(borrowed_must_not_restart) end
                ),
                error(setup_failure)
            end)
        ),
        ?assertEqual(Saved, saved_env()),
        ?assert(is_process_alive(Pid))
    after
        Pid ! stop,
        restore_env(Saved)
    end.
saved_env() ->
    [
        {K, application:get_env(imboy, K)}
     || K <-
            [dsync_enabled, cluster_nodes, cache_fenced_namespaces]
    ].
