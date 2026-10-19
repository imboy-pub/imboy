%%% Test-only listener isolation; real Cowboy/Ranch, no application or database boot.
-module(cs_test_support_tests).
-include_lib("eunit/include/eunit.hrl").

loopback_callback_and_cleanup_test() ->
    with_cowboy(fun() ->
        Ref = make_ref(),
        Key = {?MODULE, Ref},
        try
            CallbackResult = cs_test_support:with_listener(
                tenant, session_claim, #{}, fun(Port) ->
                    [Name] = [
                        N
                     || {N, Info} <- maps:to_list(ranch:info()),
                        maps:get(port, Info) =:= Port
                    ],
                    put(Key, Name),
                    ?assertEqual({{127, 0, 0, 1}, Port}, ranch:get_addr(Name)),
                    ?assert(is_integer(Port) andalso Port > 0),
                    self() ! {Ref, callback},
                    callback_result
                end
            ),
            ?assertEqual(callback_result, CallbackResult),
            receive
                {Ref, callback} -> ok
            after 0 -> error(callback_not_called)
            end,
            receive
                {Ref, callback} -> error(callback_called_twice)
            after 0 -> ok
            end,
            Name = get(Key),
            ?assert(is_atom(Name) andalso Name =/= undefined),
            ?assertNot(maps:is_key(Name, ranch:info()))
        after
            stop_captured(Key)
        end
    end).

wrong_reported_address_rethrows_and_cleans_up_test() ->
    with_cowboy(fun() ->
        Ref = make_ref(),
        Key = {?MODULE, Ref},
        Parent = self(),
        ok = meck:new(ranch, [passthrough, no_link]),
        try
            meck:expect(ranch, get_addr, fun(Name) ->
                %% The actual socket must already be loopback before fault injection.
                {{127, 0, 0, 1}, Port} = meck:passthrough([Name]),
                put(Key, Name),
                Parent ! {Ref, actual_loopback, Name, Port},
                {{0, 0, 0, 0}, Port}
            end),
            Outcome =
                try
                    cs_test_support:with_listener(
                        tenant, session_claim, #{}, fun(_) ->
                            Parent ! {Ref, callback},
                            callback_result
                        end
                    )
                of
                    Value -> {returned, Value}
                catch
                    Class:Reason -> {Class, Reason}
                end,
            receive
                {Ref, actual_loopback, Name, Port} ->
                    ?assertEqual({error, {badmatch, {{0, 0, 0, 0}, Port}}}, Outcome),
                    ?assertNot(maps:is_key(Name, ranch:info()))
            after 0 ->
                error(real_loopback_not_observed)
            end,
            receive
                {Ref, callback} -> error(callback_must_not_run)
            after 0 -> ok
            end
        after
            stop_captured(Key),
            meck:unload(ranch)
        end
    end).

stop_captured(Key) ->
    case erase(Key) of
        undefined -> ok;
        Name -> cs_test_support:stop(Name)
    end.

with_cowboy(Fun) ->
    ?assertEqual(undefined, whereis(pgsql)),
    ?assertEqual(undefined, whereis(pooler_sup)),
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())),
    {ok, Started} = application:ensure_all_started(cowboy),
    try
        Fun()
    after
        lists:foreach(fun application:stop/1, lists:reverse(Started))
    end.
