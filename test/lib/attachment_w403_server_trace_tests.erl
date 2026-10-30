-module(attachment_w403_server_trace_tests).
-include_lib("eunit/include/eunit.hrl").
-export([test_resolve/1]).

test_resolve(Identity) -> {ok, Identity}.

server_trace_is_request_scoped_and_redacts_credentials_test() ->
    Parent = self(),
    Worker = spawn(fun() ->
        receive
            go ->
                ?MODULE:test_resolve(identity(<<"real-key">>)),
                Parent ! {done, self()},
                receive
                    stop -> ok
                end
        end
    end),
    {ok, Trace} = attachment_w403_server_trace:start_for_test(
        Worker, [{?MODULE, test_resolve, 1}]
    ),
    %% Calls outside the routed request process must not enter its evidence.
    ?MODULE:test_resolve(identity(<<"forged-other-process">>)),
    Worker ! go,
    receive
        {done, Worker} -> ok
    after 1000 -> erlang:error(worker_timeout)
    end,
    {ok, [#{trust := diagnostic_untrusted, source := Source, identity := Actual}]} = attachment_w403_server_trace:collect(
        Trace
    ),
    ?assertEqual(<<"attachment_w403_server_trace_tests:test_resolve/1">>, Source),
    ?assertEqual(<<"real-key">>, maps:get(key, Actual)),
    ?assertNot(maps:is_key(garage_config, Actual)),
    Worker ! stop.

missing_identity_is_not_evidence_test() ->
    Parent = self(),
    Worker = spawn(fun() ->
        receive
            go ->
                ?MODULE:test_resolve(#{key => <<"incomplete">>}),
                Parent ! {done, self()}
        end
    end),
    {ok, Trace} = attachment_w403_server_trace:start_for_test(
        Worker, [{?MODULE, test_resolve, 1}]
    ),
    Worker ! go,
    receive
        {done, Worker} -> ok
    after 1000 -> erlang:error(worker_timeout)
    end,
    ?assertEqual({ok, []}, attachment_w403_server_trace:collect(Trace)).

identity(Key) ->
    #{
        driver => garage,
        namespace => <<"ns">>,
        container => <<"bucket">>,
        key => Key,
        locator_digest => <<"digest">>,
        garage_config => #{secret_key => <<"must-not-be-emitted">>}
    }.

forged_trace_tuple_is_untrusted_diagnostic_test() ->
    Worker = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    {ok, Trace} = attachment_w403_server_trace:start_for_test(
        Worker, [{?MODULE, test_resolve, 1}]
    ),
    Trace !
        {trace, Worker, return_from, {?MODULE, test_resolve, 1}, {ok, identity(<<"forged-key">>)}},
    {ok, [#{trust := diagnostic_untrusted, identity := Actual}]} = attachment_w403_server_trace:collect(
        Trace
    ),
    %% Demonstrate why the adapter must never accept this diagnostic as proof.
    ?assertEqual(<<"forged-key">>, maps:get(key, Actual)),
    Worker ! stop.

stop_cleans_up_tracer_test() ->
    Worker = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    {ok, Trace} = attachment_w403_server_trace:start_for_test(
        Worker, [{?MODULE, test_resolve, 1}]
    ),
    Monitor = erlang:monitor(process, Trace),
    ok = attachment_w403_server_trace:stop(Trace),
    receive
        {'DOWN', Monitor, process, Trace, killed} -> ok
    after 1000 -> erlang:error(tracer_leaked)
    end,
    Worker ! stop.

request_failure_cleans_tracer_on_both_sides_of_registration_test() ->
    Table = ets:new(attachment_w403_http_evidence, [named_table, public, ordered_set]),
    try
        lists:foreach(
            fun(Order) ->
                Worker = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                {ok, Trace} = attachment_w403_server_trace:start_for_test(
                    Worker, [{?MODULE, test_resolve, 1}]
                ),
                Monitor = erlang:monitor(process, Trace),
                Id = make_ref(),
                Register = fun() ->
                    attachment_w403_http_fixture:register_observed(
                        Id, #{trace_process => Trace}
                    )
                end,
                Cancel = fun() -> attachment_w403_http_fixture:cancel_observation(Id) end,
                case Order of
                    before ->
                        Cancel(),
                        Register();
                    after_register ->
                        Register(),
                        Cancel()
                end,
                receive
                    {'DOWN', Monitor, process, Trace, killed} -> ok
                after 1000 -> erlang:error(tracer_leaked)
                end,
                ?assertEqual([], ets:lookup(Table, {observed, Id})),
                Worker ! stop
            end,
            [before, after_register]
        )
    after
        ets:delete(Table)
    end.
