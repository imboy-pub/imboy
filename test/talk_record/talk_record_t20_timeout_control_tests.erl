%% Pure EUnit control, NOT a business/PG PASS. Compile only after root review.
%% It calls original generators only to inspect descriptors, never their real
%% setup/cleanup/test closures. Both candidate modules must be compiled and bound.
%% The only deliberate delay is 5500ms total, just beyond EUnit's default 5000ms.
-module(talk_record_t20_timeout_control_tests).
-include_lib("eunit/include/eunit.hrl").

original_instantiator_budget_test() ->
    lists:foreach(
        fun({Module, Function}) ->
            {120, 120, Line, Original} = budgets(Module, Function),
            ?assert(is_integer(Line)),
            ?assert(is_function(Original, 0))
        end,
        [
            {talk_record_delete_tests, whitelist_flags_test_},
            {talk_record_t20_handler_tests, item_method_not_allowed_test_}
        ]
    ).

survives_default_deadline_test_() ->
    control(talk_record_delete_tests, whitelist_flags_test_, 5500).

handler_instantiator_control_test_() ->
    control(talk_record_t20_handler_tests, item_method_not_allowed_test_, 0).

budgets(Module, Function) ->
    {timeout, Outer, {setup, _RealSetup, _RealCleanup, Instantiate}} =
        apply(Module, Function, []),
    %% Instantiation constructs a closure; it does not execute its DB body.
    {timeout, Inner, {Line, Original}} = Instantiate(pure_control_state),
    {Outer, Inner, Line, Original}.

control(Module, Function, Delay) ->
    {120, 120, Line, Original} = budgets(Module, Function),
    true = is_function(Original, 0),
    %% Preserve the actual wrapper shape and budgets; replace every effectful
    %% closure with a local control. Never invoke Original or the real fixture.
    {timeout, 120,
        {setup, fun() -> pure_control_state end, fun(pure_control_state) -> ok end, fun(State) ->
            {timeout, 120,
                {Line, fun() ->
                    Started = erlang:monotonic_time(millisecond),
                    timer:sleep(Delay),
                    ?assertEqual(pure_control_state, State),
                    ?assert(erlang:monotonic_time(millisecond) - Started >= Delay)
                end}}
        end}}.
