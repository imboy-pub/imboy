-module(agent_hub_runtime_trace_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% Persists one complete local-fixture chain for the SQL trace exporter.
runtime_trace_seed_test_() ->
    ?TEST_WITH_DB(fun() ->
        setup_group_ds([10, 11, 42]),
        try
            Principal = #{
                owner_uid => 42,
                client_id => 7001,
                client_key => unique(<<"mck-trace-">>)
            },
            Ctx = #{auth_info => Principal},
            Args = #{
                <<"group_id">> => erlang:unique_integer([positive]) + 900000,
                <<"tool">> => <<"agent_hub.trace.v1">>,
                <<"idempotency_key">> => unique(<<"trace-idem-">>)
            },
            {structured, Created} = imboy_mcp_tools:create_agent_task(Args, Ctx),
            TaskId = maps:get(<<"task_id">>, Created),
            Corr = maps:get(<<"correlation_id">>, Created),
            {structured, _} = imboy_mcp_tools:update_agent_task(
                #{<<"task_id">> => TaskId, <<"action">> => <<"start">>}, Ctx
            ),
            {structured, _} = imboy_mcp_tools:request_task_approval(
                #{<<"task_id">> => TaskId}, Ctx
            ),
            ?assertEqual({ok, approved}, agent_task_observer:approve(TaskId, 10)),
            {structured, _} = imboy_mcp_tools:update_agent_task(
                #{<<"task_id">> => TaskId, <<"action">> => <<"start">>}, Ctx
            ),
            {structured, _} = imboy_mcp_tools:update_agent_task(
                #{<<"task_id">> => TaskId, <<"action">> => <<"complete">>}, Ctx
            ),
            ?assertEqual(
                {structured, #{<<"task_id">> => TaskId, <<"status">> => <<"completed">>}},
                imboy_mcp_tools:get_agent_task(#{<<"task_id">> => TaskId}, Ctx)
            ),
            DeliveryId = unique(<<"dlv-">>),
            {ok, inserted} = bot_webhook_delivery_repo:insert(#{
                delivery_id => DeliveryId,
                bot_id => 100,
                event_type => <<"agent_task.completed">>,
                payload => <<"{}">>,
                correlation_id => Corr,
                idempotency_key => <<"trace-delivery:", TaskId/binary>>,
                webhook_url => <<"http://127.0.0.1:1/fixture">>,
                webhook_host => <<"127.0.0.1">>,
                pinned_ip => <<"127.0.0.1">>
            }),
            {ok, _} = bot_webhook_delivery_repo:mark_success(DeliveryId, 1),
            {ok, Delivery} = bot_webhook_delivery_repo:get_delivery(DeliveryId),
            ?assertEqual(Corr, maps:get(<<"correlation_id">>, Delivery)),
            ?assertEqual(<<"success">>, maps:get(<<"status">>, Delivery)),
            {ok, Audit} = agent_hub_audit_repo:list_by_correlation(Corr),
            Types = [maps:get(<<"entity_type">>, Row) || Row <- Audit],
            ?assertEqual(
                [
                    <<"approval">>,
                    <<"delivery">>,
                    <<"event">>,
                    <<"execution">>,
                    <<"outcome">>,
                    <<"request">>,
                    <<"task">>
                ],
                lists:usort(Types)
            ),
            ?assertEqual(1, length([ok || <<"request">> <- Types])),
            ?assertEqual(1, length([ok || <<"outcome">> <- Types]))
        after
            cleanup_group_ds()
        end
    end).

public_task_create_persists_audit_root_test_() ->
    ?TEST_WITH_DB(fun() ->
        TaskId = unique(<<"task-public-">>),
        Corr = unique(<<"corr-">>),
        {ok, _Task, true} = agent_task_repo:ensure_task(#{
            id => TaskId,
            group_id => 5,
            agent_uid => 42,
            correlation_id => Corr,
            idempotency_key => unique(<<"idem-">>)
        }),
        {ok, Audit} = agent_hub_audit_repo:list_by_correlation(Corr),
        ?assertEqual(
            [<<"request">>, <<"task">>],
            lists:sort([maps:get(<<"entity_type">>, Row) || Row <- Audit])
        )
    end).

transition_without_audit_root_rolls_back_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = elib_pg:with_tx(fun(Conn) ->
            agent_hub_audit_repo:record_transition_tx(
                Conn,
                unique(<<"corr-">>),
                unique(<<"task-missing-">>),
                unique(<<"event-">>),
                <<"working">>,
                <<"submitted">>
            )
        end),
        ?assertMatch({error, {audit_parent_missing, task}}, Result)
    end).

unique(Prefix) ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
    <<Prefix/binary, Hex/binary>>.

setup_group_ds(Members) ->
    cleanup_group_ds(),
    case
        meck_helper:setup_mock(group_ds, [
            {'member_uids', 1, fun(_) -> Members end}
        ])
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({group_ds_mock, Reason})
    end.

cleanup_group_ds() ->
    try meck_helper:cleanup_mock(group_ds) of
        _ -> ok
    catch
        _:_ -> ok
    end.
