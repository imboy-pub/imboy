-module(msg_store_ack_durability_tests).
-include_lib("eunit/include/eunit.hrl").

completion_order_test_() ->
    [
        case_test(Write, Archive, Mark)
     || {Write, Archive, Mark} <- [
            {ok, ok, {ok, 1}},
            {{error, conflict_no_insert}, ok, {ok, 1}},
            {ok, {error, timeout}, {ok, 1}},
            {{error, timeout}, ok, {ok, 1}},
            {ok, ok, {error, timeout}}
        ]
    ].

case_test(Write, Archive, Mark) ->
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() ->
            pure_mock_fixture:with_mocks(
                [
                    {msg_c2c_ds, [
                        %% ba106bc0 起 write_msg 增加 accepted_at/expire_at 两参（/11），
                        %% 与产品调用点 src/ds/msg_store_worker.erl do_write(c2c, Row)
                        %% 的 11 参保持一致；桩只关心调用事件与返回值，不读参数。
                        {write_msg, 11, fun(_, _, _, _, _, _, _, _, _, _, _) ->
                            event(write),
                            Write
                        end}
                    ]},
                    {msg_archive_repo, [
                        {archive, 1, fun(_) ->
                            event(archive),
                            Archive
                        end}
                    ]},
                    {msg_store_repo, [
                        {mark_processed, 3, fun(_, <<"c2c">>, <<"ack-durability">>) ->
                            event(processed),
                            Mark
                        end},
                        {mark_failed, 4, fun(_, _, _, _) ->
                            event(retry),
                            {ok, 1}
                        end}
                    ]},
                    {msg_store_ds, [
                        {unstage, 2, fun(_, _) ->
                            event(async_unstage),
                            ok
                        end}
                    ]},
                    %% 05b3adba0 起 complete_staged 在 c2c 完成事务内调用
                    %% reconcile_consumed_tx 回填已消费 ACK 的结果载荷；
                    %% 桩记事件以断言：完成路径必经 reconcile、
                    %% mark_processed/archive 失败的回滚路径不经 reconcile。
                    {c2c_action_receipt_repo, [
                        {reconcile_consumed_tx, 2, fun(_, _) ->
                            event(reconciled),
                            ok
                        end}
                    ]},
                    {msg_delivery_repo, [
                        {delete_delivered_batch, 5, fun(
                            _, <<"c2c">>, [<<"ack-durability">>], 9, 30
                        ) ->
                            event(clean),
                            ok
                        end}
                    ]},
                    {elib_pg, [
                        {with_tx, 1, fun(F) ->
                            try
                                F(self())
                            catch
                                throw:{rollback, R} -> {rollback, R}
                            end
                        end}
                    ]}
                ],
                fun() ->
                    application:set_env(imboy, msg_archive_enabled, true),
                    put(events, []),
                    msg_store_worker:process_row(#{
                        <<"type">> => <<"c2c">>,
                        <<"msg_id">> => <<"ack-durability">>,
                        <<"payload">> => <<"{}">>,
                        <<"from_id">> => 8,
                        <<"to_id">> => 9
                    }),
                    Expected =
                        case {Write, Archive, Mark} of
                            {{error, timeout}, _, _} -> [write, retry];
                            {_, {error, timeout}, _} -> [write, archive, retry];
                            %% mark_processed 失败在 reconcile 之前抛回滚，
                            %% 该路径不经 reconcile（回滚不触碰 ACK 事实表）。
                            {_, _, {error, timeout}} -> [write, archive, processed, retry];
                            _ -> [write, archive, processed, reconciled, clean]
                        end,
                    ?assertEqual(Expected, lists:reverse(get(events)))
                end
            )
        end)
    end}.

event(Event) ->
    put(events, [Event | get(events)]),
    ok.
