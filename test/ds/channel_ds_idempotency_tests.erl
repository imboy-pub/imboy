-module(channel_ds_idempotency_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% T7（WP4）改造后：channel_ds:publish_message 发帖主链路事务化
%%% （workspace_guard 归档写守卫 + 消息插入同事务提交，未读计数事务后执行）。
%%% 本测试验证幂等语义保持：duplicate 请求不递增未读计数、不重复插入。

publish_duplicate_request_does_not_increment_unread_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [
                {'find_by_id', 2, fun(1001, <<"nickname,avatar">>) ->
                    #{
                        <<"nickname">> => <<"admin">>,
                        <<"avatar">> => <<"avatar">>
                    }
                end}
            ]},
            {jsone_encode, [
                {'encode', 2, fun(#{}, [native_utf8]) -> {ok, <<"{}">>} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-08-09T00:00:00Z">> end}
            ]},
            %% T7 事务模拟：with_tx 内 throw abort_tx 归一 {error, Reason}
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end},
                {'query', 3, fun(_C, _Sql, _Params) -> {ok, []} end}
            ]},
            {channel_message_repo, [
                {'add_with_request_id', 3, fun(_Conn, Data, <<"req-1">>) ->
                    ?assertEqual(11, maps:get(channel_id, Data)),
                    {ok, 99, duplicate}
                end}
            ]},
            {channel_subscription_repo, [
                {'tablename', 0, fun() -> erlang:error(unread_must_not_change) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 99, duplicate},
                channel_ds:publish_message(
                    11,
                    1001,
                    <<"hello">>,
                    <<"text">>,
                    #{},
                    <<"req-1">>
                )
            )
        end
    ).

publish_inserted_message_increments_unread_after_tx_test_() ->
    ?WITH_MECKS(
        [
            {user_repo, [
                {'find_by_id', 2, fun(1001, <<"nickname,avatar">>) ->
                    #{
                        <<"nickname">> => <<"admin">>,
                        <<"avatar">> => <<"avatar">>
                    }
                end}
            ]},
            {jsone_encode, [
                {'encode', 2, fun(#{}, [native_utf8]) -> {ok, <<"{}">>} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-08-09T00:00:00Z">> end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end},
                {'query', 3, fun(_C, _Sql, _Params) -> {ok, []} end},
                %% increment_all_unread 的自动提交计数更新（事务外）
                {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]},
            {channel_message_repo, [
                {'add_with_request_id', 3, fun(_Conn, _Data, <<"req-2">>) ->
                    {ok, 100, inserted}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 100, inserted},
                channel_ds:publish_message(
                    11,
                    1001,
                    <<"hello">>,
                    <<"text">>,
                    #{},
                    <<"req-2">>
                )
            ),
            %% 未读计数在插入成功后递增（increment_all_unread 走 execute/2）
            ?assert(meck:num_calls(elib_pg, execute, 2) >= 1)
        end
    ).
