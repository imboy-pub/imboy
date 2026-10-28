-module(channel_ds_idempotency_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% T7（WP4）改造后：channel_ds:publish_message 发帖主链路事务化
%%% （workspace_guard 归档写守卫 + 消息插入同事务提交）。
%%% W08（§6.8）后：发布序号分配、订阅者未读累加、作者查看边界推进全部
%%% 同事务完成。本测试验证幂等语义保持：duplicate 请求不递增未读计数、
%%% 不重复插入、不触碰订阅表。

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
                %% W08：发布序号分配（同事务）
                {'query', 3, fun(_C, _Sql, _Params) ->
                    {ok, [#{<<"publish_seq">> => 7}]}
                end}
            ]},
            {channel_message_repo, [
                {'add_with_request_id', 3, fun(_Conn, Data, <<"req-1">>) ->
                    ?assertEqual(11, maps:get(channel_id, Data)),
                    {ok, 99, duplicate}
                end}
            ]},
            {channel_subscription_repo, [
                {'tablename', 0, fun() -> erlang:error(unread_must_not_change) end}
            ]},
            {workspace_guard, [
                %% A1c：T7 归档写守卫需连真库（channel→workspace 行锁），与本套件
                %% 的幂等语义无关——不 mock 时守卫对 fake_conn 查询异常走 fail-closed
                %% {503, 服务器繁忙}，publish 恒失败。隔离 mock 放行。
                {'ensure_writable_tx', 2, fun(_Conn, _Ref) -> ok end}
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

publish_inserted_message_increments_unread_in_tx_test_() ->
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
                %% W08：发布序号分配（同事务）
                {'query', 3, fun(_C, _Sql, _Params) ->
                    {ok, [#{<<"publish_seq">> => 8}]}
                end},
                %% 未读累加 + 作者查看边界推进（均事务内 execute/3）
                {'execute', 3, fun(_C, _Sql, _Params) -> {ok, 1} end}
            ]},
            {channel_message_repo, [
                {'add_with_request_id', 3, fun(_Conn, _Data, <<"req-2">>) ->
                    {ok, 100, inserted}
                end}
            ]},
            {workspace_guard, [
                %% A1c：同上——T7 归档写守卫与本套件的幂等语义无关，mock 放行。
                {'ensure_writable_tx', 2, fun(_Conn, _Ref) -> ok end}
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
            %% 未读累加与作者边界推进均在事务内（execute/3 各一次）
            ?assert(meck:num_calls(elib_pg, execute, 3) >= 2)
        end
    ).
