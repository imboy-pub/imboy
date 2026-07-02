-module(msg_delivery_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_delivery_repo 模块的 EUnit 测试（T03/P0-1 按设备送达标记）
%%%
%%% 目标：验证 SQL 组装与参数顺序（meck elib_pg，不依赖 DB）
%%%===================================================================

%% ===================================================================
%% mark_acked_batch/4
%% ===================================================================

mark_acked_batch_builds_upsert_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(Sql, Params) ->
                ?assertMatch({_, _}, binary:match(Sql, <<"INSERT INTO">>)),
                ?assertMatch({_, _}, binary:match(Sql, <<"msg_delivery">>)),
                ?assertMatch({_, _}, binary:match(Sql, <<"ON CONFLICT DO NOTHING">>)),
                %% 参数顺序：Kind, Uid, Did, 然后逐个 MsgId
                ?assertEqual([<<"c2c">>, 100, <<"did-a">>, <<"m1">>, <<"m2">>], Params),
                %% 两条消息 → 两组 VALUES
                ?assertMatch({_, _}, binary:match(Sql, <<"($1, $4, $2, $3),($1, $5, $2, $3)">>)),
                {ok, 2}
            end}
        ],
        fun() ->
            Result = msg_delivery_repo:mark_acked_batch(
                <<"c2c">>, [<<"m1">>, <<"m2">>], 100, <<"did-a">>
            ),
            ?assertEqual({ok, 2}, Result)
        end
    ).

mark_acked_batch_empty_msgids_noop_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, 0}, msg_delivery_repo:mark_acked_batch(<<"c2c">>, [], 100, <<"did-a">>))
    end).

mark_acked_delegates_to_batch_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(_Sql, Params) ->
                ?assertEqual([<<"s2c">>, 7, <<"did-b">>, <<"m9">>], Params),
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 1}, msg_delivery_repo:mark_acked(<<"s2c">>, <<"m9">>, 7, <<"did-b">>))
        end
    ).

%% ===================================================================
%% delete_delivered_batch/4
%% ===================================================================

%% 主行删除命中后应清除标记（第二条 DELETE）
delete_delivered_batch_cleans_marks_after_main_delete_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(Sql, Params) ->
                %% 以活跃窗口函数区分两条 DELETE（仅主行删除含 make_interval）
                case binary:match(Sql, <<"make_interval(days => $3)">>) of
                    {_, _} ->
                        %% 第一条：删主行，须含全端确认判定与活跃窗口
                        ?assertMatch({0, _}, binary:match(Sql, <<"DELETE FROM">>)),
                        ?assertMatch({_, _}, binary:match(Sql, <<"msg_c2c">>)),
                        ?assertMatch({_, _}, binary:match(Sql, <<"NOT EXISTS">>)),
                        ?assertMatch({_, _}, binary:match(Sql, <<"user_device">>)),
                        ?assertEqual([<<"c2c">>, 100, 30, <<"m1">>], Params),
                        {ok, 1};
                    nomatch ->
                        %% 第二条：清除主行已消失的标记
                        ?assertMatch({_, _}, binary:match(Sql, <<"msg_delivery">>)),
                        ?assertEqual([<<"c2c">>, 100, <<"m1">>], Params),
                        {ok, 1}
                end
            end}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_delivery_repo:delete_delivered_batch(<<"c2c">>, [<<"m1">>], 100, 30)
            ),
            %% 两条 SQL 都应被执行
            ?assertEqual(2, meck:num_calls(elib_pg, execute, 2))
        end
    ).

%% 主行未删（仍有设备未确认）时不应触发标记清除
delete_delivered_batch_skips_mark_cleanup_when_pending_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_delivery_repo:delete_delivered_batch(<<"s2c">>, [<<"m1">>], 100, 30)
            ),
            ?assertEqual(1, meck:num_calls(elib_pg, execute, 2))
        end
    ).

delete_delivered_batch_empty_msgids_noop_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, msg_delivery_repo:delete_delivered_batch(<<"c2c">>, [], 100, 30))
    end).

%% ===================================================================
%% pending_filter/3（纯函数）
%% ===================================================================

pending_filter_c2c_test_() ->
    ?TEST_SIMPLE(fun() ->
        F = msg_delivery_repo:pending_filter(<<"c2c">>, 1, 2),
        ?assertMatch({_, _}, binary:match(F, <<"NOT EXISTS">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.msg_kind = 'c2c'">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.msg_id = msg_c2c.msg_id">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.to_uid = $1">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.to_did = $2">>))
    end).

pending_filter_s2c_param_index_test_() ->
    ?TEST_SIMPLE(fun() ->
        F = msg_delivery_repo:pending_filter(<<"s2c">>, 1, 3),
        ?assertMatch({_, _}, binary:match(F, <<"a.msg_id = msg_s2c.msg_id">>)),
        ?assertMatch({_, _}, binary:match(F, <<"a.to_did = $3">>))
    end).
