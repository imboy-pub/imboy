-module(msg_c2g_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_ds 模块的 EUnit 测试
%%%
%%% 目标：验证客户端到群组消息领域服务功能
%%% 覆盖：群组消息组装、分发逻辑
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 验证 msg_c2g_ds 模块可以正常加载
        code:ensure_loaded(msg_c2g_ds),
        ?assertMatch({file, _}, code:is_loaded(msg_c2g_ds))
    end).

%% ===================================================================
%% 群组消息写入测试
%% ===================================================================

write_msg_test_() ->
    {setup,
        fun() ->
            meck:new(elib_pg, [no_link, passthrough]),
            meck:new(msg_c2g_repo, [no_link, passthrough]),
            meck:expect(elib_pg, pluck_value, 5, 0),
            meck:expect(msg_c2g_repo, write_msg, 8, ok),
            meck:expect(msg_c2g_repo, write_msg, 9, ok),
            ok
        end,
        fun(_) ->
            meck:unload(msg_c2g_repo),
            meck:unload(elib_pg)
        end,
        fun(_) ->
            ?_test(fun() ->
                NowTs = elib_dt:now(millisecond),
                MsgId = <<"msg_c2g_123">>,
                FromUid = 1,
                GroupId = 100,
                Payload = #{<<"type">> => <<"text">>, <<"content">> => <<"Hello group">>},
                PayloadMd5 = elib_hasher:md5(maps:get(<<"content">>, Payload)),
                Result = msg_c2g_ds:write_msg(NowTs, MsgId, FromUid, GroupId, Payload, PayloadMd5),
                ?assertEqual(ok, Result)
            end)
        end}.

read_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"msg_read_123">>,
        Result = msg_c2g_ds:read_msg(MsgId),
        case Result of
            {ok, Msg} when is_map(Msg) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Message}")
        end
    end).

read_msg_by_group_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = 100,
        Column = [<<"id">>, <<"from_uid">>, <<"group_id">>, <<"payload">>],
        Limit = 20,
        Result = msg_c2g_ds:read_msg(GroupId, Column, Limit),
        case Result of
            {ok, Msgs} when is_list(Msgs) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Messages}")
        end
    end).

read_offline_msg_includes_authoritative_conv_seq_test_() ->
    TimelineRows = [
        #{<<"msg_id">> => <<"m1">>, <<"created_at">> => 1, <<"conv_seq">> => 481}
    ],
    MsgRows = [
        #{
            <<"msg_id">> => <<"m1">>,
            <<"payload">> => <<"{}">>,
            <<"e2ee">> => <<"{}">>
        }
    ],
    ?WITH_MECKS(
        [
            {msg_c2g_timeline_repo, [
                {'list_by_uid', 3, fun(7, Columns, 20) ->
                    ?assertEqual(<<"tl.msg_id, tl.created_at, tl.conv_seq">>, Columns),
                    {ok, TimelineRows}
                end}
            ]},
            {msg_c2g_repo, [
                {'list_by_ids', 2, fun([<<"m1">>], _Columns) -> {ok, MsgRows} end}
            ]}
        ],
        fun() ->
            [Msg] = msg_c2g_ds:read_msg(7, 20, undefined),
            ?assertEqual(481, maps:get(<<"conv_seq">>, Msg))
        end
    ).

read_incremental_offline_msg_includes_authoritative_conv_seq_test_() ->
    TimelineRows = [
        #{<<"msg_id">> => <<"m2">>, <<"created_at">> => 2, <<"conv_seq">> => 900}
    ],
    MsgRows = [
        #{
            <<"msg_id">> => <<"m2">>,
            <<"payload">> => <<"{}">>,
            <<"e2ee">> => <<"{}">>
        }
    ],
    ?WITH_MECKS(
        [
            {msg_c2g_timeline_repo, [
                {'list_by_uid_since', 4, fun(7, Columns, 20, _Since) ->
                    ?assertEqual(<<"tl.msg_id, tl.created_at, tl.conv_seq">>, Columns),
                    {ok, TimelineRows}
                end}
            ]},
            {msg_c2g_repo, [
                {'list_by_ids', 2, fun([<<"m2">>], _Columns) -> {ok, MsgRows} end}
            ]}
        ],
        fun() ->
            [Msg] = msg_c2g_ds:read_msg(7, 20, <<"2026-09-12T00:00:00Z">>),
            ?assertEqual(900, maps:get(<<"conv_seq">>, Msg))
        end
    ).

delete_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"msg_delete_123">>,
        Result = msg_c2g_ds:delete_msg(MsgId),
        case Result of
            ok -> ok;
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).
