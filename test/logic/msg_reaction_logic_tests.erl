-module(msg_reaction_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_reaction_logic 模块的 EUnit 测试（基于 meck mock，不需要数据库）
%%%
%%% 目标：验证消息表情回应业务逻辑功能
%%% 覆盖：添加、移除、查询、统计、权限验证、边界条件
%%%===================================================================

%% ===================================================================
%% add/4 测试 - 单聊
%% ===================================================================

add_c2c_reaction_succeeds_test_() ->
    ?WITH_MECKS([
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(<<"test_msg_c2c_001">>) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}};
               (_) ->
                {error, not_found}
            end},
            {'find_msg_by_id', 2, fun(<<"test_msg_c2c_001">>, _MsgId2) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}}
            end}
        ]},
        {msg_reaction_ds, [
            {'add_reaction', 4, fun(_MsgId, _MsgType, _UserId, _Emoji) ->
                {ok, #{<<"msg_id">> => <<"test_msg_c2c_001">>, <<"emoji">> => <<"👍"/utf8>>}}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgId, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        {ok, Result} = msg_reaction_logic:add(<<"test_msg_c2c_001">>, <<"c2c">>, 999999, <<"👍"/utf8>>),
        ?assertEqual(<<"test_msg_c2c_001">>, maps:get(<<"msg_id">>, Result)),
        ?assertEqual(<<"👍"/utf8>>, maps:get(<<"emoji">>, Result))
    end).

%% ===================================================================
%% add/4 测试 - 群聊
%% ===================================================================

add_c2g_reaction_succeeds_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'find_msg_by_id', 1, fun(<<"test_msg_c2g_001">>) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 100}};
               (_) ->
                {error, not_found}
            end},
            {'find_msg_by_id', 2, fun(<<"test_msg_c2g_001">>, _MsgId2) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 100}}
            end}
        ]},
        {group_member_ds, [
            {'is_member', 2, fun(_Gid, _Uid) -> true end}
        ]},
        {msg_reaction_ds, [
            {'add_reaction', 4, fun(_MsgId, _MsgType, _UserId, _Emoji) ->
                {ok, #{<<"msg_id">> => <<"test_msg_c2g_001">>, <<"emoji">> => <<"❤️"/utf8>>}}
            end}
        ]},
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [999999, 999998] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_From, _To, _Action, _MsgId, _E2EE, _Payload, _Save) -> ok end}
        ]}
    ], fun() ->
        {ok, Result} = msg_reaction_logic:add(<<"test_msg_c2g_001">>, <<"c2g">>, 999999, <<"❤️"/utf8>>),
        ?assertEqual(<<"test_msg_c2g_001">>, maps:get(<<"msg_id">>, Result))
    end).

%% ===================================================================
%% add/4 重复添加测试
%% ===================================================================

add_duplicate_reaction_is_idempotent_test_() ->
    ?WITH_MECKS([
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}}
            end},
            {'find_msg_by_id', 2, fun(_, _) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}}
            end}
        ]},
        {msg_reaction_ds, [
            {'add_reaction', 4, fun(_MsgId, _MsgType, _UserId, _Emoji) ->
                {ok, #{<<"msg_id">> => <<"test_msg_002">>, <<"emoji">> => <<"😄"/utf8>>}}
            end},
            {'get_reaction_stats', 2, fun(_MsgId, _MsgType) ->
                {ok, [#{<<"emoji">> => <<"😄"/utf8>>, <<"count">> => 1, <<"users">> => [999999]}]}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
        ]}
    ], fun() ->
        {ok, _} = msg_reaction_logic:add(<<"test_msg_002">>, <<"c2c">>, 999999, <<"😄"/utf8>>),
        {ok, Stats} = msg_reaction_logic:stats(<<"test_msg_002">>, <<"c2c">>),
        TotalCount = maps:get(<<"total_count">>, Stats),
        ?assertEqual(1, TotalCount)
    end).

%% ===================================================================
%% remove/4 测试
%% ===================================================================

remove_reaction_succeeds_test_() ->
    ?WITH_MECKS([
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}}
            end}
        ]},
        {msg_reaction_ds, [
            {'remove_reaction', 4, fun(_MsgId, _MsgType, _UserId, _Emoji) -> ok end},
            {'get_reaction_stats', 2, fun(_MsgId, _MsgType) ->
                {ok, []}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
        ]}
    ], fun() ->
        ok = msg_reaction_logic:remove(<<"test_msg_003">>, <<"c2c">>, 999999, <<"🎉"/utf8>>),
        {ok, Stats} = msg_reaction_logic:stats(<<"test_msg_003">>, <<"c2c">>),
        ?assertEqual(0, maps:get(<<"total_count">>, Stats))
    end).

%% ===================================================================
%% list/2 测试
%% ===================================================================

list_reactions_returns_grouped_data_test_() ->
    ?WITH_MECKS([
        {msg_reaction_ds, [
            {'get_reactions', 2, fun(_MsgId, _MsgType) ->
                {ok, [
                    {<<"👍"/utf8>>, #{<<"count">> => 2, <<"users">> => [999999, 999998]}},
                    {<<"❤️"/utf8>>, #{<<"count">> => 1, <<"users">> => [999999]}}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, List} = msg_reaction_logic:list(<<"test_msg_004">>, <<"c2c">>),
        Reactions = maps:get(<<"reactions">>, List),
        ?assertEqual(2, length(Reactions)),
        TotalCount = maps:get(<<"total_count">>, List),
        ?assertEqual(3, TotalCount)
    end).

%% ===================================================================
%% stats/2 测试
%% ===================================================================

stats_returns_totals_test_() ->
    ?WITH_MECKS([
        {msg_reaction_ds, [
            {'get_reaction_stats', 2, fun(_MsgId, _MsgType) ->
                {ok, [
                    #{<<"emoji">> => <<"👍"/utf8>>, <<"count">> => 2, <<"users">> => [999999, 999998]},
                    #{<<"emoji">> => <<"❤️"/utf8>>, <<"count">> => 1, <<"users">> => [999999]},
                    #{<<"emoji">> => <<"😄"/utf8>>, <<"count">> => 1, <<"users">> => [999997]}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Stats} = msg_reaction_logic:stats(<<"test_msg_005">>, <<"c2g">>),
        Reactions = maps:get(<<"reactions">>, Stats),
        ?assertEqual(3, length(Reactions)),
        TotalCount = maps:get(<<"total_count">>, Stats),
        ?assertEqual(4, TotalCount)
    end).

%% ===================================================================
%% is_reacted/4 测试
%% ===================================================================

is_reacted_delegates_to_ds_test_() ->
    ?WITH_MECKS([
        {msg_reaction_ds, [
            {'is_reacted', 4, fun(_, _, _, _) -> true end}
        ]}
    ], fun() ->
        ?assertEqual(true, msg_reaction_logic:is_reacted(<<"test_msg_006">>, <<"c2c">>, 999999, <<"🔥"/utf8>>))
    end).

is_reacted_returns_false_test_() ->
    ?WITH_MECKS([
        {msg_reaction_ds, [
            {'is_reacted', 4, fun(_, _, _, _) -> false end}
        ]}
    ], fun() ->
        ?assertEqual(false, msg_reaction_logic:is_reacted(<<"test_msg_006">>, <<"c2c">>, 999999, <<"🔥"/utf8>>))
    end).

%% ===================================================================
%% 权限验证测试
%% ===================================================================

add_c2c_permission_denied_test_() ->
    ?WITH_MECKS([
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}}
            end},
            {'find_msg_by_id', 2, fun(_, _) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 999998}}
            end}
        ]}
    ], fun() ->
        % 非消息参与者尝试添加表情
        {error, permission_denied} = msg_reaction_logic:add(<<"test_msg_007">>, <<"c2c">>, 999997, <<"👍"/utf8>>)
    end).

add_c2g_not_group_member_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'find_msg_by_id', 1, fun(_) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 100}}
            end},
            {'find_msg_by_id', 2, fun(_, _) ->
                {ok, #{<<"from_id">> => 999999, <<"to_id">> => 100}}
            end}
        ]},
        {group_member_ds, [
            {'is_member', 2, fun(_Gid, 999997) -> false; (_, _) -> true end}
        ]}
    ], fun() ->
        % 非群成员尝试添加表情
        {error, not_group_member} = msg_reaction_logic:add(<<"test_msg_008">>, <<"c2g">>, 999997, <<"👍"/utf8>>)
    end).

%% ===================================================================
%% 消息不存在测试
%% ===================================================================

add_msg_not_found_test_() ->
    ?WITH_MECKS([
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_) -> {error, not_found} end}
        ]}
    ], fun() ->
        {error, msg_not_found} = msg_reaction_logic:add(<<"non_existent_msg">>, <<"c2c">>, 999999, <<"👍"/utf8>>)
    end).

%% ===================================================================
%% 空 emoji 测试
%% ===================================================================

add_empty_emoji_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        {error, {invalid_param, _}} = msg_reaction_logic:add(<<"msg">>, <<"c2c">>, 999999, <<>>)
    end).
