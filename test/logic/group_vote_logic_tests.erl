-module(group_vote_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_vote_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群投票业务逻辑层功能
%%% 覆盖：创建投票、投票操作、查询统计、权限验证
%%%===================================================================

%% ===================================================================
%% create_vote/4 测试 - 创建投票
%% ===================================================================

create_vote_success_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'insert_vote', 1, fun(_Data) ->
            {ok, 1001, [{<<"id">>, 1001}, {<<"vote_id">>, <<"vote123">>}]}
        end},
        {'insert_options_batch', 1, fun(_Options) ->
            {ok, 2}
        end}
    ], fun() ->
        Gid = 123,
        CreatorId = 456,
        Title = <<"今天吃什么？"/utf8>>,
        Options = [
            #{option_text => <<"火锅"/utf8>>, sort_order => 1},
            #{option_text => <<"烧烤"/utf8>>, sort_order => 2}
        ],
        VoteType = 1,
        IsAnonymous = false,
        EndAt = undefined,

        Result = group_vote_logic:create_vote(Gid, CreatorId, Title, Options, #{
            vote_type => VoteType,
            is_anonymous => IsAnonymous,
            end_at => EndAt
        }),
        ?assertMatch({ok, #{<<"vote_id">> := _}}, Result)
    end).

create_vote_with_options_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'insert_vote', 1, fun(_Data) ->
            {ok, 1001, [{<<"id">>, 1001}, {<<"vote_id">>, <<"vote123">>}]}
        end},
        {'insert_options_batch', 1, fun(_Options) ->
            {ok, 3}
        end}
    ], fun() ->
        Options = [
            #{option_text => <<"选项1"/utf8>>, sort_order => 1},
            #{option_text => <<"选项2"/utf8>>, sort_order => 2},
            #{option_text => <<"选项3"/utf8>>, sort_order => 3}
        ],
        Result = group_vote_logic:create_vote(123, 456, <<"多选投票"/utf8>>, Options, #{
            vote_type => 2
        }),
        ?assertMatch({ok, _}, Result)
    end).

create_vote_without_title_test_() ->
    Result = group_vote_logic:create_vote(123, 456, <<>>, [], #{}),
    ?assertMatch({error, {missing_param, title}}, Result).

create_vote_without_options_test_() ->
    Result = group_vote_logic:create_vote(123, 456, <<"标题"/utf8>>, [], #{}),
    ?assertMatch({error, {missing_param, options}}, Result).

%% ===================================================================
%% cast_vote/3 测试 - 投票
%% ===================================================================

cast_vote_single_choice_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{
                <<"vote_id">> => <<"vote123">>,
                <<"vote_type">> => 1,
                <<"status">> => 1,
                <<"end_at">> => null
            }}
        end},
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {error, not_found}
        end},
        {'list_options_by_vote_id', 1, fun(_VoteId) ->
            {ok, [
                #{<<"option_id">> => <<"opt1">>},
                #{<<"option_id">> => <<"opt2">>}
            ]}
        end},
        {'insert_record', 1, fun(_Data) ->
            {ok, 3001, [{<<"id">>, 3001}]}
        end}
    ], fun() ->
        Result = group_vote_logic:cast_vote(<<"vote123">>, 789, [<<"opt1">>]),
        ?assertMatch({ok, _}, Result)
    end).

cast_vote_multiple_choice_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{
                <<"vote_id">> => <<"vote123">>,
                <<"vote_type">> => 2,
                <<"status">> => 1,
                <<"end_at">> => null
            }}
        end},
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {error, not_found}
        end},
        {'list_options_by_vote_id', 1, fun(_VoteId) ->
            {ok, [
                #{<<"option_id">> => <<"opt1">>},
                #{<<"option_id">> => <<"opt2">>},
                #{<<"option_id">> => <<"opt3">>}
            ]}
        end},
        {'insert_record', 1, fun(_Data) ->
            {ok, 3001, [{<<"id">>, 3001}]}
        end}
    ], fun() ->
        Result = group_vote_logic:cast_vote(<<"vote123">>, 789, [<<"opt1">>, <<"opt2">>]),
        ?assertMatch({ok, _}, Result)
    end).

cast_vote_already_voted_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{<<"vote_id">> => <<"vote123">>, <<"vote_type">> => 1, <<"status">> => 1}}
        end},
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {ok, #{<<"id">> => 3001, <<"option_ids">> => <<"[\"opt1\"]">>}}
        end}
    ], fun() ->
        Result = group_vote_logic:cast_vote(<<"vote123">>, 789, [<<"opt1">>]),
        ?assertMatch({error, already_voted}, Result)
    end).

cast_vote_not_found_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = group_vote_logic:cast_vote(<<"notexist">>, 789, [<<"opt1">>]),
        ?assertEqual({error, vote_not_found}, Result)
    end).

cast_vote_closed_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{<<"vote_id">> => <<"vote123">>, <<"status">> => 2}}
        end}
    ], fun() ->
        Result = group_vote_logic:cast_vote(<<"vote123">>, 789, [<<"opt1">>]),
        ?assertEqual({error, vote_is_closed}, Result)
    end).

cast_vote_expired_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{
                <<"vote_id">> => <<"vote123">>,
                <<"status">> => 1,
                <<"end_at">> => <<"2024-01-01 00:00:00">>
            }}
        end}
    ], fun() ->
        Result = group_vote_logic:cast_vote(<<"vote123">>, 789, [<<"opt1">>]),
        ?assertEqual({error, vote_is_expired}, Result)
    end).

%% ===================================================================
%% update_vote/3 测试 - 修改投票
%% ===================================================================

update_vote_success_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {ok, #{<<"id">> => 3001}}
        end},
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{
                <<"vote_id">> => <<"vote123">>,
                <<"vote_type">> => 1,
                <<"status">> => 1
            }}
        end},
        {'update_record', 2, fun(_RecordId, _Data) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_vote_logic:update_vote(<<"vote123">>, 789, [<<"opt2">>]),
        ?assertMatch({ok, _}, Result)
    end).

update_vote_not_voted_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = group_vote_logic:update_vote(<<"vote123">>, 789, [<<"opt2">>]),
        ?assertEqual({error, not_voted_yet}, Result)
    end).

%% ===================================================================
%% cancel_vote/2 测试 - 取消投票
%% ===================================================================

cancel_vote_success_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {ok, #{<<"id">> => 3001}}
        end},
        {'delete_record', 1, fun(_RecordId) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_vote_logic:cancel_vote(<<"vote123">>, 789),
        ?assertEqual(ok, Result)
    end).

cancel_vote_not_voted_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = group_vote_logic:cancel_vote(<<"vote123">>, 789),
        ?assertEqual({error, not_voted_yet}, Result)
    end).

%% ===================================================================
%% get_vote_detail/1 测试 - 获取投票详情
%% ===================================================================

get_vote_detail_success_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{
                <<"id">> => 1001,
                <<"vote_id">> => <<"vote123">>,
                <<"title">> => <<"今天吃什么？"/utf8>>,
                <<"vote_type">> => 1,
                <<"is_anonymous">> => false,
                <<"status">> => 1
            }}
        end},
        {'list_options_by_vote_id', 1, fun(_VoteId) ->
            {ok, [
                #{<<"option_id">> => <<"opt1">>, <<"option_text">> => <<"火锅"/utf8>>},
                #{<<"option_id">> => <<"opt2">>, <<"option_text">> => <<"烧烤"/utf8>>}
            ]}
        end},
        {'count_total_votes_by_vote_id', 1, fun(_VoteId) ->
            {ok, 10}
        end},
        {'count_votes_by_option_id', 1, fun(_OptionId) ->
            {ok, 5}
        end}
    ], fun() ->
        Result = group_vote_logic:get_vote_detail(<<"vote123">>),
        ?assertMatch({ok, #{<<"vote_id">> := _, <<"options">> := [_|_]}}, Result),
        {ok, Detail} = Result,
        ?assertEqual(10, maps:get(<<"total_votes">>, Detail))
    end).

get_vote_detail_not_found_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = group_vote_logic:get_vote_detail(<<"notexist">>),
        ?assertEqual({error, vote_not_found}, Result)
    end).

%% ===================================================================
%% list_votes/3 测试 - 查询群投票列表
%% ===================================================================

list_votes_success_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'list_votes_by_group_id', 3, fun(_Gid, _Page, _Size) ->
            {ok, [
                #{<<"vote_id">> => <<"vote123">>, <<"title">> => <<"投票1"/utf8>>},
                #{<<"vote_id">> => <<"vote456">>, <<"title">> => <<"投票2"/utf8>>}
            ]}
        end},
        {'count_votes_by_group_id', 1, fun(_Gid) ->
            {ok, 2}
        end}
    ], fun() ->
        Result = group_vote_logic:list_votes(123, 1, 10),
        ?assertMatch({ok, #{<<"list">> := [_|_], <<"total">> := 2}}, Result)
    end).

%% ===================================================================
%% close_vote/1 测试 - 结束投票
%% ===================================================================

close_vote_success_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{<<"vote_id">> => <<"vote123">>, <<"status">> => 1}}
        end},
        {'update_vote_status', 2, fun(_VoteId, _Status) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_vote_logic:close_vote(<<"vote123">>),
        ?assertEqual(ok, Result)
    end).

close_vote_already_closed_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_by_vote_id', 1, fun(_VoteId) ->
            {ok, #{<<"vote_id">> => <<"vote123">>, <<"status">> => 2}}
        end}
    ], fun() ->
        Result = group_vote_logic:close_vote(<<"vote123">>),
        ?assertEqual({error, vote_already_closed}, Result)
    end).

%% ===================================================================
%% get_my_vote/2 测试 - 查询我的投票记录
%% ===================================================================

get_my_vote_voted_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {ok, #{
                <<"id">> => 3001,
                <<"option_ids">> => <<"[\"opt1\",\"opt2\"]">>,
                <<"created_at">> => <<"2024-01-01 00:00:00">>
            }}
        end}
    ], fun() ->
        Result = group_vote_logic:get_my_vote(<<"vote123">>, 789),
        ?assertMatch({ok, #{<<"option_ids">> := _}}, Result),
        {ok, MyVote} = Result,
        ?assertEqual([<<"opt1">>, <<"opt2">>], maps:get(<<"option_ids">>, MyVote))
    end).

get_my_vote_not_voted_test_() ->
    ?WITH_MECK(group_vote_repo, [
        {'find_record_by_vote_and_user', 2, fun(_VoteId, _UserId) ->
            {error, not_found}
        end}
    ], fun() ->
        Result = group_vote_logic:get_my_vote(<<"vote123">>, 789),
        ?assertEqual({error, not_voted_yet}, Result)
    end).
