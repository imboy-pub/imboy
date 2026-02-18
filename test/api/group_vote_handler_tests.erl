-module(group_vote_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_vote_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群投票 API 处理器功能
%%% 覆盖：创建投票、投票操作、查询统计、参数验证
%%%===================================================================

%% ===================================================================
%% init/2 测试 - 初始化处理器
%% ===================================================================

init_returns_ok_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'reply', 4, fun(_Status, _Headers, _Body, _Req) ->
            #{status => replied}
        end}
    ], fun() ->
        Req0 = #{},
        State0 = #{action => create, current_uid => 123},
        {ok, Req1, State} = group_vote_handler:init(Req0, State0),
        ?assert(is_map(Req1)),
        ?assert(is_map(State))
    end).

%% ===================================================================
%% handle_action/3 测试 - Action 分发
%% ===================================================================

handle_action_create_test_() ->
    fun() ->
        Req = #{},
        State = #{current_uid => 123},
        Result = group_vote_handler:handle_action(create, Req, State),
        ?assert(is_map(Result))
    end.

handle_action_list_test_() ->
    fun() ->
        Req = #{},
        State = #{current_uid => 123},
        Result = group_vote_handler:handle_action(list, Req, State),
        ?assert(is_map(Result))
    end.

%% ===================================================================
%% create/2 测试 - 创建投票
%% ===================================================================

create_success_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{
                <<"gid">> => <<"MQ">>,  % HashID for 1
                <<"title">> => <<"今天吃什么？"/utf8>>,
                <<"description">> => <<"投票选择午餐"/utf8>>,
                <<"options">> => [
                    #{<<"option_text">> => <<"火锅"/utf8>>},
                    #{<<"option_text">> => <<"烧烤"/utf8>>}
                ],
                <<"vote_type">> => 1,
                <<"is_anonymous">> => false
            }
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'decode', 1, fun(<<"MQ">>) -> 1 end}
        ], fun() ->
            ?WITH_MECK(group_vote_logic, [
                {'create_vote', 6, fun(_Gid, _Uid, _Title, _Options, _Extra, _ExtConfig) ->
                    {ok, #{<<"vote_id">> => <<"vote123">>}}
                end}
            ], fun() ->
                ?WITH_MECK(elib_response, [
                    {'success', 3, fun(_Req, _Data, _Msg) ->
                        #{status => success, body => #{<<"vote_id">> => <<"vote123">>}}
                    end}
                ], fun() ->
                    Req0 = #{},
                    State = #{current_uid => 123},
                    Result = group_vote_handler:create(Req0, State),
                    ?assertMatch(#{status := success}, Result)
                end)
            end)
        end)
    end).

create_missing_title_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{<<"gid">> => <<"MQ">>}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'decode', 1, fun(<<"MQ">>) -> 1 end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    #{status => error}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:create(Req0, State),
                ?assertMatch(#{status := error}, Result)
            end)
        end)
    end).

create_missing_options_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{<<"gid">> => <<"MQ">>, <<"title">> => <<"标题"/utf8>>}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'decode', 1, fun(<<"MQ">>) -> 1 end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    #{status => error}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:create(Req0, State),
                ?assertMatch(#{status := error}, Result)
            end)
        end)
    end).

%% ===================================================================
%% list/2 测试 - 查询投票列表
%% ===================================================================

list_success_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'parse_qs', 1, fun(_Req) ->
            [{<<"gid">>, <<"MQ">>}, {<<"page">>, <<"1">>}, {<<"size">>, <<"10">>}]
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'decode', 1, fun(<<"MQ">>) -> 1 end}
        ], fun() ->
            ?WITH_MECK(group_vote_logic, [
                {'list_votes', 3, fun(_Gid, _Page, _Size) ->
                    {ok, #{
                        <<"total">> => 2,
                        <<"page">> => 1,
                        <<"size">> => 10,
                        <<"list">> => [
                            #{<<"vote_id">> => <<"vote123">>, <<"title">> => <<"投票1"/utf8>>}
                        ]
                    }}
                end}
            ], fun() ->
                ?WITH_MECK(elib_response, [
                    {'success', 3, fun(_Req, _Data, _Msg) ->
                        #{status => success}
                    end}
                ], fun() ->
                    Req0 = #{},
                    State = #{current_uid => 123},
                    Result = group_vote_handler:list(Req0, State),
                    ?assertMatch(#{status := success}, Result)
                end)
            end)
        end)
    end).

%% ===================================================================
%% detail/2 测试 - 查询投票详情
%% ===================================================================

detail_success_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'parse_qs', 1, fun(_Req) ->
            [{<<"vote_id">>, <<"vote123">>}]
        end}
    ], fun() ->
        ?WITH_MECK(group_vote_logic, [
            {'get_vote_detail', 1, fun(_VoteId) ->
                {ok, #{
                    <<"vote_id">> => <<"vote123">>,
                    <<"title">> => <<"今天吃什么？"/utf8>>,
                    <<"options">> => [
                        #{<<"option_id">> => <<"opt1">>, <<"option_text">> => <<"火锅"/utf8>>}
                    ]
                }}
            end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    #{status => success}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:detail(Req0, State),
                ?assertMatch(#{status := success}, Result)
            end)
        end)
    end).

%% ===================================================================
%% cast/2 测试 - 投票
%% ===================================================================

cast_success_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{<<"vote_id">> => <<"vote123">>, <<"option_ids">> => [<<"opt1">>]}
        end}
    ], fun() ->
        ?WITH_MECK(group_vote_logic, [
            {'cast_vote', 3, fun(_VoteId, _Uid, _OptionIds) ->
                {ok, #{<<"vote_id">> => <<"vote123">>}}
            end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    #{status => success}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:cast(Req0, State),
                ?assertMatch(#{status := success}, Result)
            end)
        end)
    end).

cast_missing_params_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{}
        end}
    ], fun() ->
        ?WITH_MECK(elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{status => error}
            end}
        ], fun() ->
            Req0 = #{},
            State = #{current_uid => 123},
            Result = group_vote_handler:cast(Req0, State),
            ?assertMatch(#{status := error}, Result)
        end)
    end).

%% ===================================================================
%% update/2 测试 - 修改投票
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{<<"vote_id">> => <<"vote123">>, <<"option_ids">> => [<<"opt2">>]}
        end}
    ], fun() ->
        ?WITH_MECK(group_vote_logic, [
            {'update_vote', 3, fun(_VoteId, _Uid, _OptionIds) ->
                {ok, #{<<"vote_id">> => <<"vote123">>}}
            end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    #{status => success}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:update(Req0, State),
                ?assertMatch(#{status := success}, Result)
            end)
        end)
    end).

%% ===================================================================
%% cancel/2 测试 - 取消投票
%% ===================================================================

cancel_success_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{<<"vote_id">> => <<"vote123">>}
        end}
    ], fun() ->
        ?WITH_MECK(group_vote_logic, [
            {'cancel_vote', 2, fun(_VoteId, _Uid) ->
                ok
            end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    #{status => success}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:cancel(Req0, State),
                ?assertMatch(#{status := success}, Result)
            end)
        end)
    end).

%% ===================================================================
%% close/2 测试 - 结束投票
%% ===================================================================

close_success_test_() ->
    ?WITH_MECK(elib_param, [
        {'post', 1, fun(_Req) ->
            #{<<"vote_id">> => <<"vote123">>}
        end}
    ], fun() ->
        ?WITH_MECK(group_vote_logic, [
            {'close_vote', 1, fun(_VoteId) ->
                ok
            end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    #{status => success}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:close(Req0, State),
                ?assertMatch(#{status := success}, Result)
            end)
        end)
    end).

%% ===================================================================
%% my_vote/2 测试 - 查询我的投票记录
%% ===================================================================

my_vote_success_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'parse_qs', 1, fun(_Req) ->
            [{<<"vote_id">>, <<"vote123">>}]
        end}
    ], fun() ->
        ?WITH_MECK(group_vote_logic, [
            {'get_my_vote', 2, fun(_VoteId, _Uid) ->
                {ok, #{
                    <<"vote_id">> => <<"vote123">>,
                    <<"option_ids">> => [<<"opt1">>],
                    <<"created_at">> => <<"2024-01-01 00:00:00">>
                }}
            end}
        ], fun() ->
            ?WITH_MECK(elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    #{status => success}
                end}
            ], fun() ->
                Req0 = #{},
                State = #{current_uid => 123},
                Result = group_vote_handler:my_vote(Req0, State),
                ?assertMatch(#{status := success}, Result)
            end)
        end)
    end).

%% ===================================================================
%% Helper functions
%% ===================================================================
