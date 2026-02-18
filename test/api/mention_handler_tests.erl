-module(mention_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% mention_handler 模块的 EUnit 测试
%%%
%%% 目标：验证@提及 API 接口功能
%%% 覆盖：查询@消息列表、标记已读、获取成员建议
%%%===================================================================

%% ===================================================================
%% init/2 测试 - action 分发
%% ===================================================================

init_dispatches_to_list_action_test_() ->
    ?WITH_MECKS([
        {mention_handler, [
            {'list', 2, fun(_Req, _State) -> req1 end}
        ]}
    ], fun() ->
        Req0 = req_mock(),
        State0 = #{action => list, current_uid => 100},
        {ok, Req, _State} = mention_handler:init(Req0, State0),
        ?assertEqual(req1, Req)
    end).

init_dispatches_to_unread_action_test_() ->
    ?WITH_MECKS([
        {mention_handler, [
            {'unread', 2, fun(_Req, _State) -> req2 end}
        ]}
    ], fun() ->
        Req0 = req_mock(),
        State0 = #{action => unread, current_uid => 100},
        {ok, Req, _State} = mention_handler:init(Req0, State0),
        ?assertEqual(req2, Req)
    end).

init_dispatches_to_mark_read_action_test_() ->
    ?WITH_MECKS([
        {mention_handler, [
            {'mark_read', 2, fun(_Req, _State) -> req3 end}
        ]}
    ], fun() ->
        Req0 = req_mock(),
        State0 = #{action => mark_read, current_uid => 100},
        {ok, Req, _State} = mention_handler:init(Req0, State0),
        ?assertEqual(req3, Req)
    end).

init_dispatches_to_suggest_action_test_() ->
    ?WITH_MECKS([
        {mention_handler, [
            {'suggest', 2, fun(_Req, _State) -> req4 end}
        ]}
    ], fun() ->
        Req0 = req_mock(),
        State0 = #{action => suggest, current_uid => 100},
        {ok, Req, _State} = mention_handler:init(Req0, State0),
        ?assertEqual(req4, Req)
    end).

%% ===================================================================
%% list/2 测试
%% ===================================================================

list_returns_mentions_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {mention_logic, [
            {'list_mentions', 3, fun(_Uid, false, _Options) ->
                {ok, [
                    #{<<"msg_id">> => <<"msg1">>, <<"group_id">> => 100},
                    #{<<"msg_id">> => <<"msg2">>, <<"group_id">> => 100}
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:list(Req, State),
        ?assertEqual(req_ok, Result)
    end).

list_with_is_read_param_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"is_read">>, <<"true">>}] end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {mention_logic, [
            {'list_mentions', 3, fun(_Uid, true, _Options) ->
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:list(Req, State),
        ?assertEqual(req_ok, Result)
    end).

%% ===================================================================
%% unread/2 测试
%% ===================================================================

unread_returns_unread_count_test_() ->
    ?WITH_MECKS([
        {mention_logic, [
            {'count_unread', 1, fun(_Uid) -> 5 end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:unread(Req, State),
        ?assertEqual(req_ok, Result)
    end).

%% ===================================================================
%% mark_read/2 测试
%% ===================================================================

mark_read_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"msg_id">> => <<"msg1">>}
            end}
        ]},
        {mention_logic, [
            {'mark_as_read', 2, fun(_MsgId, _Uid) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:mark_read(Req, State),
        ?assertEqual(req_ok, Result)
    end).

mark_read_missing_msg_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> req_error end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:mark_read(Req, State),
        ?assertEqual(req_error, Result)
    end).

%% ===================================================================
%% suggest/2 测试
%% ===================================================================

suggest_returns_member_list_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"gid">>, <<"gid123">>}, {<<"keyword">>, <<"张"/utf8>>}] end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"gid123">>) -> 100 end}
        ]},
        {mention_logic, [
            {'get_member_suggestions', 3, fun(_Gid, _Uid, _Keyword) ->
                {ok, [
                    #{<<"id">> => <<"user1">>, <<"nickname">> => <<"张三"/utf8>>}
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:suggest(Req, State),
        ?assertEqual(req_ok, Result)
    end).

suggest_missing_gid_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> req_error end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 100},
        Result = mention_handler:suggest(Req, State),
        ?assertEqual(req_error, Result)
    end).

%% ===================================================================
%% Helper functions
%% ===================================================================

req_mock() ->
    #{
        mock_req => true
    }.
