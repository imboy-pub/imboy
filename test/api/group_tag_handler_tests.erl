-module(group_tag_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_tag_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群组标签 API 处理器功能
%%% 覆盖：添加、删除、查询、热门标签 API
%%%===================================================================

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% @doc 创建模拟请求对象
create_req(Params) ->
    QueryParams = maps:get(qs, Params, #{}),
    PostBody = maps:get(body, Params, #{}),
    #{
        path => <<"/v1/group/tag">>,
        method => maps:get(method, Params, <<"POST">>),
        qs => QueryParams,
        body => PostBody,
        headers => #{<<"authorization">> => <<"Bearer test_token">>},
        bindings => #{}
    }.

%% @doc 创建模拟状态
create_state(Uid) ->
    #{
        current_uid => Uid,
        action => add
    }.

%% ===================================================================
%% handle_action/3 测试
%% ===================================================================

%% @doc 测试添加标签 action
handle_action_add_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = add,
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:handle_action(Action, Req, State),
        % 验证返回的是 cowboy_req 格式
        ?assert(is_map(Result))
    end).

%% @doc 测试删除标签 action
handle_action_remove_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = remove,
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:handle_action(Action, Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试查询标签列表 action
handle_action_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = list,
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:handle_action(Action, Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试搜索标签 action
handle_action_search_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = search,
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:handle_action(Action, Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试热门标签 action
handle_action_hot_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = hot,
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:handle_action(Action, Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试未知 action
handle_action_false_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = false,
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:handle_action(Action, Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% add/2 测试
%% ===================================================================

%% @doc 测试添加标签成功
add_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"gid">> => <<"test_gid">>,
                    <<"tag_name">> => <<"技术交流"/utf8>>
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"test_gid">>) -> 1 end}
        ]},
        {group_tag_logic, [
            {'add', 3, fun(_GroupId, _Uid, _TagName) -> {ok, 1} end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:add(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试添加标签缺少群组ID
add_missing_gid_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"tag_name">> => <<"技术交流"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:add(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试添加标签缺少标签名
add_missing_tag_name_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => <<"test_gid">>}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:add(Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% remove/2 测试
%% ===================================================================

%% @doc 测试删除标签成功
remove_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"gid">> => <<"test_gid">>,
                    <<"tag_name">> => <<"技术交流"/utf8>>
                }
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"test_gid">>) -> 1 end}
        ]},
        {group_tag_logic, [
            {'remove', 3, fun(_GroupId, _Uid, _TagName) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:remove(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试删除标签缺少群组ID
remove_missing_gid_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"tag_name">> => <<"技术交流"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:remove(Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% list/2 测试
%% ===================================================================

%% @doc 测试查询标签列表成功
list_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"gid">>, <<"test_gid">>}]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"test_gid">>) -> 1 end}
        ]},
        {group_tag_logic, [
            {'list', 2, fun(_GroupId, _Uid) ->
                {ok, [
                    #{<<"id">> => 1, <<"tag_name">> => <<"技术交流"/utf8>>}
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:list(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试查询标签列表缺少群组ID
list_missing_gid_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:list(Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% search/2 测试
%% ===================================================================

%% @doc 测试搜索标签成功
search_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"tag_name">>, <<"技术交流"/utf8>>}]
            end}
        ]},
        {group_tag_logic, [
            {'search', 1, fun(_TagName) ->
                {ok, [
                    #{<<"group_id">> => 1, <<"tag_name">> => <<"技术交流"/utf8>>}
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:search(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试搜索标签缺少标签名
search_missing_tag_name_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:search(Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% hot/2 测试
%% ===================================================================

%% @doc 测试获取热门标签成功
hot_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"limit">>, <<"10">>}]
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) -> {ok, 10} end}
        ]},
        {group_tag_logic, [
            {'hot_tags', 1, fun(_Limit) ->
                {ok, [
                    #{<<"tag_name">> => <<"技术交流"/utf8>>, <<"count">> => 100}
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:hot(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试获取热门标签默认限制
hot_with_default_limit_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) -> {ok, 20} end}
        ]},
        {group_tag_logic, [
            {'hot_tags', 1, fun(_Limit) ->
                ?assertEqual(20, _Limit),
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:hot(Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试无效的群组ID
add_with_invalid_gid_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => <<"0">>, <<"tag_name">> => <<"标签"/utf8>>}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"0">>) -> 0 end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:add(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试超长标签名
add_with_long_tag_name_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                LongTag = list_to_binary(lists:duplicate(100, $x)),
                #{<<"gid">> => <<"test_gid">>, <<"tag_name">> => LongTag}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"test_gid">>) -> 1 end}
        ]},
        {group_tag_logic, [
            {'add', 3, fun(_GroupId, _Uid, _TagName) ->
                {error, <<"标签名过长"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) -> #{status => error} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:add(Req, State),
        ?assert(is_map(Result))
    end).

%% @doc 测试 UTF-8 标签名
add_with_utf8_tag_name_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => <<"test_gid">>, <<"tag_name">> => <<"技术交流群"/utf8>>}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"test_gid">>) -> 1 end}
        ]},
        {group_tag_logic, [
            {'add', 3, fun(_GroupId, _Uid, TagName) ->
                ?assertEqual(<<"技术交流群"/utf8>>, TagName),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),
        Result = group_tag_handler:add(Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的 API 请求流程
complete_api_flow_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => <<"test_gid">>, <<"tag_name">> => <<"测试标签"/utf8>>}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"test_gid">>) -> 1 end}
        ]},
        {group_tag_logic, [
            {'add', 3, fun(_GroupId, _Uid, _TagName) -> {ok, 1} end},
            {'list', 2, fun(_GroupId, _Uid) ->
                {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"测试标签"/utf8>>}]}
            end},
            {'remove', 3, fun(_GroupId, _Uid, _TagName) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Msg) -> #{status => ok} end}
        ]}
    ], fun() ->
        Req = create_req(#{}),
        State = create_state(100),

        % 添加标签
        AddResult = group_tag_handler:add(Req, State),
        ?assert(is_map(AddResult)),

        % 查询标签列表
        ListResult = group_tag_handler:list(Req, State),
        ?assert(is_map(ListResult)),

        % 删除标签
        RemoveResult = group_tag_handler:remove(Req, State),
        ?assert(is_map(RemoveResult))
    end).
