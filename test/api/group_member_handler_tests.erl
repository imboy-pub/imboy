-module(group_member_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_member_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群组成员处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试添加群成员功能
add_member_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"member_uids">>, [67890, 11111]}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_member_logic, [
            {'add', 3, fun(_GroupId, _UserId, _MemberUids) ->
                {ok, #{added_count => 2}}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_member_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试移除群成员功能
remove_member_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"member_uid">>, <<"encoded_67890">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<"encoded_1001">> -> 1001;
                    <<"encoded_67890">> -> 67890
                end
            end}
        ]},
        {group_member_logic, [
            {'remove', 3, fun(_GroupId, _UserId, _MemberUid) ->
                {ok, #{removed_count => 1}}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_member_handler:init(MockReq, #{
            action => remove,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试列出群成员功能
list_members_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_member_logic, [
            {'list_member', 2, fun(_GroupId, _Page) ->
                [
                    #{
                        user_id => 12345,
                        nickname => <<"Owner">>,
                        avatar => <<"avatar1.jpg">>,
                        role => owner,
                        joined_at => <<"2025-12-24 10:00:00">>
                    },
                    #{
                        user_id => 67890,
                        nickname => <<"Member1">>,
                        avatar => <<"avatar2.jpg">>,
                        role => member,
                        joined_at => <<"2025-12-24 10:01:00">>
                    }
                ]
            end}
        ]},
        {group_member_transfer, [
            {'member_list', 1, fun(MemberList) ->
                MemberList
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"gid=encoded_1001&page=1">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_member_handler:init(MockReq, #{
            action => list,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试转让群主功能
transfer_owner_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"new_owner_uid">>, <<"encoded_67890">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<"encoded_1001">> -> 1001;
                    <<"encoded_67890">> -> 67890
                end
            end}
        ]},
        {group_member_logic, [
            {'transfer_owner', 3, fun(_GroupId, _UserId, _NewOwnerId) ->
                {ok, #{transferred => true}}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_member_handler:init(MockReq, #{
            action => transfer_owner,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试添加群成员功能 - 无效群组ID
add_member_invalid_group_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"invalid_id">>},
                    {<<"member_uids">>, [67890]}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                0  % 无效ID
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 400,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（无效群组ID）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_member_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试退出群组功能
leave_group_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_member_logic, [
            {'leave', 2, fun(_GroupId, _UserId) ->
                {ok, #{left => true}}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_member_handler:init(MockReq, #{
            action => leave,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).
