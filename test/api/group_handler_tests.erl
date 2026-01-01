-module(group_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群组处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试创建群组功能
add_test_() ->
    ?WITH_MOCKS([
        {throttle, [
            {'check', 2, fun(_Type, _UserId) ->
                ok  % 未超过限制
            end}
        ]},
        {imboy_pg, [
            {'pluck', 4, fun(_Table, _Column, _Conditions, _Options) ->
                {ok, 0}  % 用户当前群组数量为0
            end}
        ]},
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"member_uids">>, [67890, 11111]}
                ]
            end}
        ]},
        {group_logic, [
            {'add', 4, fun(_Count, _UserId, _Type, _MemberUids) ->
                {ok, 1001}  % 新创建的群组ID
            end}
        ]},
        {group_repo, [
            {'find_by_id', 2, fun(_GroupId, _Columns) ->
                #{
                    id => 1001,
                    title => <<"Test Group">>,
                    avatar => <<"https://example.com/group_avatar.jpg">>,
                    introduction => <<"A test group">>,
                    owner_uid => 12345,
                    creator_uid => 12345,
                    type => 2,
                    status => 1,
                    created_at => <<"2025-12-24 10:00:00">>
                }
            end}
        ]},
        {group_member_logic, [
            {'list_member', 1, fun(_GroupId) ->
                {ok, [
                    #{user_id => 12345, nickname => <<"Owner">>, avatar => <<"avatar1.jpg">>},
                    #{user_id => 67890, nickname => <<"Member1">>, avatar => <<"avatar2.jpg">>},
                    #{user_id => 11111, nickname => <<"Member2">>, avatar => <<"avatar3.jpg">>}
                ]}
            end}
        ]},
        {imboy_hashids, [
            {'replace_id', 1, fun(Group) ->
                _UserId = maps:get(<<"owner_uid">>, Group),
                _CreatorId = maps:get(<<"creator_uid">>, Group),
                Group#{
                    <<"id">> => <<"encoded_1001">>,
                    <<"owner_uid">> => <<"encoded_12345">>,
                    <<"creator_uid">> => <<"encoded_12345">>
                }
            end},
            {'replace_id', 2, fun(Group, _Field) ->
                Group  % 简化处理
            end}
        ]},
        {group_member_transfer, [
            {'member_list', 1, fun(MemberList) ->
                MemberList
            end}
        ]},
        {imboy_response, [
            {'success', 4, fun(_Req, _Data, _Message, _Headers) ->
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
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试创建群组功能 - 超过限制
add_throttle_exceeded_test_() ->
    ?WITH_MOCKS([
        {throttle, [
            {'check', 2, fun(_Type, _UserId) ->
                {limit_exceeded, 1, 3}  % 超过限制
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 429,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（超过限制）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(429, StatusCode)
    end).

%% @doc 测试编辑群组功能
edit_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"title">>, <<"Updated Group Title">>},
                    {<<"avatar">>, <<"https://example.com/new_avatar.jpg">>},
                    {<<"introduction">>, <<"Updated group description">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {imboy_pg, [
            {'pluck', 4, fun(_Table, _Column, _Conditions, _Options) ->
                {ok, 1}  % 群组存在
            end},
            {'update', 3, fun(_Table, _Data, _Conditions) ->
                {ok, 1}  % 更新成功
            end}
        ]},
        {imboy_dt, [
            {'now', 0, fun() ->
                <<"2025-12-24 10:30:00">>
            end}
        ]},
        {group_repo, [
            {'find_by_id', 2, fun(_GroupId, _Columns) ->
                #{
                    id => 1001,
                    title => <<"Updated Group Title">>,
                    avatar => <<"https://example.com/new_avatar.jpg">>,
                    introduction => <<"Updated group description">>,
                    owner_uid => 12345,
                    creator_uid => 12345,
                    type => 2,
                    status => 1,
                    updated_at => <<"2025-12-24 10:30:00">>
                }
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Group) ->
                Group
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
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => edit,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试编辑群组功能 - 无效群组ID
edit_invalid_group_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"invalid_id">>},
                    {<<"title">>, <<"Updated Title">>}
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
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => edit,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试获取群组详情功能
detail_test_() ->
    ?WITH_MOCKS([
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_repo, [
            {'find_by_id', 2, fun(_GroupId, _Columns) ->
                #{
                    id => 1001,
                    title => <<"Test Group">>,
                    avatar => <<"https://example.com/group_avatar.jpg">>,
                    introduction => <<"A test group">>,
                    owner_uid => 12345,
                    creator_uid => 12345,
                    type => 2,
                    status => 1,
                    member_count => 10,
                    created_at => <<"2025-12-24 10:00:00">>
                }
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Group) ->
                Group#{<<"encoded_id">> => <<"encoded_1001">>}
            end}
        ]},
        {imboy_response, [
            {'success', 4, fun(_Req, _Data, _Message, _Headers) ->
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
            qs => <<"gid=encoded_1001">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => detail,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试获取群组详情功能 - 无效群组ID
detail_invalid_group_test_() ->
    ?WITH_MOCKS([
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
        % 模拟一个 GET 请求（无效群组ID）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"gid=invalid_id">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => detail,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试面对面建群功能
face2face_test_() ->
    ?WITH_MOCKS([
        {throttle, [
            {'check', 2, fun(_Type, _UserId) ->
                ok  % 未超过限制
            end}
        ]},
        {group_logic, [
            {'face2face', 4, fun(_UserId, _Code, _Lng, _Lat) ->
                {ok, 1002}  % 新建群组ID
            end}
        ]},
        {imboy_hashids, [
            {'encode', 1, fun(_GroupId) ->
                <<"encoded_1002">>
            end}
        ]},
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) ->
                [12345, 67890]  % 群组成员UID列表
            end}
        ]},
        {user_repo, [
            {'find_by_id', 2, fun(_UserId, _Columns) ->
                #{
                    account => <<"test_user">>,
                    avatar => <<"https://example.com/avatar.jpg">>,
                    nickname => <<"Test User">>
                }
            end},
            {'list_by_ids', 2, fun(_UserIds, _Columns) ->
                {ok, [
                    #{user_id => 12345, account => <<"test_user">>, avatar => <<"avatar1.jpg">>, nickname => <<"Test User">>},
                    #{user_id => 67890, account => <<"user2">>, avatar => <<"avatar2.jpg">>, nickname => <<"User 2">>}
                ]}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 5, fun(_FromUid, _Payload, _ToUidList, _SaveOption) ->
                ok
            end}
        ]},
        {group_member_transfer, [
            {'member_list', 1, fun(MemberList) ->
                MemberList
            end}
        ]},
        {imboy_response, [
            {'success', 4, fun(_Req, _Data, _Message, _Headers) ->
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
            qs => <<"longitude=116.404&latitude=39.915&code=ABC123">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => face2face,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试面对面建群功能 - 超过限制
face2face_throttle_exceeded_test_() ->
    ?WITH_MOCKS([
        {throttle, [
            {'check', 2, fun(_Type, _UserId) ->
                {limit_exceeded, 1, 3}  % 超过限制
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 429,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求（超过限制）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"longitude=116.404&latitude=39.915&code=ABC123">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => face2face,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(429, StatusCode)
    end).
