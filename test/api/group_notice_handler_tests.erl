-module(group_notice_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群组公告处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试创建群公告功能
create_notice_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"title">>, <<"Important Notice">>},
                    {<<"content">>, <<"This is an important group notice">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_notice_logic, [
            {'create', 4, fun(_GroupId, _UserId, _Title, _Content) ->
                {ok, #{notice_id => 2001, created => true}}
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
        {ok, Req, _State} = group_notice_handler:init(MockReq, #{
            action => create,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试更新群公告功能
update_notice_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"notice_id">>, <<"2001">>},
                    {<<"title">>, <<"Updated Notice">>},
                    {<<"content">>, <<"Updated notice content">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_notice_logic, [
            {'update', 5, fun(_GroupId, _UserId, _NoticeId, _Title, _Content) ->
                {ok, #{updated => true}}
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
        {ok, Req, _State} = group_notice_handler:init(MockReq, #{
            action => update,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试删除群公告功能
delete_notice_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"notice_id">>, <<"2001">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_notice_logic, [
            {'delete', 3, fun(_GroupId, _UserId, _NoticeId) ->
                {ok, #{deleted => true}}
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
        {ok, Req, _State} = group_notice_handler:init(MockReq, #{
            action => delete,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试列出群公告功能
list_notices_test_() ->
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
        {group_notice_logic, [
            {'list', 2, fun(_GroupId, _Page) ->
                [
                    #{
                        notice_id => 2001,
                        title => <<"Important Notice">>,
                        content => <<"This is an important group notice">>,
                        creator_uid => 12345,
                        creator_name => <<"Group Owner">>,
                        created_at => <<"2025-12-24 10:00:00">>,
                        updated_at => <<"2025-12-24 10:00:00">>
                    },
                    #{
                        notice_id => 2002,
                        title => <<"Weekly Update">>,
                        content => <<"Weekly group update and announcements">>,
                        creator_uid => 12345,
                        creator_name => <<"Group Owner">>,
                        created_at => <<"2025-12-23 15:00:00">>,
                        updated_at => <<"2025-12-23 15:00:00">>
                    }
                ]
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
        {ok, Req, _State} = group_notice_handler:init(MockReq, #{
            action => list,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试列出群公告功能 - 无效群组ID
list_notices_invalid_group_test_() ->
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
            qs => <<"gid=invalid_id&page=1">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_notice_handler:init(MockReq, #{
            action => list,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试创建群公告功能 - 权限不足
create_notice_permission_denied_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"gid">>, <<"encoded_1001">>},
                    {<<"title">>, <<"Unauthorized Notice">>},
                    {<<"content">>, <<"This should fail">>}
                ]
            end}
        ]},
        {imboy_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                1001
            end}
        ]},
        {group_notice_logic, [
            {'create', 4, fun(_GroupId, _UserId, _Title, _Content) ->
                {error, permission_denied}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 403,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（权限不足）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = group_notice_handler:init(MockReq, #{
            action => create,
            current_uid => 99999  % 非管理员用户
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(403, StatusCode)
    end).
