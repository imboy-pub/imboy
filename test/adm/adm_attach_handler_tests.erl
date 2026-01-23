-module(adm_attach_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_attach_handler 模块的 EUnit 测试
%%%
%%% 目标：验证附件授权管理后台 API 功能
%%% 覆盖：附件 URI 权限检查
%%%===================================================================

%% 创建 Cowboy 2.x 模拟请求对象
%% Cowboy 2.x 使用 Map 作为请求对象，而不是 mock_request()
mock_request() ->
    #{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => 8080,
        path => <<"/adm/attach">>,
        qs => <<>>,
        headers => #{},
        peer => {{127,0,0,1}, 12345},
        body_length => 0
    }.

%% ===================================================================
%% init/2 测试
%% ===================================================================

init_with_auth_action_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"POST">> end}
    ], fun() ->
        Req = mock_request(),
        State = #{action => auth},
        {ok, NewReq, NewState} = adm_attach_handler:init(Req, State),
        ?assert(is_map(NewState)),
        ?assertNot(maps:is_key(action, NewState))
    end).

init_with_false_action_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        State = #{action => false},
        {ok, NewReq, NewState} = adm_attach_handler:init(Req, State),
        ?assert(is_map(NewState))
    end).

%% ===================================================================
%% auth/3 测试
%% ===================================================================

auth_with_single_uri_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/attachment/file1.jpg">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) -> #{response_status => 200} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

auth_with_multiple_uris_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/file1.jpg,/file2.png,/file3.pdf">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ?assertMatch([true, true, true], maps:get(<<"uri">>, Payload)),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

auth_with_mixed_permissions_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/public/file.jpg,/private/file.pdf">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(Uri) ->
                case Uri of
                    <<"/public/", _/binary>> -> true;
                    <<"/private/", _/binary>> -> false
                end
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ?assertMatch([true, false], maps:get(<<"uri">>, Payload)),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

auth_with_empty_uri_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<>>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> false end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ?assertMatch([false], maps:get(<<"uri">>, Payload)),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

auth_with_non_post_method_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'new', 0, fun() -> #{method => <<"GET">>} end}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"GET">>, Req, State),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

auth_with_many_uris_test_() ->
    ManyUris = list_to_binary(lists:join(",", [
        <<"/file", (integer_to_binary(N))/binary, ".jpg">> || N <- lists:seq(1, 100)
    ])),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => ManyUris}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ResultList = maps:get(<<"uri">>, Payload),
                ?assertEqual(100, length(ResultList)),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

auth_with_special_characters_in_uri_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/file with spaces.jpg,/file-with-dashes.pdf">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ?assertMatch([_, _], maps:get(<<"uri">>, Payload)),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

auth_with_chinese_uri_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/attachment/文件.jpg,/文档.pdf">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ?assertMatch([_, _], maps:get(<<"uri">>, Payload)),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

auth_returns_map_with_uri_key_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/test.jpg">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(_Uri) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                ?assert(maps:is_key(<<"uri">>, Payload)),
                ?assert(is_list(maps:get(<<"uri">>, Payload))),
                #{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        State = #{},
        Result = adm_attach_handler:auth(<<"POST">>, Req, State),
        ?assertMatch(#{response_status := 200}, Result)
    end).
