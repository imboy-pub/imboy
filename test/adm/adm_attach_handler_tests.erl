-module(adm_attach_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"GET">>, path => <<"/adm/attach">>}.

init_false_action_passthrough_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = adm_attach_handler:init(Req, #{action => false, adm_user_id => 10}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{adm_user_id => 10}, State)
    end).

init_auth_single_uri_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<"/attachment/file1.jpg">>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(<<"/attachment/file1.jpg">>) -> true end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual([true], maps:get(<<"uri">>, maps:get(payload, RespReq)))
    end).

init_auth_mixed_permissions_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
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
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual([true, false], maps:get(<<"uri">>, maps:get(payload, RespReq)))
    end).

init_auth_empty_uri_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uri">> => <<>>}
            end}
        ]},
        {elib_uri, [
            {'check_auth', 1, fun(<<>>) -> false end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual([false], maps:get(<<"uri">>, maps:get(payload, RespReq)))
    end).

init_auth_non_post_passthrough_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
        ?assertEqual(Req, RespReq)
    end).
