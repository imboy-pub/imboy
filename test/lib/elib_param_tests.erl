-module(elib_param_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_param 模块的 EUnit 测试
%%%
%%% 目标：验证参数处理工具功能
%%% 覆盖：参数解析、验证、默认值、分页参数、整数参数、GET/POST参数
%%%===================================================================

%% ===================================================================
%% page/1 测试
%% ===================================================================

page_default_values_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(1, Page),
            ?assertEqual(20, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

page_with_custom_values_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"page">>, <<"3">>}, {<<"size">>, <<"50">>}] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(3, Page),
            ?assertEqual(50, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

page_with_zero_page_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"page">>, <<"0">>}, {<<"size">>, <<"10">>}] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(1, Page),
            ?assertEqual(10, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

page_with_negative_page_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"page">>, <<"-5">>}, {<<"size">>, <<"10">>}] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(1, Page),
            ?assertEqual(10, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

page_with_large_size_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"page">>, <<"1">>}, {<<"size">>, <<"2000">>}] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(1, Page),
            ?assertEqual(1000, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

page_with_zero_size_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"page">>, <<"1">>}, {<<"size">>, <<"0">>}] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(1, Page),
            ?assertEqual(20, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

page_with_invalid_values_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"page">>, <<"abc">>}, {<<"size">>, <<"xyz">>}] end),
            {Page, Size} = elib_param:page(Req0),
            ?assertEqual(1, Page),
            ?assertEqual(20, Size),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% int/3 测试
%% ===================================================================

int_from_get_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"age">>, <<"25">>}] end),
            {ok, Age} = elib_param:int(age, Req0, 0),
            ?assertEqual(25, Age),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

int_from_post_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"json">>, []} end),
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<"{\"count\":42}">>, Req0} end),
            {ok, Count} = elib_param:int(count, Req0, 0),
            ?assertEqual(42, Count),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

int_with_default_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            {ok, Value} = elib_param:int(missing, Req0, 10),
            ?assertEqual(10, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

int_with_binary_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"user_id">>, <<"123">>}] end),
            {ok, UserId} = elib_param:int(<<"user_id">>, Req0, 0),
            ?assertEqual(123, UserId),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

int_with_string_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"number">>, <<"99">>}] end),
            {ok, Number} = elib_param:int(number, Req0, 0),
            ?assertEqual(99, Number),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

int_with_invalid_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"invalid">>, <<"abc">>}] end),
            {ok, Value} = elib_param:int(invalid, Req0, -1),
            ?assertEqual(-1, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

int_with_negative_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"balance">>, <<"-100">>}] end),
            {ok, Balance} = elib_param:int(balance, Req0, 0),
            ?assertEqual(-100, Balance),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% get/3 测试
%% ===================================================================

get_with_existing_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"name">>, <<"Alice">>}] end),
            Value = elib_param:get(name, Req0, undefined),
            ?assertEqual(<<"Alice">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

get_with_default_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            Value = elib_param:get(missing, Req0, <<"default">>),
            ?assertEqual(<<"default">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

get_with_atom_key_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"status">>, <<"active">>}] end),
            Value = elib_param:get(status, Req0, <<>>),
            ?assertEqual(<<"active">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% post/3 测试
%% ===================================================================

post_from_json_body_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"json">>, []} end),
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<"{\"email\":\"test@example.com\",\"password\":\"secret\"}">>, Req0} end),
            Email = elib_param:post(email, Req0, <<>>),
            ?assertEqual(<<"test@example.com">>, Email),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_with_default_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"json">>, []} end),
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<"{\"name\":\"Test\"}">>, Req0} end),
            Value = elib_param:post(missing, Req0, <<"default">>),
            ?assertEqual(<<"default">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% param/3 测试
%% ===================================================================

param_from_get_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [{<<"token">>, <<"abc123">>}] end),
            Value = elib_param:param(token, Req0, <<>>),
            ?assertEqual(<<"abc123">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

param_from_post_when_get_missing_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"json">>, []} end),
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<"{\"action\":\"submit\"}">>, Req0} end),
            Value = elib_param:param(action, Req0, <<>>),
            ?assertEqual(<<"submit">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

param_with_default_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"GET">> end),
            meck:expect(cowboy_req, parse_qs, fun(_Req0) -> [] end),
            Value = elib_param:param(missing, Req0, <<"not_found">>),
            ?assertEqual(<<"not_found">>, Value),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% post/1 测试 (解析完整 POST 参数)
%% ===================================================================

post_json_body_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"json">>, []} end),
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<"{\"username\":\"john\",\"age\":30,\"active\":true}">>, Req0} end),
            Params = elib_param:post(Req0),
            ?assertEqual(<<"john">>, maps:get(<<"username">>, Params)),
            ?assertEqual(30, maps:get(<<"age">>, Params)),
            ?assertEqual(true, maps:get(<<"active">>, Params)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_urlencoded_body_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"x-www-form-urlencoded">>, []} end),
            meck:expect(cowboy_req, read_urlencoded_body, fun(_Req0, _) -> {ok, [{<<"name">>, <<"Alice">>}, {<<"city">>, <<"NYC">>}], Req0} end),
            Params = elib_param:post(Req0),
            ?assertEqual(<<"Alice">>, maps:get(<<"name">>, Params)),
            ?assertEqual(<<"NYC">>, maps:get(<<"city">>, Params)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_urlencoded_body_duplicate_keys_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"x-www-form-urlencoded">>, []} end),
            meck:expect(cowboy_req, read_urlencoded_body, fun(_Req0, _) ->
                {ok, [{<<"tag">>, <<"a">>}, {<<"tag">>, <<"b">>}], Req0}
            end),
            Params = elib_param:post(Req0),
            ?assertEqual([<<"a">>, <<"b">>], maps:get(<<"tag">>, Params)),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

post_empty_body_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(cowboy_req, method, fun(_Req0) -> <<"POST">> end),
            meck:expect(cowboy_req, parse_header, fun(<<"content-type">>, _Req0) -> {<<"application">>, <<"json">>, []} end),
            meck:expect(cowboy_req, read_body, fun(_Req0) -> {ok, <<>>, Req0} end),
            Params = elib_param:post(Req0),
            ?assertEqual(#{}, Params),
            ?assert(meck:validate(cowboy_req))
        after
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% 辅助函数测试
%% ===================================================================

get_required_existing_value_test_() ->
    ?TEST_SIMPLE(fun() ->
        Params = #{<<"a">> => <<"1">>},
        ?assertEqual({ok, <<"1">>}, elib_param:get_required(<<"a">>, Params))
    end).

get_required_missing_or_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, missing_param}, elib_param:get_required(<<"missing">>, #{})),
        ?assertEqual({error, missing_param}, elib_param:get_required(<<"empty">>, #{<<"empty">> => <<>>}))
    end).

get_optional_uses_default_test_() ->
    ?TEST_SIMPLE(fun() ->
        Params = #{<<"a">> => <<"1">>},
        ?assertEqual(<<"1">>, elib_param:get_optional(<<"a">>, Params, <<"default">>)),
        ?assertEqual(<<"default">>, elib_param:get_optional(<<"missing">>, Params, <<"default">>))
    end).

validate_required_success_test_() ->
    ?TEST_SIMPLE(fun() ->
        Params = #{<<"a">> => <<"1">>, <<"b">> => <<"2">>},
        ?assertEqual(ok, elib_param:validate_required([<<"a">>, <<"b">>], Params))
    end).

validate_required_first_missing_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        Params = #{<<"a">> => <<"1">>, <<"c">> => <<>>},
        ?assertEqual(
            {error, {missing_param, <<"b">>}},
            elib_param:validate_required([<<"a">>, <<"b">>, <<"c">>], Params)
        )
    end).
