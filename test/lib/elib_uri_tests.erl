-module(elib_uri_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_uri 模块的 EUnit 测试
%%%
%%% 目标：验证 URI 工具功能
%%% 覆盖：URL构建、参数解析、文件上传下载
%%%===================================================================

%% ===================================================================
%% build_query/3 测试
%% ===================================================================

build_query_basic_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = <<"https://example.com">>,
        Path = <<"api/test">>,
        Args = #{<<"key1">> => <<"value1">>, <<"key2">> => <<"value2">>},
        Result = elib_uri:build_query(Base, Path, Args),
        % 验证构建的URL包含所有部分
        ?assertMatch(<<"https://example.com/api/test?", _/binary>>, Result),
        ?assert(binary:match(Result, <<"key1=">>) =/= nomatch),
        ?assert(binary:match(Result, <<"key2=">>) =/= nomatch)
    end).

build_query_with_trailing_slash_base_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = <<"https://example.com/">>,
        Path = <<"api/test">>,
        Args = #{<<"q">> => <<"test">>},
        Result = elib_uri:build_query(Base, Path, Args),
        % 验证移除尾部斜杠
        ?assertNotMatch(<<"https://example.com//api/test">>, Result)
    end).

build_query_with_leading_slash_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = <<"https://example.com">>,
        Path = <<"/api/test">>,
        Args = #{<<"x">> => <<"1">>},
        Result = elib_uri:build_query(Base, Path, Args),
        % 验证路径正确
        ?assertMatch(<<"https://example.com/api/test?", _/binary>>, Result)
    end).

build_query_without_leading_slash_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = <<"https://example.com">>,
        Path = <<"api/test">>,
        Args = #{<<"y">> => <<"2">>},
        Result = elib_uri:build_query(Base, Path, Args),
        % 验证添加前导斜杠
        ?assertMatch(<<"https://example.com/api/test?", _/binary>>, Result)
    end).

build_query_empty_args_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = <<"https://example.com">>,
        Path = <<"path">>,
        Args = #{},
        Result = elib_uri:build_query(Base, Path, Args),
        % 验证返回结果是二进制
        ?assert(is_binary(Result)),
        ?assert(byte_size(Result) > 0)
    end).

%% ===================================================================
%% exclusion_param/2 测试
%% ===================================================================

exclusion_param_single_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/img.jpg?s=dev&a=123&v=531378&width=375">>,
        Result = elib_uri:exclusion_param(Url, [<<"width">>]),
        ?assertMatch(<<"https://example.com/img.jpg?", _/binary>>, Result),
        ?assert(binary:match(Result, <<"width=">>) =:= nomatch),
        ?assert(binary:match(Result, <<"a=123">>) =/= nomatch)
    end).

exclusion_param_multiple_keys_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/img.jpg?s=dev&a=344af61665efff23&v=531378&width=375">>,
        Result = elib_uri:exclusion_param(Url, [<<"width">>, <<"v">>]),
        ?assert(binary:match(Result, <<"width=">>) =:= nomatch),
        ?assert(binary:match(Result, <<"v=">>) =:= nomatch),
        ?assert(binary:match(Result, <<"s=">>) =/= nomatch),
        ?assert(binary:match(Result, <<"a=">>) =/= nomatch)
    end).

exclusion_param_all_keys_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/img.jpg?a=1&b=2&c=3">>,
        Result = elib_uri:exclusion_param(Url, [<<"a">>, <<"b">>, <<"c">>]),
        % 验证所有参数被排除
        ?assert(binary:match(Result, <<"a=">>) =:= nomatch),
        ?assert(binary:match(Result, <<"b=">>) =:= nomatch),
        ?assert(binary:match(Result, <<"c=">>) =:= nomatch)
    end).

exclusion_param_no_match_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/img.jpg?a=1&b=2">>,
        Result = elib_uri:exclusion_param(Url, [<<"x">>, <<"y">>]),
        % 验证参数保留
        ?assert(binary:match(Result, <<"a=1">>) =/= nomatch),
        ?assert(binary:match(Result, <<"b=2">>) =/= nomatch)
    end).

exclusion_param_with_list_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = "https://example.com/img.jpg?a=1&b=2",
        Result = elib_uri:exclusion_param(Url, [<<"b">>]),
        ?assert(binary:match(Result, <<"a=1">>) =/= nomatch),
        ?assert(binary:match(Result, <<"b=">>) =:= nomatch)
    end).

%% ===================================================================
%% get_params/1 测试
%% ===================================================================

get_params_full_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375">>,
        {UrlMap, Params} = elib_uri:get_params(Url),
        ?assertMatch(#{host := _, path := _, query := _}, UrlMap),
        ?assertEqual(<<"dev">>, maps:get(<<"s">>, Params)),
        ?assertEqual(<<"344af61665efff23">>, maps:get(<<"a">>, Params)),
        ?assertEqual(<<"531378">>, maps:get(<<"v">>, Params)),
        ?assertEqual(<<"375">>, maps:get(<<"width">>, Params))
    end).

get_params_no_query_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/path">>,
        {UrlMap, Params} = elib_uri:get_params(Url),
        ?assertMatch(#{host := <<"example.com">>}, UrlMap),
        ?assertEqual(#{}, Params)
    end).

%% ===================================================================
%% get_params/2 测试
%% ===================================================================

get_params_key_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/img.jpg?s=dev&a=123&width=375">>,
        Result = elib_uri:get_params(<<"width">>, Url),
        ?assertEqual(<<"375">>, Result)
    end).

get_params_key_not_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/img.jpg?s=dev&a=123">>,
        Result = elib_uri:get_params(<<"nonexistent">>, Url),
        ?assertEqual(<<>>, Result)
    end).

get_params_duplicate_keys_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/?a=1&a=2&a=3">>,
        {_UrlMap, Params} = elib_uri:get_params(Url),
        AValue = maps:get(<<"a">>, Params),
        ?assert(is_list(AValue)),
        ?assertEqual([<<"1">>, <<"2">>, <<"3">>], AValue)
    end).

%% ===================================================================
%% get_params/3 测试 (带默认值)
%% ===================================================================

get_params_with_default_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/?width=300">>,
        Result = elib_uri:get_params(<<"width">>, Url, <<"400">>),
        ?assertEqual(<<"300">>, Result)
    end).

get_params_with_default_not_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/?height=200">>,
        Result = elib_uri:get_params(<<"width">>, Url, <<"400">>),
        ?assertEqual(<<"400">>, Result)
    end).

%% ===================================================================
%% download/2 测试
%% ===================================================================

download_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        meck:new(file, [unstick, passthrough]),
        try
            Url = <<"https://example.com/image.jpg">>,
            FilePath = "./temp_test_image.jpg",
            ImageData = <<255, 254, 253, 0, 1, 2>>,

            meck:expect(httpc, request, fun(get, {_Url, []}, _, []) ->
                {ok, {{'HTTP/1.1', 200, 'OK'}, [], ImageData}}
            end),
            meck:expect(file, open, fun(_Path, _Modes) -> {ok, mock_file} end),
            meck:expect(file, write, fun(mock_file, _Data) -> ok end),
            meck:expect(file, close, fun(mock_file) -> ok end),

            Result = elib_uri:download(Url, FilePath),

            ?assertMatch({ok, FilePath}, Result),
            ?assert(meck:validate(httpc)),
            ?assert(meck:validate(file))
        after
            meck:unload(httpc),
            meck:unload(file)
        end
    end).

download_http_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"https://example.com/404.jpg">>,

            meck:expect(httpc, request, fun(get, {_Url, []}, _, []) ->
                {ok, {{'HTTP/1.1', 404, 'Not Found'}, [], <<>>}}
            end),

            Result = elib_uri:download(Url, "./temp.jpg"),

            ?assertMatch({error, 404}, Result)
        after
            meck:unload(httpc)
        end
    end).

download_network_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(httpc, [unstick, passthrough]),
        try
            Url = <<"https://unreachable.com/file.jpg">>,

            meck:expect(httpc, request, fun(get, {_Url, []}, _, []) ->
                {error, nxdomain}
            end),

            Result = elib_uri:download(Url, "./temp.jpg"),

            ?assertMatch({error, nxdomain}, Result)
        after
            meck:unload(httpc)
        end
    end).

%% ===================================================================
%% check_auth/1 测试
%% ===================================================================

check_auth_valid_url_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_dt, [passthrough, no_link]),
        meck:new(auth_ds, [passthrough, no_link]),
        try
            Url = <<"https://a.imboy.pub/img.jpg?s=dev&a=123">>,

            meck:expect(elib_dt, utc, fun(second) -> 1704067200 end),
            meck:expect(auth_ds, get_token, fun(assets, <<"dev">>, "1704067200") ->
                <<"generated_token">>
            end),

            Result = elib_uri:check_auth(Url),

            % 验证返回的URL包含参数
            ?assert(binary:match(Result, <<"s=dev">>) =/= nomatch),
            ?assert(binary:match(Result, <<"a=">>) =/= nomatch),
            ?assert(binary:match(Result, <<"v=1704067200">>) =/= nomatch)
        after
            meck:unload(elib_dt),
            meck:unload(auth_ds)
        end
    end).

%% ===================================================================
%% 内部函数测试
%% ===================================================================

query_pairs_to_map_simple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Pairs = [{"a", "1"}, {"b", "2"}],
        Result = elib_uri:query_pairs_to_map(Pairs),
        ?assertEqual(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>}, Result)
    end).

query_pairs_to_map_duplicate_test_() ->
    ?TEST_SIMPLE(fun() ->
        Pairs = [{"a", "1"}, {"a", "2"}, {"b", "3"}],
        Result = elib_uri:query_pairs_to_map(Pairs),
        ?assertEqual([<<"1">>, <<"2">>], maps:get(<<"a">>, Result)),
        ?assertEqual(<<"3">>, maps:get(<<"b">>, Result))
    end).

query_pairs_to_map_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_uri:query_pairs_to_map([]),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

build_query_with_special_chars_test_() ->
    ?TEST_SIMPLE(fun() ->
        Base = <<"https://example.com">>,
        Path = <<"api/search">>,
        Args = #{<<"q">> => <<"hello world">>, <<"filter">> => <<"a&b=c">>},
        Result = elib_uri:build_query(Base, Path, Args),
        ?assert(is_binary(Result)),
        ?assert(byte_size(Result) > 0)
    end).

exclusion_param_empty_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<"https://example.com/">>,
        Result = elib_uri:exclusion_param(Url, [<<"a">>]),
        ?assert(is_binary(Result))
    end).

get_params_empty_url_test_() ->
    ?TEST_SIMPLE(fun() ->
        Url = <<>>,
        {_UrlMap, Params} = elib_uri:get_params(Url),
        ?assertEqual(#{}, Params)
    end).
