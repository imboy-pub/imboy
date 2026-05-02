-module(elib_id_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_id 模块的 EUnit 测试
%%%
%%% 目标：验证 ID 生成功能和 TSID 字段序列化
%%% 覆盖：无前缀生成、带前缀生成（binary、integer、list）、
%%%        tsid_to_bin、tsid_keys_to_bin
%%%===================================================================

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% @doc 设置 uid mock，每次调用 uid:g/0 返回一个固定值
%% uid:encode64/1 直接返回 base64 字符串
setup_uid_mock() ->
    meck:new(uid, [no_link, non_strict]),
    meck:expect(uid, g, 0, fun() ->
        {uid, node(), {1, 2, 3}, 42}
    end),
    meck:expect(uid, encode64, 1, fun(_Uid) ->
        <<"abc123">>
    end).

setup_uid_mock_unique() ->
    meck:new(uid, [no_link, non_strict]),
    Counter = atomics:new(1, [{signed, false}]),
    meck:expect(uid, g, 0, fun() ->
        N = atomics:add(Counter, 1, 1),
        {uid, node(), {1, 2, N}, 42}
    end),
    meck:expect(uid, encode64, 1, fun({uid, _, {_, _, N}, _}) ->
        list_to_binary("enc" ++ integer_to_list(N))
    end).

cleanup_uid_mock(_State) ->
    meck:unload(uid).

%% ===================================================================
%% ID 生成测试
%% ===================================================================

gen_without_prefix_generates_binary_test_() ->
    {setup,
     fun setup_uid_mock/0,
     fun cleanup_uid_mock/1,
     fun(_) -> ?_test(fun() ->
        Result = elib_id:gen(),
        ?assert(is_binary(Result)),
        ?assert(byte_size(Result) > 0)
     end) end}.

gen_with_binary_prefix_generates_binary_with_prefix_test_() ->
    {setup,
     fun setup_uid_mock/0,
     fun cleanup_uid_mock/1,
     fun(_) -> ?_test(fun() ->
        Prefix = <<"test_">>,
        Result = elib_id:gen(Prefix),
        ?assert(is_binary(Result)),
        ?assertMatch(<<"test_", _/binary>>, Result)
     end) end}.

gen_with_integer_prefix_test_() ->
    {setup,
     fun setup_uid_mock/0,
     fun cleanup_uid_mock/1,
     fun(_) -> ?_test(fun() ->
        Prefix = 123,
        Result = elib_id:gen(Prefix),
        ?assert(is_binary(Result)),
        ?assertMatch(<<"123", _/binary>>, Result)
     end) end}.

gen_with_list_prefix_test_() ->
    {setup,
     fun setup_uid_mock/0,
     fun cleanup_uid_mock/1,
     fun(_) -> ?_test(fun() ->
        Prefix = "user_",
        Result = elib_id:gen(Prefix),
        ?assert(is_binary(Result)),
        ?assertMatch(<<"user_", _/binary>>, Result)
     end) end}.

gen_generates_unique_values_test_() ->
    {setup,
     fun setup_uid_mock_unique/0,
     fun cleanup_uid_mock/1,
     fun(_) -> ?_test(fun() ->
        Result1 = elib_id:gen(),
        Result2 = elib_id:gen(),
        ?assert(Result1 =/= Result2)
     end) end}.

gen_with_different_prefixes_test_() ->
    {setup,
     fun setup_uid_mock_unique/0,
     fun cleanup_uid_mock/1,
     fun(_) -> ?_test(fun() ->
        MsgId = elib_id:gen(<<"msg_">>),
        S2cId = elib_id:gen(<<"s2c">>),
        UserId = elib_id:gen("user_"),
        ?assert(is_binary(MsgId)),
        ?assert(is_binary(S2cId)),
        ?assert(is_binary(UserId)),
        ?assertMatch(<<"msg_", _/binary>>, MsgId),
        ?assertMatch(<<"s2c", _/binary>>, S2cId),
        ?assertMatch(<<"user_", _/binary>>, UserId),
        ?assert(MsgId =/= S2cId),
        ?assert(S2cId =/= UserId),
        ?assert(UserId =/= MsgId)
     end) end}.

%% ===================================================================
%% tsid_to_bin 测试
%% ===================================================================

tsid_to_bin_integer_test() ->
    ?assertEqual(<<"84442613760000001">>,
                 elib_id:tsid_to_bin(84442613760000001)).

tsid_to_bin_small_integer_test() ->
    ?assertEqual(<<"42">>, elib_id:tsid_to_bin(42)).

tsid_to_bin_zero_test() ->
    ?assertEqual(<<"0">>, elib_id:tsid_to_bin(0)).

tsid_to_bin_negative_integer_test() ->
    ?assertEqual(<<"-1">>, elib_id:tsid_to_bin(-1)).

tsid_to_bin_binary_passthrough_test() ->
    ?assertEqual(<<"already_bin">>, elib_id:tsid_to_bin(<<"already_bin">>)).

tsid_to_bin_atom_passthrough_test() ->
    ?assertEqual(some_atom, elib_id:tsid_to_bin(some_atom)).

tsid_to_bin_list_passthrough_test() ->
    ?assertEqual([1, 2, 3], elib_id:tsid_to_bin([1, 2, 3])).

%% ===================================================================
%% tsid_keys_to_bin 测试
%% ===================================================================

tsid_keys_to_bin_single_binary_key_test() ->
    Map = #{<<"id">> => 84442613760000001, <<"name">> => <<"alice">>},
    Result = elib_id:tsid_keys_to_bin(Map, [<<"id">>]),
    ?assertEqual(#{<<"id">> => <<"84442613760000001">>, <<"name">> => <<"alice">>}, Result).

tsid_keys_to_bin_multiple_atom_keys_test() ->
    Map = #{from_id => 84442613760000002, to_id => 84442613760000003},
    Result = elib_id:tsid_keys_to_bin(Map, [from_id, to_id]),
    ?assertEqual(#{from_id => <<"84442613760000002">>, to_id => <<"84442613760000003">>}, Result).

tsid_keys_to_bin_missing_key_noop_test() ->
    Map = #{<<"name">> => <<"alice">>},
    Result = elib_id:tsid_keys_to_bin(Map, [<<"id">>]),
    ?assertEqual(#{<<"name">> => <<"alice">>}, Result).

tsid_keys_to_bin_non_integer_value_noop_test() ->
    Map = #{<<"id">> => <<"already_string">>, <<"name">> => <<"alice">>},
    Result = elib_id:tsid_keys_to_bin(Map, [<<"id">>]),
    ?assertEqual(#{<<"id">> => <<"already_string">>, <<"name">> => <<"alice">>}, Result).

tsid_keys_to_bin_empty_keys_test() ->
    Map = #{<<"id">> => 84442613760000001},
    Result = elib_id:tsid_keys_to_bin(Map, []),
    ?assertEqual(Map, Result).

tsid_keys_to_bin_empty_map_test() ->
    Result = elib_id:tsid_keys_to_bin(#{}, [<<"id">>]),
    ?assertEqual(#{}, Result).
