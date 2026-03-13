-module(elib_response_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_response 模块的 EUnit 测试
%%%
%%% 目标：验证响应工具功能
%%% 覆盖：success/error 系列函数、JSON 字段解析、响应格式
%%%===================================================================

%% ===================================================================
%% success/1 测试
%% ===================================================================

success_default_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(elib_cnv, convert_at_timestamps, fun(Payload) -> Payload end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% success/2 测试
%% ===================================================================

success_with_map_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"id">> => 123, <<"name">> => <<"Test">>},
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

success_with_list_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = [#{<<"id">> => 1}, #{<<"id">> => 2}],
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

success_with_empty_map_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{},
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% success/3 测试
%% ===================================================================

success_with_payload_and_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"result">> => <<"ok">>},
            Msg = <<"Operation successful">>,
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

success_with_list_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{},
            Msg = "Success message",
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% success/4 测试
%% ===================================================================

success_with_all_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"data">> => <<"test">>},
            Msg = <<"Complete success">>,
            Options = #{<<"extra">> => <<"value">>},
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload, Msg, Options),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/1 测试
%% ===================================================================

error_default_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:error(Req0),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/2 测试
%% ===================================================================

error_with_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"An error occurred">>,
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:error(Req0, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

error_with_list_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = "Error message",
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:error(Req0, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/3 测试
%% ===================================================================

error_with_msg_and_code_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"Not found">>,
            Code = 404,
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:error(Req0, Msg, Code),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/4 测试
%% ===================================================================

error_with_all_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"Validation failed">>,
            Code = 422,
            Options = #{<<"details">> => [#{<<"field">> => <<"email">>}]},
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:error(Req0, Msg, Code, Options),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% json_decode_field/2 测试
%% ===================================================================

json_decode_field_valid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"{\"name\":\"Alice\",\"age\":30}">>, <<"id">> => 1},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        ?assertMatch(#{<<"payload">> := #{<<"name">> := <<"Alice">>}, <<"id">> := 1}, Result)
    end).

json_decode_field_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<>>},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        ?assertEqual(#{<<"payload">> => <<>>}, Result)
    end).

json_decode_field_missing_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"id">> => 1},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        ?assertEqual(#{<<"id">> => 1}, Result)
    end).

json_decode_field_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"{invalid json}">>, <<"id">> => 1},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        % 解析失败时应该保持原样
        ?assertEqual(#{<<"payload">> => <<"{invalid json}">>, <<"id">> => 1}, Result)
    end).

json_decode_field_nested_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"data">> => <<"{\"user\":{\"name\":\"Bob\",\"id\":123},\"active\":true}">>},
        Result = elib_response:json_decode_field(Row, <<"data">>),
        ?assertMatch(#{<<"data">> := #{<<"user">> := #{<<"name">> := <<"Bob">>}}}, Result)
    end).

json_decode_field_array_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"items">> => <<"[{\"id\":1},{\"id\":2},{\"id\":3}]">>},
        Result = elib_response:json_decode_field(Row, <<"items">>),
        ?assertMatch(#{<<"items">> := [#{<<"id">> := 1} | _]}, Result)
    end).

%% ===================================================================
%% json_decode_list_field/2 测试
%% ===================================================================

json_decode_list_field_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [],
        Result = elib_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertEqual([], Result)
    end).

json_decode_list_field_single_row_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [#{<<"payload">> => <<"{\"name\":\"Test\"}">>}],
        Result = elib_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"name">> := <<"Test">>}}], Result)
    end).

json_decode_list_field_multiple_rows_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [
            #{<<"payload">> => <<"{\"id\":1}">>},
            #{<<"payload">> => <<"{\"id\":2}">>},
            #{<<"payload">> => <<"{\"id\":3}">>}
        ],
        Result = elib_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"id">> := 1}}, #{<<"payload">> := #{<<"id">> := 2}}, #{<<"payload">> := #{<<"id">> := 3}}], Result)
    end).

json_decode_list_field_mixed_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [
            #{<<"payload">> => <<"{\"id\":1}">>},
            #{<<"payload">> => <<>>},
            #{<<"payload">> => <<"{\"id\":2}">>}
        ],
        Result = elib_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"id">> := 1}}, #{<<"payload">> := <<>>}, #{<<"payload">> := #{<<"id">> := 2}}], Result)
    end).

json_decode_list_field_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [
            #{<<"payload">> => <<"{\"valid\":true}">>},
            #{<<"payload">> => <<"{invalid}">>}
        ],
        Result = elib_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"valid">> := true}}, #{<<"payload">> := <<"{invalid}">>}], Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

success_with_nil_payload_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = undefined,
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

error_with_empty_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<>>,
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:error(Req0, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

success_with_large_nested_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{
                <<"items">> => [#{<<"id">> => I} || I <- lists:seq(1, 100)],
                <<"metadata">> => #{<<"total">> => 100, <<"page">> => 1}
            },
            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = elib_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% handle_logic_result/2 测试
%% ===================================================================

handle_logic_result_ok_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Data = #{<<"id">> => 123, <<"name">> => <<"test">>},

            meck:expect(elib_response, success, fun(Req, Payload) ->
                ?assertEqual(Data, Payload),
                Req
            end),

            Result = elib_response:handle_logic_result(Req0, {ok, Data}),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

handle_logic_result_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            ErrorMsg = <<"Not found">>,

            meck:expect(elib_response, error, fun(Req, Msg) ->
                ?assertEqual(ErrorMsg, Msg),
                Req
            end),

            Result = elib_response:handle_logic_result(Req0, {error, ErrorMsg}),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

handle_logic_result_error_list_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            ErrorMsg = "error message",

            meck:expect(elib_response, error, fun(Req, Msg) ->
                ?assertEqual(ErrorMsg, Msg),
                Req
            end),

            Result = elib_response:handle_logic_result(Req0, {error, ErrorMsg}),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% handle_logic_result_with/4 测试
%% ===================================================================

handle_logic_result_with_ok_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Data = #{<<"id">> => 1},
            Options = #{<<"extra">> => <<"value">>},

            meck:expect(elib_response, success, fun(Req, EnrichedData, Msg, Opts) ->
                ?assertEqual(#{<<"id">> => 1, <<"enriched">> => true}, EnrichedData),
                ?assertEqual(<<"success.">>, Msg),
                ?assertEqual(Options, Opts),
                Req
            end),

            EnrichFun = fun(D) -> D#{<<"enriched">> => true} end,
            Result = elib_response:handle_logic_result_with(Req0, {ok, Data}, EnrichFun, Options),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

handle_logic_result_with_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            ErrorMsg = <<"validation failed">>,
            Options = #{},

            meck:expect(elib_response, error, fun(Req, Msg) ->
                ?assertEqual(ErrorMsg, Msg),
                Req
            end),

            EnrichFun = fun(D) -> D end,
            Result = elib_response:handle_logic_result_with(Req0, {error, ErrorMsg}, EnrichFun, Options),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% error_with_code/2 测试
%% ===================================================================

error_with_code_only_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            ErrorCode = 1001,

            meck:expect(elib_response, error, fun(Req, Msg, Code) ->
                ?assertEqual(<<"操作失败"/utf8>>, Msg),
                ?assertEqual(ErrorCode, Code),
                Req
            end),

            Result = elib_response:error_with_code(Req0, ErrorCode),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% error_with_code/3 测试
%% ===================================================================

error_with_code_and_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"Custom error"/utf8>>,
            ErrorCode = 2001,

            meck:expect(elib_response, error, fun(Req, M, Code) ->
                ?assertEqual(Msg, M),
                ?assertEqual(ErrorCode, Code),
                Req
            end),

            Result = elib_response:error_with_code(Req0, Msg, ErrorCode),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

error_with_code_list_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_response, [passthrough, no_link]),
        meck:new(cowboy_req, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = "List error message",
            ErrorCode = 3001,

            meck:expect(elib_response, error, fun(Req, M, Code) ->
                ?assertEqual(Msg, M),
                ?assertEqual(ErrorCode, Code),
                Req
            end),

            Result = elib_response:error_with_code(Req0, Msg, ErrorCode),

            ?assertEqual(#{}, Result)
        after
            meck:unload(elib_response),
            meck:unload(cowboy_req)
        end
    end).

%% ===================================================================
%% is_potential_json/1 内部函数测试（间接测试）
%% ===================================================================

json_decode_field_non_json_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"plain text not json">>},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        % 不是 JSON 格式应该保持原样
        ?assertEqual(#{<<"payload">> => <<"plain text not json">>}, Result)
    end).

json_decode_field_too_large_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 超过 100KB 的数据
        LargeBinary = binary:copy(<<"x">>, 1024 * 101),
        Row = #{<<"payload">> => LargeBinary},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        % 太大的数据应该保持原样
        ?assertEqual(#{<<"payload">> => LargeBinary}, Result)
    end).

json_decode_field_with_leading_spaces_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"  {\"test\": true}">>},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        ?assertMatch(#{<<"payload">> := #{<<"test">> := true}}, Result)
    end).

json_decode_field_with_leading_newline_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"\n{\"test\": true}">>},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        ?assertMatch(#{<<"payload">> := #{<<"test">> := true}}, Result)
    end).

json_decode_field_array_json_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"[1, 2, 3]">>},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        ?assertMatch(#{<<"payload">> := [1, 2, 3]}, Result)
    end).

%% ===================================================================
%% preview_binary/2 内部函数测试（间接测试）
%% ===================================================================

json_decode_field_logs_preview_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试超过100字节的二进制数据会记录预览
        LargeJson = <<"{\"data\":\"", (binary:copy(<<"x">>, 200))/binary, "\"}">>,
        Row = #{<<"payload">> => LargeJson},
        Result = elib_response:json_decode_field(Row, <<"payload">>),
        % 即使很大，如果是有效的 JSON 也应该解析
        ?assertMatch(#{<<"payload">> := #{<<"data">> := _}}, Result)
    end).

%% ===================================================================
%% json_decode_list_field 非列表输入测试
%% ===================================================================

json_decode_list_field_non_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{<<"payload">> => <<"{\"test\": true}">>},
        Result = elib_response:json_decode_list_field(Input, <<"payload">>),
        % 非列表输入应该直接返回
        ?assertEqual(Input, Result)
    end).

%% ===================================================================
%% reply_json 内部函数测试（间接测试）
%% ===================================================================

success_sets_response_headers_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        meck:new(elib_dt, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"data">> => <<"test">>},

            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),
            meck:expect(jsone, encode, fun(Map, _Opts) ->
                ?assert(maps:is_key(<<"code">>, Map)),
                ?assert(maps:is_key(<<"msg">>, Map)),
                ?assert(maps:is_key(<<"sv_ts">>, Map)),
                ?assert(maps:is_key(<<"payload">>, Map)),
                ?assertEqual(false, maps:is_key(<<"data">>, Map)),
                Map
            end),
            meck:expect(cowboy_req, reply, fun(_Status, Headers, _Body, Req) ->
                HeadersMap = case Headers of
                    Map when is_map(Map) -> Map;
                    List when is_list(List) -> maps:from_list(List)
                end,
                ?assertMatch(#{<<"content-type">> := <<"application/json; charset=utf-8">>},
                             HeadersMap),
                ?assertMatch(#{<<"Referrer-Policy">> := <<"strict-origin-when-cross-origin">>},
                             HeadersMap),
                Req
            end),

            Result = elib_response:success(Req0, Payload),

            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(elib_cnv)),
            ?assert(meck:validate(jsone)),
            ?assert(meck:validate(elib_dt))
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone),
            meck:unload(elib_dt)
        end
    end).

error_with_options_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        meck:new(elib_dt, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"error">>,
            Code = 500,
            Options = #{<<"debug">> => <<"info">>},

            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),
            meck:expect(jsone, encode, fun(Map, _Opts) ->
                ?assert(maps:is_key(<<"debug">>, Map)),
                ?assertEqual(#{}, maps:get(<<"payload">>, Map)),
                ?assertEqual(false, maps:is_key(<<"data">>, Map)),
                Map
            end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),

            Result = elib_response:error(Req0, Msg, Code, Options),

            ?assertEqual(#{}, Result)
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone),
            meck:unload(elib_dt)
        end
    end).

success_with_options_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        meck:new(elib_dt, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"data">> => 1},
            Msg = <<"ok"/utf8>>,
            Options = #{<<"count">> => 100},

            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),
            meck:expect(jsone, encode, fun(Map, _Opts) ->
                ?assertEqual(100, maps:get(<<"count">>, Map)),
                ?assertEqual(false, maps:is_key(<<"data">>, Map)),
                Map
            end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),

            Result = elib_response:success(Req0, Payload, Msg, Options),

            ?assertEqual(#{}, Result)
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone),
            meck:unload(elib_dt)
        end
    end).

reply_json_options_not_map_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        meck:new(elib_dt, [unstick, passthrough]),
        try
            Req0 = #{},
            Options = [],

            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),
            meck:expect(jsone, encode, fun(Map, _Opts) ->
                ?assertMatch(#{<<"code">> := 0}, Map),
                ?assertEqual(false, maps:is_key(<<"data">>, Map)),
                Map
            end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),

            Result = elib_response:success(Req0, #{}, <<"ok">>, Options),

            ?assertEqual(#{}, Result)
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone),
            meck:unload(elib_dt)
        end
    end).

options_cannot_override_core_envelope_fields_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(elib_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        meck:new(elib_dt, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"id">> => 1},
            Msg = <<"ok">>,
            Options = #{
                <<"code">> => 999,
                <<"msg">> => <<"override">>,
                <<"sv_ts">> => 1,
                <<"payload">> => #{<<"id">> => 999},
                <<"extra">> => <<"keep">>
            },

            meck:expect(elib_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(elib_dt, millisecond, fun() -> 1640995200000 end),
            meck:expect(jsone, encode, fun(Map, _Opts) ->
                ?assertEqual(0, maps:get(<<"code">>, Map)),
                ?assertEqual(<<"ok">>, maps:get(<<"msg">>, Map)),
                ?assertEqual(1640995200000, maps:get(<<"sv_ts">>, Map)),
                ?assertEqual(Payload, maps:get(<<"payload">>, Map)),
                ?assertEqual(false, maps:is_key(<<"data">>, Map)),
                ?assertEqual(<<"keep">>, maps:get(<<"extra">>, Map)),
                Map
            end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),

            Result = elib_response:success(Req0, Payload, Msg, Options),

            ?assertEqual(#{}, Result)
        after
            meck:unload(cowboy_req),
            meck:unload(elib_cnv),
            meck:unload(jsone),
            meck:unload(elib_dt)
        end
    end).
