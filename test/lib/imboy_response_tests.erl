-module(imboy_response_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_response 模块的 EUnit 测试
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
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(imboy_cnv, convert_at_timestamps, fun(Payload) -> Payload end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% success/2 测试
%% ===================================================================

success_with_map_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"id">> => 123, <<"name">> => <<"Test">>},
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

success_with_list_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = [#{<<"id">> => 1}, #{<<"id">> => 2}],
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

success_with_empty_map_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{},
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% success/3 测试
%% ===================================================================

success_with_payload_and_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"result">> => <<"ok">>},
            Msg = <<"Operation successful">>,
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

success_with_list_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{},
            Msg = "Success message",
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% success/4 测试
%% ===================================================================

success_with_all_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{<<"data">> => <<"test">>},
            Msg = <<"Complete success">>,
            Options = #{<<"extra">> => <<"value">>},
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload, Msg, Options),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/1 测试
%% ===================================================================

error_default_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:error(Req0),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/2 测试
%% ===================================================================

error_with_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"An error occurred">>,
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:error(Req0, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

error_with_list_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = "Error message",
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:error(Req0, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/3 测试
%% ===================================================================

error_with_msg_and_code_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"Not found">>,
            Code = 404,
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:error(Req0, Msg, Code),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% error/4 测试
%% ===================================================================

error_with_all_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<"Validation failed">>,
            Code = 422,
            Options = #{<<"details">> => [#{<<"field">> => <<"email">>}]},
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:error(Req0, Msg, Code, Options),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

%% ===================================================================
%% json_decode_field/2 测试
%% ===================================================================

json_decode_field_valid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"{\"name\":\"Alice\",\"age\":30}">>, <<"id">> => 1},
        Result = imboy_response:json_decode_field(Row, <<"payload">>),
        ?assertMatch(#{<<"payload">> := #{<<"name">> := <<"Alice">>}, <<"id">> := 1}, Result)
    end).

json_decode_field_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<>>},
        Result = imboy_response:json_decode_field(Row, <<"payload">>),
        ?assertEqual(#{<<"payload">> => <<>>}, Result)
    end).

json_decode_field_missing_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"id">> => 1},
        Result = imboy_response:json_decode_field(Row, <<"payload">>),
        ?assertEqual(#{<<"id">> => 1}, Result)
    end).

json_decode_field_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"payload">> => <<"{invalid json}">>, <<"id">> => 1},
        Result = imboy_response:json_decode_field(Row, <<"payload">>),
        % 解析失败时应该保持原样
        ?assertEqual(#{<<"payload">> => <<"{invalid json}">>, <<"id">> => 1}, Result)
    end).

json_decode_field_nested_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"data">> => <<"{\"user\":{\"name\":\"Bob\",\"id\":123},\"active\":true}">>},
        Result = imboy_response:json_decode_field(Row, <<"data">>),
        ?assertMatch(#{<<"data">> := #{<<"user">> := #{<<"name">> := <<"Bob">>}}}, Result)
    end).

json_decode_field_array_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{<<"items">> => <<"[{\"id\":1},{\"id\":2},{\"id\":3}]">>},
        Result = imboy_response:json_decode_field(Row, <<"items">>),
        ?assertMatch(#{<<"items">> := [#{<<"id">> := 1} | _]}, Result)
    end).

%% ===================================================================
%% json_decode_list_field/2 测试
%% ===================================================================

json_decode_list_field_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [],
        Result = imboy_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertEqual([], Result)
    end).

json_decode_list_field_single_row_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [#{<<"payload">> => <<"{\"name\":\"Test\"}">>}],
        Result = imboy_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"name">> := <<"Test">>}}], Result)
    end).

json_decode_list_field_multiple_rows_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [
            #{<<"payload">> => <<"{\"id\":1}">>},
            #{<<"payload">> => <<"{\"id\":2}">>},
            #{<<"payload">> => <<"{\"id\":3}">>}
        ],
        Result = imboy_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"id">> := 1}}, #{<<"payload">> := #{<<"id">> := 2}}, #{<<"payload">> := #{<<"id">> := 3}}], Result)
    end).

json_decode_list_field_mixed_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [
            #{<<"payload">> => <<"{\"id\":1}">>},
            #{<<"payload">> => <<>>},
            #{<<"payload">> => <<"{\"id\":2}">>}
        ],
        Result = imboy_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"id">> := 1}}, #{<<"payload">> := <<>>}, #{<<"payload">> := #{<<"id">> := 2}}], Result)
    end).

json_decode_list_field_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rows = [
            #{<<"payload">> => <<"{\"valid\":true}">>},
            #{<<"payload">> => <<"{invalid}">>}
        ],
        Result = imboy_response:json_decode_list_field(Rows, <<"payload">>),
        ?assertMatch([#{<<"payload">> := #{<<"valid">> := true}}, #{<<"payload">> := <<"{invalid}">>}], Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

success_with_nil_payload_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = undefined,
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

error_with_empty_msg_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Msg = <<>>,
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:error(Req0, Msg),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).

success_with_large_nested_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(cowboy_req, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        meck:new(jsone, [unstick, passthrough]),
        try
            Req0 = #{},
            Payload = #{
                <<"items">> => [#{<<"id">> => I} || I <- lists:seq(1, 100)],
                <<"metadata">> => #{<<"total">> => 100, <<"page">> => 1}
            },
            meck:expect(imboy_cnv, convert_at_timestamps, fun(P) -> P end),
            meck:expect(jsone, encode, fun(Map, _) -> Map end),
            meck:expect(cowboy_req, reply, fun(_Status, _Headers, _Body, Req) -> Req end),
            Result = imboy_response:success(Req0, Payload),
            ?assertEqual(#{}, Result),
            ?assert(meck:validate(cowboy_req)),
            ?assert(meck:validate(imboy_cnv)),
            ?assert(meck:validate(jsone))
        after
            meck:unload(cowboy_req),
            meck:unload(imboy_cnv),
            meck:unload(jsone)
        end
    end).
