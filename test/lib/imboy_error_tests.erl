-module(imboy_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_error 模块的 EUnit 测试
%%%
%%% 目标：验证统一错误处理和参数验证功能
%%% 覆盖：必需参数验证、ID验证、ID列表验证、错误响应
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
        path => <<"/api/error">>,
        qs => <<>>,
        headers => #{},
        peer => {{127,0,0,1}, 12345},
        body_length => 0
    }.

%% ===================================================================
%% validate_required/2 测试 - 简单验证
%% ===================================================================

validate_required_with_valid_value_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'match_qs', 2, fun(_Pattern, _Req) ->
            #{account => <<"test_user">>}
        end}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_required(Req, account),
        ?assertMatch({ok, <<"test_user">>}, Result)
    end).

validate_required_with_missing_value_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'match_qs', 2, fun(_Pattern, _Req) ->
                #{account => undefined}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_required(Req, account),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_required_with_empty_binary_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'match_qs', 2, fun(_Pattern, _Req) ->
                #{account => <<>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_required(Req, account),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_required_with_integer_zero_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'match_qs', 2, fun(_Pattern, _Req) ->
            #{page => 0}
        end}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_required(Req, page),
        ?assertMatch({ok, 0}, Result)
    end).

%% ===================================================================
%% validate_required/3 测试 - 自定义验证函数
%% ===================================================================

validate_required_with_custom_validator_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'match_qs', 2, fun(_Pattern, _Req) ->
            #{age => 25}
        end}
    ], fun() ->
        Req = mock_request(),
        Validator = fun(Age) -> is_integer(Age) andalso Age >= 0 end,
        Result = imboy_error:validate_required(Req, age, Validator),
        ?assertMatch({ok, 25}, Result)
    end).

validate_required_custom_validator_fails_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'match_qs', 2, fun(_Pattern, _Req) ->
                #{age => -1}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Validator = fun(Age) -> is_integer(Age) andalso Age >= 0 end,
        Result = imboy_error:validate_required(Req, age, Validator),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_required_with_binary_validator_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'match_qs', 2, fun(_Pattern, _Req) ->
            #{email => <<"test@example.com">>}
        end}
    ], fun() ->
        Req = mock_request(),
        Validator = fun(Email) ->
            case Email of
                undefined -> false;
                <<>> -> false;
                _ -> true
            end
        end,
        Result = imboy_error:validate_required(Req, email, Validator),
        ?assertMatch({ok, <<"test@example.com">>}, Result)
    end).

validate_required_with_list_validator_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'match_qs', 2, fun(_Pattern, _Req) ->
            #{ids => [1, 2, 3]}
        end}
    ], fun() ->
        Req = mock_request(),
        Validator = fun(Ids) -> is_list(Ids) andalso length(Ids) > 0 end,
        Result = imboy_error:validate_required(Req, ids, Validator),
        ?assertMatch({ok, [1, 2, 3]}, Result)
    end).

%% ===================================================================
%% validate_id/2 测试 - ID 格式验证
%% ===================================================================

validate_id_with_valid_legacy_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id(Req, <<"5abc123">>),
        ?assertMatch({ok, 100}, Result)
    end).

validate_id_with_invalid_legacy_id_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id(Req, <<"invalid_id">>),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_id_with_non_binary_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id(Req, 123),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_id_with_empty_binary_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id(Req, <<>>),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_id_with_atom_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id(Req, 'invalid'),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

%% ===================================================================
%% validate_id_list/2 测试 - ID 列表验证
%% ===================================================================

validate_id_list_with_valid_ids_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id_list(Req, [<<"abc">>, <<"def">>, <<"ghi">>]),
        ?assertMatch({ok, [1, 2, 3]}, Result)
    end).

validate_id_list_with_one_invalid_id_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id_list(Req, [<<"valid">>, <<"invalid">>]),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).

validate_id_list_with_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id_list(Req, []),
        ?assertMatch({ok, []}, Result)
    end).

validate_id_list_with_single_valid_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_id_list(Req, [<<"single">>]),
        ?assertMatch({ok, [999]}, Result)
    end).

%% ===================================================================
%% missing_param/2 测试 - 缺失参数错误
%% ===================================================================

missing_param_with_atom_field_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"account 必须"/utf8>>, Msg),
                #{response_status => 400}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:missing_param(Req, account),
        ?assertMatch(#{response_status := 400}, Result)
    end).

missing_param_with_binary_field_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"字段 必须"/utf8>>, Msg),
                #{response_status => 400}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:missing_param(Req, <<"字段"/utf8>>),
        ?assertMatch(#{response_status := 400}, Result)
    end).

missing_param_with_long_field_name_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"very_long_parameter_name 必须"/utf8>>, Msg),
                #{response_status => 400}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:missing_param(Req, very_long_parameter_name),
        ?assertMatch(#{response_status := 400}, Result)
    end).

%% ===================================================================
%% invalid_id/2 测试 - 无效 ID 错误
%% ===================================================================

invalid_id_returns_error_response_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"ID 格式有误"/utf8>>, Msg),
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:invalid_id(Req, <<"invalid_hash_id">>),
        ?assertMatch(#{response_status := 400}, Result)
    end).

invalid_id_with_empty_id_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"ID 格式有误"/utf8>>, Msg),
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:invalid_id(Req, <<>>),
        ?assertMatch(#{response_status := 400}, Result)
    end).

%% ===================================================================
%% not_found/3 测试 - 资源未找到错误
%% ===================================================================

not_found_with_user_resource_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"用户 不存在: 123abc"/utf8>>, Msg),
                #{response_status => 404}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:not_found(Req, <<"用户"/utf8>>, <<"123abc">>),
        ?assertMatch(#{response_status := 404}, Result)
    end).

not_found_with_group_resource_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"群组 不存在: group456"/utf8>>, Msg),
                #{response_status => 404}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:not_found(Req, <<"群组"/utf8>>, <<"group456">>),
        ?assertMatch(#{response_status := 404}, Result)
    end).

not_found_with_message_resource_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ?assertEqual(<<"消息 不存在: msg789"/utf8>>, Msg),
                #{response_status => 404}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:not_found(Req, <<"消息"/utf8>>, <<"msg789">>),
        ?assertMatch(#{response_status := 404}, Result)
    end).

not_found_with_long_id_test_() ->
    ?WITH_MECKS([
        {elib_response, [
            {'error', 2, fun(_Req, Msg) ->
                ExpectedMsg = <<"资源 不存在: very_long_id_string_here"/utf8>>,
                ?assertEqual(ExpectedMsg, Msg),
                #{response_status => 404}
            end}
        ]},
        {elib_cnv, [
            {'safe_to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:not_found(Req, <<"资源"/utf8>>, <<"very_long_id_string_here">>),
        ?assertMatch(#{response_status := 404}, Result)
    end).

%% ===================================================================
%% 综合场景测试
%% ===================================================================

validate_multiple_params_successfully_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'match_qs', 2, fun(_Pattern, _Req) ->
            #{account => <<"user">>, page => 1}
        end}
    ], fun() ->
        Req = mock_request(),
        ?assertMatch({ok, <<"user">>}, imboy_error:validate_required(Req, account)),
        ?assertMatch({ok, 100}, imboy_error:validate_id(Req, <<"gid123">>))
    end).

validate_param_chain_fails_on_first_error_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'match_qs', 2, fun(_Pattern, _Req) ->
                #{account => undefined}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Msg) ->
                #{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Result = imboy_error:validate_required(Req, account),
        ?assertMatch({error, #{response_status := 400}}, Result)
    end).
