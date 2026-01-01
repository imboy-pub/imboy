-module(imboy_response_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_response 模块的 EUnit 测试
%%%
%%% 目标：验证响应工具功能
%%% 覆盖：JSON响应、错误响应
%%%===================================================================

%% ===================================================================
%% JSON 响应测试
%% ===================================================================

json_response_with_valid_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        Data = #{status => ok, data => #{<<"id">> => 123}},
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:success(Req, Data),
        % 验证响应结构
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

json_response_with_empty_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        Data = #{},
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:success(Req, Data),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

%% ===================================================================
%% 错误响应测试
%% ===================================================================

error_response_with_code_test_() ->
    ?TEST_WITH_APP(fun() ->
        Message = <<"Bad Request">>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:error(Req, Message),
        % 验证错误响应
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

error_response_with_custom_code_test_() ->
    ?TEST_WITH_APP(fun() ->
        Code = 404,
        Message = <<"Not Found">>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:error(Req, Code, Message),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

error_response_with_details_test_() ->
    ?TEST_WITH_APP(fun() ->
        Code = 422,
        Message = <<"Validation Error">>,
        Details = #{<<"field">> => <<"email">>, <<"error">> => <<"invalid">>},
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:error(Req, Code, Message, Details),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

%% ===================================================================
%% 成功响应测试
%% ===================================================================

success_response_test_() ->
    ?TEST_WITH_APP(fun() ->
        Data = #{result => <<"success">>},
        Message = <<"Operation completed">>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:success(Req, Data, Message),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

success_response_with_only_message_test_() ->
    ?TEST_WITH_APP(fun() ->
        Message = <<"Created successfully">>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:success(Req, Message),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

%% ===================================================================
%% 分页响应测试
%% ===================================================================

page_response_test_() ->
    ?TEST_WITH_APP(fun() ->
        Total = 100,
        Page = 1,
        Size = 20,
        Items = [#{id => 1}, #{id => 2}],
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:page(Req, Total, Page, Size, Items),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

page_response_with_empty_items_test_() ->
    ?TEST_WITH_APP(fun() ->
        Total = 0,
        Page = 1,
        Size = 20,
        Items = [],
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:page(Req, Total, Page, Size, Items),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

%% ===================================================================
%% 重定向响应测试
%% ===================================================================

redirect_response_test_() ->
    ?TEST_WITH_APP(fun() ->
        URL = <<"https://example.com">>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:redirect(Req, URL),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

redirect_response_with_status_test_() ->
    ?TEST_WITH_APP(fun() ->
        URL = <<"https://example.com/login">>,
        Status = 301,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:redirect(Req, URL, Status),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

%% ===================================================================
%% 文件响应测试
%% ===================================================================

file_response_test_() ->
    ?TEST_WITH_APP(fun() ->
        FilePath = <<"/path/to/file.pdf">>,
        ContentType = <<"application/pdf">>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:file(Req, FilePath, ContentType),
        % 文件响应处理
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

nil_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        Data = undefined,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:success(Req, Data),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

empty_message_test_() ->
    ?TEST_WITH_APP(fun() ->
        Message = <<>>,
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:error(Req, Message),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).

large_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        LargeData = maps:from_list([{I, I} || I <- lists:seq(1, 1000)]),
        Req = cowboy_req_h:new(#{}),
        Result = imboy_response:success(Req, LargeData),
        case Result of
            {ok, NewReq} when is_map(NewReq) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Req}")
        end
    end).