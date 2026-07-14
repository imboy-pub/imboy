-module(group_file_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% group_file_handler 模块的 EUnit 测试
%%%
%%% 覆盖 action:
%%% - upload/download/list/delete/search/categories
%%%===================================================================

%% 模拟 cowboy 流式 multipart 协议：read_part 每次返回一个 part 的
%% headers map，read_part_body 返回对应 body，读完返回 {done, Req}。
%% 旧 mock 把 parts 列表塞进单次 read_part 返回值，固化了错误契约，
%% 正是生产 function_clause 崩溃（2026-07-14）漏网的原因。
multipart_stream_mocks(PartSpecs) ->
    % PartSpecs :: [{HeadersMap, BodyBinary}]
    % atomics 计数器跨进程可见（mock fun 在 eunit 工作进程执行，进程字典不通）。
    Ctr = atomics:new(2, []),
    ReadPart = fun(_Req) ->
        N = atomics:add_get(Ctr, 1, 1) - 1,
        case N < length(PartSpecs) of
            true ->
                {Headers, _Body} = lists:nth(N + 1, PartSpecs),
                {ok, Headers, req_headers};
            false ->
                {done, req_done}
        end
    end,
    ReadBody = fun(_Req) ->
        N = atomics:add_get(Ctr, 2, 1) - 1,
        {_Headers, Body} = lists:nth(N + 1, PartSpecs),
        {ok, Body, req_body}
    end,
    [{'read_part', 1, ReadPart}, {'read_part_body', 1, ReadBody}].

upload_missing_gid_returns_missing_param_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req,
                multipart_stream_mocks([
                    {
                        #{
                            <<"content-disposition">> =>
                                <<"form-data; name=\"file\"; filename=\"a.txt\"">>,
                            <<"content-type">> => <<"text/plain">>
                        },
                        <<"hello">>
                    }
                ])},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(upload, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
        end
    ).

upload_success_returns_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req,
                multipart_stream_mocks([
                    {#{<<"content-disposition">> => <<"form-data; name=\"gid\"">>}, <<"gid_1">>},
                    {
                        #{<<"content-disposition">> => <<"form-data; name=\"file_name\"">>},
                        <<"a.txt">>
                    },
                    {
                        #{<<"content-disposition">> => <<"form-data; name=\"file_type\"">>},
                        <<"text/plain">>
                    },
                    {
                        #{
                            <<"content-disposition">> =>
                                <<"form-data; name=\"file\"; filename=\"a.txt\"">>,
                            <<"content-type">> => <<"text/plain">>
                        },
                        <<"hello">>
                    }
                ])},
            {group_file_logic, [
                {'upload', 5, fun(_Gid, _Uid, _FileName, _FileBinary, _FileType) ->
                    {ok, #{<<"file_id">> => <<"f_1">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(upload, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            ?assertMatch(#{<<"file_id">> := <<"f_1">>}, receive_resp_data())
        end
    ).

download_missing_file_id_returns_missing_param_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(download, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
        end
    ).

download_not_member_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"file_id">>, <<"12">>}] end}
            ]},
            {group_file_logic, [
                {'download', 2, fun(_FileId, _Uid) -> {error, not_member} end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(download, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_NOT_GROUP_MEMBER, receive_resp_code())
        end
    ).

list_missing_gid_returns_missing_param_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"page">>, <<"1">>}] end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(list, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
        end
    ).

list_success_with_category_filter_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"gid">>, <<"gid_2">>},
                        {<<"page">>, <<"2">>},
                        {<<"size">>, <<"10">>},
                        {<<"category">>, <<"document">>}
                    ]
                end}
            ]},
            {group_file_logic, [
                {'list', 5, fun(_Gid, _Uid, _Page, _Size, _Opt) ->
                    {ok, #{<<"items">> => [], <<"total">> => 0}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(list, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            ?assertMatch(#{<<"total">> := 0}, receive_resp_data())
        end
    ).

delete_missing_file_id_returns_missing_param_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) -> {ok, #{}, req_after_body} end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(delete, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
        end
    ).

delete_permission_denied_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) -> {ok, #{<<"file_id">> => 33}, req_after_body} end}
            ]},
            {group_file_logic, [
                {'delete', 2, fun(_FileId, _Uid) -> {error, permission_denied} end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(delete, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_GROUP_PERMISSION_DENIED, receive_resp_code())
        end
    ).

search_missing_keyword_returns_missing_param_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"gid">>, <<"gid_3">>}]
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(search, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
        end
    ).

search_success_wraps_items_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"gid">>, <<"gid_3">>},
                        {<<"keyword">>, <<"spec">>},
                        {<<"page">>, <<"1">>},
                        {<<"size">>, <<"20">>}
                    ]
                end}
            ]},
            {group_file_logic, [
                {'search', 5, fun(_Gid, _Keyword, _Page, _Size, _CurrentUid) ->
                    {ok, [#{<<"file_id">> => <<"f_2">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(search, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            ?assertMatch(#{<<"items">> := [#{<<"file_id">> := <<"f_2">>}]}, receive_resp_data())
        end
    ).

categories_missing_gid_returns_missing_param_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(categories, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
        end
    ).

categories_not_member_maps_error_code_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"gid">>, <<"gid_5">>}] end}
            ]},
            {group_file_logic, [
                {'get_categories', 2, fun(_Gid, _Uid) -> {error, not_member} end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Result = group_file_handler:handle_action(categories, req_mock(), #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(?ERR_NOT_GROUP_MEMBER, receive_resp_code())
        end
    ).

req_mock() ->
    #{mock_req => true}.

receive_resp_code() ->
    receive
        {resp_code, Code} -> Code
    after 1000 ->
        timeout
    end.

receive_resp_data() ->
    receive
        {resp_data, Data} -> Data
    after 1000 ->
        timeout
    end.
