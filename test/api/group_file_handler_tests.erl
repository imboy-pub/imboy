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

upload_missing_gid_returns_missing_param_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'read_part', 1, fun(_Req) ->
                {ok, [
                    {<<"file_name">>, <<"a.txt">>},
                    {<<"file">>, <<"hello">>}
                ], req_after_part}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_file_handler:handle_action(upload, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
    end).

upload_success_returns_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'read_part', 1, fun(_Req) ->
                {ok, [
                    {<<"gid">>, <<"gid_1">>},
                    {<<"file_name">>, <<"a.txt">>},
                    {<<"file_type">>, <<"text/plain">>},
                    {<<"file">>, <<"hello">>}
                ], req_after_part}
            end}
        ]},
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
    ], fun() ->
        Result = group_file_handler:handle_action(upload, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        ?assertMatch(#{<<"file_id">> := <<"f_1">>}, receive_resp_data())
    end).

download_missing_file_id_returns_missing_param_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_file_handler:handle_action(download, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
    end).

download_not_member_maps_error_code_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Result = group_file_handler:handle_action(download, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_NOT_GROUP_MEMBER, receive_resp_code())
    end).

list_missing_gid_returns_missing_param_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"page">>, <<"1">>}] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_file_handler:handle_action(list, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
    end).

list_success_with_category_filter_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Result = group_file_handler:handle_action(list, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        ?assertMatch(#{<<"total">> := 0}, receive_resp_data())
    end).

delete_missing_file_id_returns_missing_param_test_() ->
    ?WITH_MECKS([
        {elib_req, [
            {'body', 2, fun(_Req, _Opts) -> {ok, #{}, req_after_body} end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_file_handler:handle_action(delete, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
    end).

delete_permission_denied_maps_error_code_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Result = group_file_handler:handle_action(delete, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_GROUP_PERMISSION_DENIED, receive_resp_code())
    end).

search_missing_keyword_returns_missing_param_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Result = group_file_handler:handle_action(search, req_mock(), #{}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
    end).

search_success_wraps_items_test_() ->
    ?WITH_MECKS([
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
            {'search', 4, fun(_Gid, _Keyword, _Page, _Size) ->
                {ok, [#{<<"file_id">> => <<"f_2">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Result = group_file_handler:handle_action(search, req_mock(), #{}),
        ?assertEqual(req_ok, Result),
        ?assertMatch(#{<<"items">> := [#{<<"file_id">> := <<"f_2">>} ]}, receive_resp_data())
    end).

categories_missing_gid_returns_missing_param_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        Result = group_file_handler:handle_action(categories, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_MISSING_PARAM, receive_resp_code())
    end).

categories_not_member_maps_error_code_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Result = group_file_handler:handle_action(categories, req_mock(), #{current_uid => 100}),
        ?assertEqual(req_error, Result),
        ?assertEqual(?ERR_NOT_GROUP_MEMBER, receive_resp_code())
    end).

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
