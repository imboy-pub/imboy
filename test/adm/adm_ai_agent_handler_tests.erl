-module(adm_ai_agent_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc adm_ai_agent_handler 新端点 EUnit 测试
%%% 覆盖：roles（GET 全量 / POST save|delete 幂等保存删除）、
%%%       upload_avatar（multipart → Garage → URL 回显）。
%%% 权限链 mock：adm_acl:ensure_permission 直接放行（权限常量测试
%%% 见 adm_acl_tests）。
%%%===================================================================

mock_req() ->
    #{method => <<"GET">>, path => <<"/api/adm/ai_agent">>}.

with_perm_ok() ->
    [{adm_acl, [{'ensure_permission', 3, fun(_State, _Perm, _Req) -> ok end}]}].

response_ok() ->
    [
        {elib_response, [
            {'success', 2, fun(Req, Data) -> Req#{response_status => 200, data => Data} end},
            {'success', 3, fun(Req, Data, _Msg) -> Req#{response_status => 200, data => Data} end},
            {'error', 3, fun(Req, _Msg, Code) -> Req#{response_status => Code} end}
        ]}
    ].

%% ===================================================================
%% roles — GET 全量 / POST save|delete
%% ===================================================================

roles_get_returns_full_map_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {ai_agent_ds, [
                    {'roles', 0, fun() -> #{<<"doctor">> => <<"你是医生"/utf8>>} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{<<"roles">> => #{<<"doctor">> => <<"你是医生"/utf8>>}},
                maps:get(data, RespReq)
            )
        end
    ).

roles_save_calls_ds_and_returns_roles_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{
                            <<"action">> => <<"save">>,
                            <<"role_id">> => <<"lawyer">>,
                            <<"prompt">> => <<"你是律师"/utf8>>
                        }
                    end}
                ]},
                {ai_agent_ds, [
                    {'save_role', 2, fun(<<"lawyer">>, <<"你是律师"/utf8>>) -> ok end},
                    {'roles', 0, fun() ->
                        #{<<"lawyer">> => <<"你是律师"/utf8>>}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            %% 保存后回显全量角色
            ?assertEqual(
                #{<<"roles">> => #{<<"lawyer">> => <<"你是律师"/utf8>>}},
                maps:get(data, RespReq)
            )
        end
    ).

roles_delete_calls_ds_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{<<"action">> => <<"delete">>, <<"role_id">> => <<"doctor">>}
                    end}
                ]},
                {ai_agent_ds, [
                    {'delete_role', 1, fun(<<"doctor">>) -> ok end},
                    {'roles', 0, fun() -> #{} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

roles_save_without_prompt_rejected_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{<<"action">> => <<"save">>, <<"role_id">> => <<"doctor">>}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(?ERR_BAD_REQUEST, maps:get(response_status, RespReq))
        end
    ).

roles_invalid_action_rejected_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) -> #{<<"action">> => <<"rename">>} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(?ERR_BAD_REQUEST, maps:get(response_status, RespReq))
        end
    ).

%% ===================================================================
%% upload_avatar — multipart → Garage → URL
%% ===================================================================

upload_avatar_success_returns_url_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [
                    {'method', 1, fun(_) -> <<"POST">> end},
                    {'read_part', 1, fun(Req) ->
                        case get(read_part_calls) of
                            undefined ->
                                put(read_part_calls, 1),
                                {ok, #{}, Req};
                            _ ->
                                {done, Req}
                        end
                    end},
                    {'read_part_body', 1, fun(Req) -> {ok, <<"PNGDATA">>, Req} end}
                ]},
                {cow_multipart, [
                    {'form_data', 1, fun(_) ->
                        {file, <<"file">>, <<"avatar.png">>, <<"image/png">>}
                    end}
                ]},
                {elib_oss, [
                    {'upload', 3, fun(
                        <<"PNGDATA">>, <<"avatar.png">>, #{mime_type := <<"image/png">>}
                    ) ->
                        {ok, <<"https://s3.example.com/avatar.png">>, <<"file123">>}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => upload_avatar, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{<<"url">> => <<"https://s3.example.com/avatar.png">>},
                maps:get(data, RespReq)
            )
        end
    ).

upload_avatar_rejects_oversize_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [
                    {'method', 1, fun(_) -> <<"POST">> end},
                    {'read_part', 1, fun(Req) ->
                        case get(read_part_calls) of
                            undefined ->
                                put(read_part_calls, 1),
                                {ok, #{}, Req};
                            _ ->
                                {done, Req}
                        end
                    end},
                    {'read_part_body', 1, fun(Req) -> {ok, <<"BIG">>, Req} end}
                ]},
                {cow_multipart, [
                    {'form_data', 1, fun(_) ->
                        {file, <<"file">>, <<"big.bin">>, <<"application/octet-stream">>}
                    end}
                ]},
                {elib_oss, [
                    {'upload', 3, fun(_, _, _) -> {error, file_too_large} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => upload_avatar, adm_user_id => 1}),
            ?assertEqual(?ERR_FILE_SIZE_EXCEEDED, maps:get(response_status, RespReq))
        end
    ).

upload_avatar_missing_file_rejected_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [
                    {'method', 1, fun(_) -> <<"POST">> end},
                    {'read_part', 1, fun(Req) -> {done, Req} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => upload_avatar, adm_user_id => 1}),
            ?assertEqual(?ERR_MISSING_PARAM, maps:get(response_status, RespReq))
        end
    ).

%% ===================================================================
%% list — category 筛选透传
%% ===================================================================

list_category_filter_passed_to_ds_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {elib_param, [
                    {'page', 1, fun(_) -> {1, 10} end},
                    {'get', 3, fun(category, _Req, _Def) -> <<"medical">> end}
                ]},
                {ai_agent_ds, [
                    {'list', 3, fun(Page, Size, Category) ->
                        {ok, #{
                            total => 0, page => Page, size => Size, list => [], category => Category
                        }}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => list, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{total => 0, page => 1, size => 10, list => [], category => <<"medical">>},
                maps:get(data, RespReq)
            )
        end
    ).

list_without_category_passes_empty_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {elib_param, [
                    {'page', 1, fun(_) -> {1, 10} end},
                    {'get', 3, fun(category, _Req, _Def) -> <<>> end}
                ]},
                {ai_agent_ds, [
                    {'list', 3, fun(Page, Size, <<>>) ->
                        {ok, #{total => 0, page => Page, size => Size, list => []}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => list, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

role_list_passes_keyword_and_status_filters_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {elib_param, [
                    {'page', 1, fun(_) -> {2, 20} end},
                    {'get', 3, fun
                        (keyword, _Req, _Def) -> <<"doctor">>;
                        (status, _Req, _Def) -> <<"1">>
                    end}
                ]},
                {ai_agent_role_ds, [
                    {'page', 3, fun(2, 20, #{keyword := <<"doctor">>, status := 1}) ->
                        {ok, #{total => 1, page => 2, size => 20, list => []}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => role_list, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(1, maps:get(total, maps:get(data, RespReq)))
        end
    ).

role_publish_passes_operator_and_version_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{
                            <<"role_code">> => <<"doctor">>,
                            <<"version">> => <<"3">>,
                            <<"admin_uid">> => <<"99">>
                        }
                    end}
                ]},
                {ai_agent_role_ds, [
                    {'publish', 3, fun(<<"doctor">>, 3, 1) -> {ok, #{published => true}} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => role_publish, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{published => true},
                maps:get(data, RespReq)
            )
        end
    ).
