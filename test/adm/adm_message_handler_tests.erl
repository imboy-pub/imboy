-module(adm_message_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% adm_message_handler 模块的 EUnit 测试
%%%
%%% 目标：验证消息管理处理器的核心过滤与列表查询行为
%%%===================================================================

%% 公共行 fixture（A-01 用例与旧用例共用）
base_row() ->
    #{
        <<"scope">> => <<"c2c">>,
        <<"msg_id">> => <<"m_1">>,
        <<"from_id">> => 12,
        <<"to_id">> => 34,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<>>,
        <<"payload">> => <<"secret">>,
        <<"created_at">> => <<"2026-01-01T00:00:00Z">>,
        <<"server_ts">> => <<"2026-01-01T00:00:00Z">>
    }.

normalize_scope_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"c2c">>, adm_message_handler:normalize_scope(<<"C2C">>)),
        ?assertEqual(<<"c2g">>, adm_message_handler:normalize_scope(<<"c2g">>)),
        ?assertEqual(<<"all">>, adm_message_handler:normalize_scope(<<"unknown">>))
    end).

parse_conversation_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({12, 34, 0}, adm_message_handler:parse_conversation(<<"12:34">>)),
        ?assertEqual({12, 34, 0}, adm_message_handler:parse_conversation(<<"12_34">>)),
        ?assertEqual({0, 0, 7}, adm_message_handler:parse_conversation(<<"7">>)),
        ?assertEqual({0, 0, 0}, adm_message_handler:parse_conversation(<<"bad_input">>))
    end).

parse_conversation_accepts_legacy_tokens_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% TSID 迁移后 hash ID 不再被解析为数字，返回 {0, 0, 0}
        ?assertEqual(
            {0, 0, 0},
            adm_message_handler:parse_conversation(<<"uidhash12:uidhash34">>)
        ),
        ?assertEqual(
            {0, 0, 0},
            adm_message_handler:parse_conversation(<<"gidhash7">>)
        )
    end).

normalize_ts_test_() ->
    ?TEST_SIMPLE(fun() ->
        Ts = adm_message_handler:normalize_ts(<<"1700000000000">>),
        ?assert(is_binary(Ts)),
        ?assertNotEqual(<<>>, Ts),
        ?assertEqual(
            <<"2026-01-01T08:00:00+08:00">>,
            adm_message_handler:normalize_ts(<<"2026-01-01T08:00:00+08:00">>)
        ),
        ?assertEqual(<<>>, adm_message_handler:normalize_ts(<<"not_a_ts">>))
    end).

csv_escape_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"abc">>, adm_message_handler:csv_escape(<<"abc">>)),
        ?assertEqual(<<"\"a,b\"">>, adm_message_handler:csv_escape(<<"a,b">>)),
        ?assertEqual(<<"\"a\"\"b\"">>, adm_message_handler:csv_escape(<<"a\"b">>)),
        ?assertEqual(<<"123">>, adm_message_handler:csv_escape(123))
    end).

row_to_csv_line_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{
            scope => <<"c2c">>,
            msg_id => <<"m1">>,
            from_id => 1,
            to_id => 2,
            msg_type => <<"text">>,
            action => <<>>,
            payload => <<"{\"text\":\"hello,world\"}">>,
            created_at => <<"2026-01-01T00:00:00Z">>,
            server_ts => <<"2026-01-01T00:00:00Z">>
        },
        Line = iolist_to_binary(adm_message_handler:row_to_csv_line(Row)),
        ?assertNotEqual(
            nomatch,
            binary:match(
                Line,
                <<"c2c,m1,1,2,text,,\"{\"\"text\"\":\"\"hello,world\"\"}\",2026-01-01T00:00:00Z,2026-01-01T00:00:00Z">>
            )
        )
    end).

build_union_sql_scope_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}]}
        ],
        fun() ->
            SqlC2c = adm_message_handler:build_union_sql(#{scope => <<"c2c">>}),
            ?assertEqual(nomatch, binary:match(SqlC2c, <<"'c2g' AS scope">>)),
            ?assertNotEqual(nomatch, binary:match(SqlC2c, <<"'c2c' AS scope">>)),

            SqlAll = adm_message_handler:build_union_sql(#{scope => <<"all">>}),
            ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'c2c' AS scope">>)),
            ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'c2g' AS scope">>)),
            ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'c2s' AS scope">>)),
            ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'s2c' AS scope">>))
        end
    ).

init_list_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> metadata end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(_, _, _, _, _, _) -> ok end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(Key, _Req, Def) ->
                    case Key of
                        uid -> {ok, 12};
                        _ -> {ok, Def}
                    end
                end},
                {'binary', 3, fun(Key, _Req, Def) ->
                    case Key of
                        msg_scope -> {ok, <<"c2c">>};
                        conversation -> {ok, <<"12:34">>};
                        keyword -> {ok, <<"hello">>};
                        from_ts -> {ok, <<>>};
                        to_ts -> {ok, <<>>};
                        _ -> {ok, Def}
                    end
                end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}]},
            {elib_pg, [
                {'one', 2, fun(Sql, Params) ->
                    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Sql), <<"COUNT(*)">>)),
                    ?assertEqual(8, length(Params)),
                    {ok, #{<<"count">> => 1}}
                end},
                {'query', 2, fun(Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"LIMIT $9 OFFSET $10">>)),
                    ?assertEqual(10, length(Params)),
                    {ok, [
                        #{
                            <<"scope">> => <<"c2c">>,
                            <<"msg_id">> => <<"m_1">>,
                            <<"from_id">> => 12,
                            <<"to_id">> => 34,
                            <<"msg_type">> => <<"text">>,
                            <<"action">> => <<>>,
                            <<"payload">> => <<"{\"text\":\"hello\"}">>,
                            <<"created_at">> => <<"2026-01-01T00:00:00Z">>,
                            <<"server_ts">> => <<"2026-01-01T00:00:00Z">>
                        }
                    ]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            {ok, RespReq, _State} = adm_message_handler:init(Req, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(1, maps:get(total, Payload)),
            [First | _] = maps:get(list, Payload),
            ?assertEqual(false, maps:is_key(items, Payload)),
            ?assertEqual(<<"m_1">>, maps:get(msg_id, First))
        end
    ).

init_list_accepts_legacy_uid_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(_, _, _, _, _, _) -> ok end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(Key, _Req, Def) ->
                    case Key of
                        uid -> {ok, 0};
                        _ -> {ok, Def}
                    end
                end},
                {'binary', 3, fun(Key, _Req, Def) ->
                    case Key of
                        uid -> {ok, <<"uid_hash_99">>};
                        msg_scope -> {ok, <<"all">>};
                        conversation -> {ok, <<>>};
                        keyword -> {ok, <<>>};
                        from_ts -> {ok, <<>>};
                        to_ts -> {ok, <<>>};
                        _ -> {ok, Def}
                    end
                end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"count">> => 0}}
                end},
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, []}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            {ok, RespReq, _State} = adm_message_handler:init(Req, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(0, maps:get(total, Payload)),
            ?assertEqual([], maps:get(list, Payload)),
            ?assertEqual(false, maps:is_key(items, Payload))
        end
    ).

sanitize_row_by_audit_mode_metadata_test_() ->
    ?TEST_SIMPLE(fun() ->
        Row = #{
            scope => <<"c2c">>,
            msg_id => <<"m1">>,
            from_id => <<"u1">>,
            to_id => <<"u2">>,
            msg_type => <<"text">>,
            action => <<>>,
            payload => <<"secret">>,
            created_at => <<"2026-01-01T00:00:00Z">>,
            server_ts => <<"2026-01-01T00:00:00Z">>
        },
        Sanitized = adm_message_handler:sanitize_row_by_audit_mode(Row, metadata),
        ?assertEqual(<<>>, maps:get(payload, Sanitized))
    end).

init_list_redacts_payload_when_audit_mode_metadata_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> metadata end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(_, _, _, _, _, _) -> ok end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(_Key, _Req, Def) -> {ok, Def} end},
                {'binary', 3, fun(_Key, _Req, Def) -> {ok, Def} end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 1}} end},
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [
                        #{
                            <<"scope">> => <<"c2c">>,
                            <<"msg_id">> => <<"m_1">>,
                            <<"from_id">> => 12,
                            <<"to_id">> => 34,
                            <<"msg_type">> => <<"text">>,
                            <<"action">> => <<>>,
                            <<"payload">> => <<"secret">>,
                            <<"created_at">> => <<"2026-01-01T00:00:00Z">>,
                            <<"server_ts">> => <<"2026-01-01T00:00:00Z">>
                        }
                    ]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            [First | _] = maps:get(list, maps:get(payload, RespReq)),
            ?assertEqual(<<>>, maps:get(payload, First))
        end
    ).

init_list_disabled_when_audit_mode_none_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> none end},
                {'message_audit_enabled', 0, fun() -> false end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req, Msg, Code) ->
                    Req#{response_status => 200, error_msg => Msg, error_code => Code}
                end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_, _) -> erlang:error(should_not_be_called) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(?ERR_FEATURE_DISABLED, maps:get(error_code, RespReq)),
            ?assertEqual(<<"功能未启用"/utf8>>, maps:get(error_msg, RespReq)),
            ?assertEqual(0, meck:num_calls(elib_pg, one, 2))
        end
    ).

init_export_disabled_when_message_export_false_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'stream_reply', 3, fun(_, _, _) -> erlang:error(should_not_be_called) end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_export_enabled', 0, fun() -> false end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req, Msg, Code) ->
                    Req#{response_status => 200, error_msg => Msg, error_code => Code}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => export}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(?ERR_FEATURE_DISABLED, maps:get(error_code, RespReq)),
            ?assertEqual(<<"功能未启用"/utf8>>, maps:get(error_msg, RespReq))
        end
    ).

%% 安全回归：无 messages:read / messages:metadata:read 权限的管理员不能读取消息（曾经只有功能开关无RBAC）
init_list_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, Permission, Req) when
                    Permission =:= <<"messages:read">> orelse
                        Permission =:= <<"messages:metadata:read">>
                ->
                    {error, Req#{response_status => 403}}
                end}
            ]},
            {imboy_policy, [
                {'message_audit_enabled', 0, fun() -> erlang:error(should_not_be_called) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).

%% A-01：messages:read 被拒但持显式 messages:metadata:read → 放行且仅元数据
init_list_metadata_alias_permission_test_() ->
    Row = base_row(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun
                    (_State, <<"messages:read">>, Req) ->
                        {error, Req};
                    (_State, <<"messages:metadata:read">>, _Req) ->
                        ok;
                    (_State, <<"messages:content:read">>, _Req) ->
                        {error, should_not_check_content}
                end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> metadata end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(
                    _Uid, <<"message_list_access">>, undefined, <<"message">>, Detail, _Ip
                ) ->
                    ?assertEqual(false, maps:get(<<"content_accessed">>, Detail)),
                    ok
                end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(_Key, _Req, Def) -> {ok, Def} end},
                {'binary', 3, fun(_Key, _Req, Def) -> {ok, Def} end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 1}} end},
                {'query', 2, fun(_Sql, _Params) -> {ok, [Row]} end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            [First | _] = maps:get(list, maps:get(payload, RespReq)),
            ?assertEqual(<<>>, maps:get(payload, First))
        end
    ).

%% A-01：mode=full + content:read + 有效工单/原因 → payload 放行，审计记录 content_accessed=true
init_list_content_access_with_valid_ticket_test_() ->
    Row = base_row(),
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> full end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {report_ticket_ds, [
                {'find_by_id', 1, fun(501) -> #{<<"id">> => 501, <<"status">> => 1} end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(
                    _Uid, <<"message_list_access">>, undefined, <<"message">>, Detail, _Ip
                ) ->
                    ?assertEqual(true, maps:get(<<"content_accessed">>, Detail)),
                    ?assertEqual(501, maps:get(<<"ticket_id">>, Detail)),
                    ?assertEqual(<<"处理举报">>, maps:get(<<"reason">>, Detail)),
                    ?assertEqual(<<"full">>, maps:get(<<"effective_mode">>, Detail)),
                    ok
                end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun
                    (ticket, _Req, _Def) -> {ok, 501};
                    (_, _Req, Def) -> {ok, Def}
                end},
                {'binary', 3, fun
                    (reason, _Req, _Def) ->
                        {ok, <<"处理举报">>};
                    (_, _Req, Def) ->
                        {ok, Def}
                end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 1}} end},
                {'query', 2, fun(_Sql, _Params) -> {ok, [Row]} end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            [First | _] = maps:get(list, maps:get(payload, RespReq)),
            ?assertEqual(<<"secret">>, maps:get(payload, First))
        end
    ).

%% A-01：mode=full 但缺 messages:content:read → 静默降级 metadata，不报错不出内容
init_list_content_denied_without_content_permission_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun
                    (_State, <<"messages:read">>, _Req) ->
                        ok;
                    (_State, <<"messages:content:read">>, Req) ->
                        {error, Req}
                end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> full end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(_Uid, _Action, _Tid, _TType, Detail, _Ip) ->
                    ?assertEqual(false, maps:get(<<"content_accessed">>, Detail)),
                    ok
                end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun
                    (ticket, _Req, _Def) -> {ok, 501};
                    (_, _Req, Def) -> {ok, Def}
                end},
                {'binary', 3, fun
                    (reason, _Req, _Def) ->
                        {ok, <<"处理举报">>};
                    (_, _Req, Def) ->
                        {ok, Def}
                end}
            ]},
            {report_ticket_ds, [
                {'find_by_id', 1, fun(_) -> erlang:error(should_not_validate_ticket) end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 1}} end},
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [base_row()]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            [First | _] = maps:get(list, maps:get(payload, RespReq)),
            ?assertEqual(<<>>, maps:get(payload, First))
        end
    ).

%% A-01：带工单但缺原因 → 硬错误（不静默降级）
init_list_content_requires_reason_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> full end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun
                    (ticket, _Req, _Def) -> {ok, 501};
                    (_, _Req, Def) -> {ok, Def}
                end},
                {'binary', 3, fun(_Key, _Req, Def) -> {ok, Def} end}
            ]},
            {report_ticket_ds, [
                {'find_by_id', 1, fun(_) -> erlang:error(should_not_validate_ticket) end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) ->
                    Req#{response_status => 200, error_msg => Msg}
                end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_, _) -> erlang:error(should_not_query) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(<<"查看消息内容必须附处理原因"/utf8>>, maps:get(error_msg, RespReq)),
            ?assertEqual(0, meck:num_calls(elib_pg, one, 2))
        end
    ).

%% A-01：带工单但工单不存在 → 硬错误
init_list_content_ticket_not_found_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> full end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun
                    (ticket, _Req, _Def) -> {ok, 999};
                    (_, _Req, Def) -> {ok, Def}
                end},
                {'binary', 3, fun
                    (reason, _Req, _Def) ->
                        {ok, <<"处理举报">>};
                    (_, _Req, Def) ->
                        {ok, Def}
                end}
            ]},
            {report_ticket_ds, [
                {'find_by_id', 1, fun(999) -> {error, not_found} end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) ->
                    Req#{response_status => 200, error_msg => Msg}
                end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_, _) -> erlang:error(should_not_query) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(<<"举报工单不存在"/utf8>>, maps:get(error_msg, RespReq)),
            ?assertEqual(0, meck:num_calls(elib_pg, one, 2))
        end
    ).

%% A-01：审计写失败 → fail-closed 拒绝本次访问，不执行数据查询
init_list_audit_failure_fails_closed_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Permission, _Req) -> ok end}
            ]},
            {imboy_policy, [
                {'message_audit_mode', 0, fun() -> metadata end},
                {'message_audit_enabled', 0, fun() -> true end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(_, _, _, _, _, _) -> {error, pg_down} end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(_Key, _Req, Def) -> {ok, Def} end},
                {'binary', 3, fun(_Key, _Req, Def) -> {ok, Def} end}
            ]},
            {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
            {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
            {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
            {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"count">> => 1}} end},
                {'query', 2, fun(_, _) -> erlang:error(should_not_query_data) end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) ->
                    Req#{response_status => 200, error_msg => Msg}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => list}),
            ?assertEqual(<<"审计写入失败，访问被拒绝"/utf8>>, maps:get(error_msg, RespReq)),
            ?assertEqual(0, meck:num_calls(elib_pg, query, 2))
        end
    ).

%% A-01：导出权限独立——只有 messages:read 也拿不到导出（职责分离）
init_export_denied_without_export_permission_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'stream_reply', 3, fun(_, _, _) -> erlang:error(should_not_stream) end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, <<"messages:export">>, Req) ->
                    {error, Req#{response_status => 403}}
                end}
            ]},
            {imboy_policy, [
                {'message_export_enabled', 0, fun() -> erlang:error(should_not_check_toggle) end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req, Msg, Code) ->
                    Req#{response_status => 200, error_msg => Msg, error_code => Code}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_message_handler:init(#{}, #{action => export}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).
