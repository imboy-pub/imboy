-module(adm_message_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_message_handler 模块的 EUnit 测试
%%%
%%% 目标：验证消息管理处理器的核心过滤与列表查询行为
%%%===================================================================

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

parse_conversation_accepts_hashid_tokens_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun
                (<<"uidhash12">>) -> 12;
                (<<"uidhash34">>) -> 34;
                (<<"gidhash7">>) -> 7;
                (_) -> 0
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {12, 34, 0},
            adm_message_handler:parse_conversation(<<"uidhash12:uidhash34">>)
        ),
        ?assertEqual(
            {0, 0, 7},
            adm_message_handler:parse_conversation(<<"gidhash7">>)
        )
    end).

normalize_ts_test_() ->
    ?TEST_SIMPLE(fun() ->
        Ts = adm_message_handler:normalize_ts(<<"1700000000000">>),
        ?assert(is_binary(Ts)),
        ?assertNotEqual(<<>>, Ts),
        ?assertEqual(<<"2026-01-01T08:00:00+08:00">>, adm_message_handler:normalize_ts(<<"2026-01-01T08:00:00+08:00">>)),
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
        ?assertNotEqual(nomatch, binary:match(Line, <<"c2c,m1,1,2,text,,\"{\"\"text\"\":\"\"hello,world\"\"}\",2026-01-01T00:00:00Z,2026-01-01T00:00:00Z">>))
    end).

build_union_sql_scope_test_() ->
    ?WITH_MECKS([
        {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
        {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
        {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
        {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
        {msg_c2g_timeline_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}]}
    ], fun() ->
        SqlC2c = adm_message_handler:build_union_sql(#{scope => <<"c2c">>}),
        ?assertEqual(nomatch, binary:match(SqlC2c, <<"'c2g' AS scope">>)),
        ?assertNotEqual(nomatch, binary:match(SqlC2c, <<"'c2c' AS scope">>)),

        SqlAll = adm_message_handler:build_union_sql(#{scope => <<"all">>}),
        ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'c2c' AS scope">>)),
        ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'c2g' AS scope">>)),
        ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'c2s' AS scope">>)),
        ?assertNotEqual(nomatch, binary:match(SqlAll, <<"'s2c' AS scope">>))
    end).

init_list_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
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
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_message_handler:init(Req, #{action => list}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        [First | _] = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(<<"m_1">>, maps:get(msg_id, First))
    end).

init_list_accepts_hashid_uid_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
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
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_99">>) -> 99 end}
        ]},
        {msg_c2c_repo, [{'tablename', 0, fun() -> <<"public.msg_c2c">> end}]},
        {msg_c2g_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g">> end}]},
        {msg_c2s_repo, [{'tablename', 0, fun() -> <<"public.msg_c2s">> end}]},
        {msg_s2c_repo, [{'tablename', 0, fun() -> <<"public.msg_s2c">> end}]},
        {msg_c2g_timeline_repo, [{'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}]},
        {elib_pg, [
            {'one', 2, fun(_Sql, Params) ->
                ?assertEqual(99, hd(Params)),
                {ok, #{<<"count">> => 0}}
            end},
            {'query', 2, fun(_Sql, Params) ->
                ?assertEqual(99, hd(Params)),
                ?assertEqual(10, length(Params)),
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_message_handler:init(Req, #{action => list}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(0, maps:get(total, Payload)),
        ?assertEqual([], maps:get(list, Payload)),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).
