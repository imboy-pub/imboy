-module(adm_logout_apply_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

normalize_ts_test_() ->
    [
        ?_assertEqual(<<>>, adm_logout_apply_handler:normalize_ts(<<>>)),
        ?_assertMatch(<<_/binary>>, adm_logout_apply_handler:normalize_ts(<<"1700000000000">>)),
        ?_assertEqual(
            <<"2026-02-20T10:00:00+08:00">>,
            adm_logout_apply_handler:normalize_ts(<<"2026-02-20T10:00:00+08:00">>)
        ),
        ?_assertEqual(<<>>, adm_logout_apply_handler:normalize_ts(<<"bad">>))
    ].

build_where_sql_test_() ->
    [
        ?_test(
            begin
                {Sql, Params} = adm_logout_apply_handler:build_where_sql(#{
                    uid => 12,
                    keyword_like => <<"%tom%">>,
                    from_ts => <<"2026-02-01T00:00:00+08:00">>,
                    to_ts => <<"2026-02-28T23:59:59+08:00">>
                }),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"l.uid = $1">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"u.account ILIKE $2">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"created_at >= $3::timestamptz">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"created_at <= $4::timestamptz">>)),
                ?assertEqual(
                    [
                        12,
                        <<"%tom%">>,
                        <<"2026-02-01T00:00:00+08:00">>,
                        <<"2026-02-28T23:59:59+08:00">>
                    ],
                    Params
                )
            end
        ),
        ?_test(
            begin
                {Sql, Params} = adm_logout_apply_handler:build_where_sql(#{
                    uid => 0,
                    keyword_like => <<>>,
                    from_ts => <<>>,
                    to_ts => <<>>
                }),
                ?assertEqual(<<>>, Sql),
                ?assertEqual([], Params)
            end
        )
    ].

csv_escape_test_() ->
    [
        ?_assertEqual(<<"abc">>, adm_logout_apply_handler:csv_escape(<<"abc">>)),
        ?_assertEqual(<<"\"a,b\"">>, adm_logout_apply_handler:csv_escape(<<"a,b">>)),
        ?_assertEqual(<<"\"a\"\"b\"">>, adm_logout_apply_handler:csv_escape(<<"a\"b">>)),
        ?_assertEqual(<<"12">>, adm_logout_apply_handler:csv_escape(12))
    ].

row_to_csv_line_test_() ->
    ?_test(
        begin
            Line = iolist_to_binary(
                adm_logout_apply_handler:row_to_csv_line(#{
                    uid => 1,
                    account => <<"tom">>,
                    nickname => <<"Tom">>,
                    app_vsn => <<"0.7.1">>,
                    did => <<"dev-1">>,
                    dtype => <<"android">>,
                    ip => <<"127.0.0.1">>,
                    created_at => <<"2026-02-20T10:00:00+08:00">>,
                    body => <<"{\"k\":\"v\"}">>
                })
            ),
            ?assertNotEqual(nomatch, binary:match(Line, <<"tom">>)),
            ?assertNotEqual(nomatch, binary:match(Line, <<"android">>))
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
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 10} end},
                {'int', 3, fun(Key, _Req, Def) ->
                    case Key of
                        uid -> {ok, 0};
                        _ -> {ok, Def}
                    end
                end},
                {'binary', 3, fun(Key, _Req, Def) ->
                    case Key of
                        uid -> {ok, <<"uid_hash_88">>};
                        keyword -> {ok, <<>>};
                        from_ts -> {ok, <<>>};
                        to_ts -> {ok, <<>>};
                        _ -> {ok, Def}
                    end
                end}
            ]},
            {user_log_repo, [
                {'tablename', 0, fun() -> <<"public.user_log">> end}
            ]},
            {user_repo, [
                {'tablename', 0, fun() -> <<"public.users">> end}
            ]},
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
            {ok, RespReq, _State} = adm_logout_apply_handler:init(Req, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(0, maps:get(total, Payload)),
            ?assertEqual([], maps:get(list, Payload)),
            ?assertEqual(false, maps:is_key(items, Payload))
        end
    ).

%% 安全回归：无 logout_applications:approve 权限的管理员不能批准注销（曾经零权限校验，等价删号）
init_approve_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"POST">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, <<"logout_applications:approve">>, Req) ->
                    {error, Req#{response_status => 403}}
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            {ok, RespReq, _State} = adm_logout_apply_handler:init(Req, #{action => approve}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).

%% 安全回归：无 logout_applications:approve 权限的管理员不能驳回注销
init_reject_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"POST">> end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, <<"logout_applications:approve">>, Req) ->
                    {error, Req#{response_status => 403}}
                end}
            ]}
        ],
        fun() ->
            Req = #{},
            {ok, RespReq, _State} = adm_logout_apply_handler:init(Req, #{action => reject}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).
