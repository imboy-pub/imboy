%% Real Human JWT request through the standard middleware and database.
-module(workspace_channel_limit_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(S) ->
    C = maps:get(conn, S),
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>, <<"limit-test">>, <<"synthetic.limit">>, <<"synthetic-device-key-only">>
    ),
    Previous = application:get_env(imboy, jwt_key),
    ok = application:set_env(imboy, jwt_key, <<"synthetic-channel-limit-test-key-only">>),
    ok = intbe02_http_support:sql_exec(C, <<
        "INSERT INTO channel (id,name,creator_uid,status,scope,workspace_id,created_at,updated_at) "
        "SELECT id,'synthetic-channel-limit',995011,1,'workspace',995201,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP "
        "FROM generate_series(995980,995982) AS id"
    >>),
    try
        lists:foreach(
            fun({Limit, Expected}) ->
                R = request(S, 995001, Limit),
                ?assertEqual(200, maps:get(status, R)),
                #{<<"code">> := 0, <<"payload">> := #{<<"list">> := Rows}} = jsone:decode(
                    maps:get(body, R)
                ),
                ?assertEqual(Expected, length(Rows)),
                lists:foreach(
                    fun(Row) -> ?assertEqual(995201, maps:get(<<"workspace_id">>, Row)) end, Rows
                )
            end,
            [{<<"1">>, 1}, {<<"2">>, 2}, {<<"0">>, 1}, {<<"-9">>, 1}]
        ),
        page_checks(S),
        Denied = request(S, 995002, <<"1">>),
        ?assertEqual(403, maps:get(<<"code">>, jsone:decode(maps:get(body, Denied))))
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"DELETE FROM channel WHERE id BETWEEN 995980 AND 995983">>
        ),
        case Previous of
            {ok, Value} -> application:set_env(imboy, jwt_key, Value);
            undefined -> application:unset_env(imboy, jwt_key)
        end
    end.

request(S, Uid, Limit) ->
    Token = token_ds:encrypt_token(Uid),
    intbe02_http_support:http(
        maps:get(port, S),
        <<"GET">>,
        <<"/api/v1/workspaces/995201/channels?limit=", Limit/binary>>,
        undefined,
        maps:merge(intbe02_http_support:auth(Token), #{
            <<"cos">> => <<"synthetic">>,
            <<"vsn">> => <<"limit-test">>,
            <<"pkg">> => <<"synthetic.limit">>,
            <<"did">> => <<"synthetic-device">>,
            <<"method">> => <<"sha256">>,
            <<"sign">> => elib_hasher:hmac_sha256(
                <<"synthetic-device|limit-test|synthetic|synthetic.limit">>,
                <<"synthetic-device-key-only">>
            )
        })
    ).

page_checks(S) ->
    First = page(request(S, 995001, <<"2&paged=1">>)),
    ?assertMatch(#{<<"has_more">> := true, <<"next_cursor">> := 995981}, First),
    ?assertEqual([995982, 995981], ids(First)),
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(C, <<
        "INSERT INTO channel (id,name,creator_uid,status,scope,workspace_id,created_at,updated_at) "
        "VALUES (995983,'synthetic-later',995011,1,'workspace',995201,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)"
    >>),
    ok = intbe02_http_support:sql_exec(C, <<"DELETE FROM channel WHERE id=995981">>),
    Second = page(request(S, 995001, <<"2&paged=1&cursor=995981">>)),
    ?assertMatch(#{<<"has_more">> := false, <<"next_cursor">> := 0}, Second),
    ?assertEqual(2, length(ids(Second))),
    ?assertEqual(995980, hd(ids(Second))),
    ?assertNot(lists:member(995983, ids(Second))),
    ok = intbe02_http_support:sql_exec(C, <<"UPDATE channel SET status=0 WHERE id=995982">>),
    Archived = page(request(S, 995001, <<"200&paged=1&status=archived">>)),
    ?assertEqual([995982], ids(Archived)),
    All = page(request(S, 995001, <<"200&paged=1&status=all">>)),
    ?assertEqual(4, length(ids(All))),
    lists:foreach(
        fun(Query) ->
            R = request(S, 995001, Query),
            ?assertEqual(400, maps:get(<<"code">>, jsone:decode(maps:get(body, R))))
        end,
        [
            <<"2&paged=1&cursor=-1">>,
            <<"2&paged=1&cursor=bad">>,
            <<"2&paged=1&cursor=9223372036854775808">>,
            <<"2&paged=1&status=bad">>,
            <<"2&paged=bad">>
        ]
    ),
    page_cap_checks(S),
    Denied = request(S, 995002, <<"2&paged=1">>),
    ?assertEqual(403, maps:get(<<"code">>, jsone:decode(maps:get(body, Denied)))).

page(Response) ->
    #{<<"code">> := 0, <<"payload">> := Page} = jsone:decode(maps:get(body, Response)),
    ?assertEqual(995201, maps:get(<<"workspace_id">>, Page)),
    Page.

ids(Page) -> [maps:get(<<"id">>, R) || R <- maps:get(<<"list">>, Page)].

page_cap_checks(S) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(C, <<
        "INSERT INTO channel (id,name,creator_uid,status,scope,workspace_id,created_at,updated_at) "
        "SELECT id,'synthetic-page-cap',995011,1,'workspace',995201,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP "
        "FROM generate_series(996100,996301) AS id"
    >>),
    try
        First = page(request(S, 995001, <<"500&paged=1">>)),
        ?assertEqual(200, length(ids(First))),
        ?assertMatch(#{<<"has_more">> := true, <<"next_cursor">> := 996102}, First),
        Second = page(request(S, 995001, <<"200&paged=1&cursor=996102">>)),
        ?assertEqual(5, length(ids(Second))),
        ?assertMatch(#{<<"has_more">> := false, <<"next_cursor">> := 0}, Second),
        ?assertEqual(205, length(lists:usort(ids(First) ++ ids(Second))))
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"DELETE FROM channel WHERE id BETWEEN 996100 AND 996301">>
        )
    end.
