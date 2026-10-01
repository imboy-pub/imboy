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
        Denied = request(S, 995002, <<"1">>),
        ?assertEqual(403, maps:get(<<"code">>, jsone:decode(maps:get(body, Denied))))
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"DELETE FROM channel WHERE id BETWEEN 995980 AND 995982">>
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
