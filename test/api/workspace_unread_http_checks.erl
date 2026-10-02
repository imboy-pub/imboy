-module(workspace_unread_http_checks).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).

workspace_unread_test_() -> {timeout, 180, fun run/0}.

run() ->
    S = ?H:setup_all(),
    Saved = [{K, application:get_env(imboy, K)} || K <- [jwt_key, api_auth_switch]],
    Key = app_version_ds:sign_key(<<"synthetic">>, <<"orgdir-test">>, <<"synthetic.orgdir">>),
    try
        application:set_env(imboy, api_auth_switch, <<"on">>),
        application:set_env(imboy, jwt_key, <<"orgdir_http_test_jwt_key_0123456789">>),
        ok = app_version_ds:set_sign_key(
            <<"synthetic">>,
            <<"orgdir-test">>,
            <<"synthetic.orgdir">>,
            <<"synthetic-orgdir-device-key-only">>
        ),
        check(S)
    after
        ok = app_version_ds:set_sign_key(
            <<"synthetic">>, <<"orgdir-test">>, <<"synthetic.orgdir">>, Key
        ),
        lists:foreach(fun restore/1, Saved),
        ?H:teardown_all(S),
        inttest_marker_db:release(S)
    end.

check(#{conn := C} = S) ->
    G1 = group(S, 995201),
    G2 = group(S, 995202),
    message(C, G1, <<"unread-one">>, 1, 995011),
    message(C, G1, <<"unread-two">>, 2, 995011),
    message(C, G1, <<"unread-own">>, 3, 995012),
    message(C, G2, <<"unread-other">>, 1, 995011),
    message(C, G1, <<"unread-expired">>, 4, 995011),
    message(C, G1, <<"unread-revoked">>, 5, 995011),
    ok = ?H:sql_exec(
        C,
        <<"UPDATE msg_c2g SET expire_at=NOW()-INTERVAL '1 second' WHERE msg_id='unread-expired'">>
    ),
    ok = ?H:sql_exec(
        C,
        <<"UPDATE msg_c2g SET payload='{\"action\":\"message_revoke_ack\"}' WHERE msg_id='unread-revoked'">>
    ),
    ?assertEqual(2, unread(S, 995201, G1)),
    ?assertEqual(1, unread(S, 995202, G2)),
    ?assertEqual(0, code(read(S, 995201, G1, [<<"unread-one">>]))),
    ?assertEqual(1, unread(S, 995201, G1)),
    ?assertEqual(1, unread(S, 995202, G2)),
    lists:foreach(
        fun(Ids) ->
            ?assertEqual(404, code(read(S, 995201, G1, Ids))),
            ?assertEqual(1, unread(S, 995201, G1))
        end,
        [
            [<<"unread-other">>],
            [<<"unknown">>],
            [<<"unread-expired">>],
            [<<"unread-revoked">>],
            [<<"unread-two">>, <<"unread-revoked">>]
        ]
    ),
    ?assertEqual(404, code(read(S, 995201, G2, [<<"unread-one">>]))),
    ?assertEqual(0, code(read(S, 995201, G1, [<<"unread-two">>, <<"unread-two">>]))),
    ?assertEqual(0, unread(S, 995201, G1)),
    ?assertEqual(0, code(read(S, 995201, G1, [<<"unread-one">>]))),
    ?assertEqual(0, unread(S, 995201, G1)),
    generation(C, G1),
    message(C, G1, <<"unread-new-generation">>, 10, 995011),
    ?assertEqual(1, unread(S, 995201, G1)),
    ?assertEqual(404, code(read(S, 995201, G1, [<<"unread-two">>]))),
    revoke(S, G1),
    ?assertEqual(400, code(read(S, 995201, G1, []))),
    ok.

group(S, Ws) ->
    Headers = maps:merge(
        ?H:auth(maps:get(cred_a, S)), ?H:idem(integer_to_binary(erlang:unique_integer([positive])))
    ),
    R = ?H:http(
        maps:get(port, S),
        <<"POST">>,
        <<"/api/internal/v1/groups">>,
        #{
            <<"workspace_id">> => Ws,
            <<"title">> => <<"unread fixture">>,
            <<"members">> => [<<"intbe02-ext-h2">>]
        },
        Headers
    ),
    ?assertEqual(200, status(R)),
    maps:get(<<"group_id">>, jsone:decode(maps:get(body, R))).

message(C, Gid, Id, Seq, From) ->
    {ok, _} = enterprise_message_repo:insert_group_tx(C, Id, From, Gid, <<"text">>, #{
        <<"content">> => <<"synthetic unread">>
    }),
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO msg_c2g_timeline(msg_id,to_uid,to_gid,created_at,conv_seq,client_ack) SELECT msg_id,995012,to_id,created_at,$2,true FROM msg_c2g WHERE msg_id=$1">>,
        [Id, Seq]
    ).

generation(C, Gid) ->
    ok = ?H:sql_exec(
        C,
        <<"UPDATE group_member_generation SET end_seq=9 WHERE group_id=$1 AND user_id=995012 AND end_seq IS NULL">>,
        [Gid]
    ),
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO group_member_generation(group_id,user_id,generation_no,start_seq) VALUES($1,995012,2,10)">>,
        [Gid]
    ).

revoke(#{conn := C} = S, Gid) ->
    ok = ?H:sql_exec(
        C,
        <<"UPDATE organization_member SET status='removed' WHERE organization_id=995101 AND user_id=995012">>
    ),
    try
        ?assertEqual([], rows(S, 995201)),
        ?assertEqual(404, code(read(S, 995201, Gid, [<<"unread-new-generation">>])))
    after
        ok = ?H:sql_exec(
            C,
            <<"UPDATE organization_member SET status='active' WHERE organization_id=995101 AND user_id=995012">>
        )
    end.

unread(S, Ws, Gid) ->
    [R] = [R || R <- rows(S, Ws), maps:get(<<"id">>, R) =:= Gid],
    maps:get(<<"unread_count">>, R).

rows(S, Ws) ->
    Path =
        <<"/api/v1/workspaces/", (integer_to_binary(Ws))/binary,
            "/groups?member_only=1&preview=1">>,
    R = ?H:http(
        maps:get(port, S), <<"GET">>, Path, <<>>, organization_directory_http_support:bearer(995012)
    ),
    ?assertEqual(200, status(R)),
    maps:get(<<"list">>, maps:get(<<"payload">>, jsone:decode(maps:get(body, R)))).

read(S, _Ws, Gid, Ids) ->
    Path =
        <<"/api/v1/groups/", (integer_to_binary(Gid))/binary, "/read">>,
    ?H:http(
        maps:get(port, S),
        <<"POST">>,
        Path,
        #{<<"msg_ids">> => Ids},
        organization_directory_http_support:bearer(995012)
    ).

code(R) ->
    ?assertEqual(200, maps:get(status, R)),
    maps:get(<<"code">>, jsone:decode(maps:get(body, R))).

status(R) -> maps:get(status, R).
restore({K, undefined}) -> application:unset_env(imboy, K);
restore({K, {ok, V}}) -> application:set_env(imboy, K, V).
