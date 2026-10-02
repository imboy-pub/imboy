%% Real HTTP message authorization against disposable PostgreSQL.
-module(enterprise_message_authorization_http_checks).
-export([run/0, run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).

message_authorization_test_() -> {timeout, 180, fun run/0}.

run() ->
    S = ?H:setup_all(),
    try
        run(S)
    after
        ?H:teardown_all(S),
        inttest_marker_db:release(S)
    end.

run(#{conn := C} = S) ->
    R = send(S, <<"/api/internal/v1/groups">>, #{
        <<"workspace_id">> => 995201,
        <<"title">> => <<"sender boundary fixture">>,
        <<"members">> => [<<"intbe02-ext-h1">>, <<"intbe02-ext-h2">>]
    }),
    ?assertEqual(200, maps:get(status, R)),
    Gid = maps:get(<<"group_id">>, jsone:decode(maps:get(body, R))),
    Group = <<"/api/internal/v1/groups/", (integer_to_binary(Gid))/binary, "/messages">>,
    Files = files(C, S),
    Principal = ?H:one(C, <<"SELECT account_type FROM \"user\" WHERE id=995014">>),
    ok = ?H:sql_exec(C, <<"UPDATE \"user\" SET account_type=1 WHERE id=995014">>),
    try
        lists:foreach(
            fun(Path) ->
                lists:foreach(
                    fun(Mode) ->
                        Body = body(Mode),
                        sent(S, Path, Body),
                        inactive_sender(S, Path, Mode, Body),
                        file_boundaries(S, Path, Body, Files)
                    end,
                    [<<"application">>, <<"human">>]
                )
            end,
            [<<"/api/internal/v1/messages/direct">>, Group]
        )
    after
        ok = ?H:sql_exec(
            C,
            <<"UPDATE \"user\" SET account_type=$1 WHERE id=995014">>,
            [maps:get(<<"account_type">>, Principal)]
        )
    end,
    io:format("MESSAGE_AUTHORIZATION_HTTP=PASS active=4 inactive=4 files=12~n"),
    ok.

inactive_sender(#{conn := C} = S, Path, Mode, Body) ->
    {Uid, Status, Code} =
        case Mode of
            <<"application">> -> {995014, 400, <<"invalid_request">>};
            <<"human">> -> {995011, 422, <<"identity_not_mapped">>}
        end,
    ok = ?H:sql_exec(C, <<"UPDATE \"user\" SET status=0 WHERE id=$1">>, [Uid]),
    try
        deny(S, Path, Body, Status, Code)
    after
        ok = ?H:sql_exec(C, <<"UPDATE \"user\" SET status=1 WHERE id=$1">>, [Uid])
    end.

file_boundaries(S, Path, Body, [Own, OtherApp, OtherOrg]) ->
    File = maps:remove(<<"content">>, Body#{<<"msg_type">> => <<"file">>}),
    sent(S, Path, File#{<<"object_key">> => Own}),
    lists:foreach(
        fun(Key) ->
            deny(S, Path, File#{<<"object_key">> => Key}, 404, <<"resource_not_found">>)
        end,
        [OtherApp, OtherOrg]
    ).

sent(#{conn := C} = S, Path, Body) ->
    R = send(S, Path, Body),
    ?assertEqual(200, maps:get(status, R)),
    Result = jsone:decode(maps:get(body, R)),
    MsgId = maps:get(<<"msg_id">>, Result),
    Mode = maps:get(<<"sender_mode">>, Body),
    Uid =
        case Mode of
            <<"human">> -> 995011;
            _ -> 995014
        end,
    ?assertEqual(Mode, maps:get(<<"sender_kind">>, Result)),
    Table =
        case Path of
            <<"/api/internal/v1/messages/direct">> -> <<"msg_c2c">>;
            _ -> <<"msg_c2g">>
        end,
    Row = ?H:one(
        C,
        [
            <<"SELECT from_id,payload::text FROM ">>,
            Table,
            <<" WHERE msg_id=$1">>
        ],
        [MsgId]
    ),
    ?assertEqual(Uid, maps:get(<<"from_id">>, Row)),
    Payload = jsone:decode(maps:get(<<"payload">>, Row)),
    assert_origin(C, S, MsgId, Mode, Uid),
    assert_payload(C, Body, Payload).

assert_origin(C, S, MsgId, Mode, Uid) ->
    Origin = ?H:one(
        C,
        <<"SELECT sender_kind,sender_user_id,application_id FROM enterprise_message_origin WHERE msg_id=$1">>,
        [MsgId]
    ),
    ?assertEqual(Mode, maps:get(<<"sender_kind">>, Origin)),
    OriginUid =
        case Mode of
            <<"human">> -> Uid;
            _ -> null
        end,
    ?assertEqual(OriginUid, maps:get(<<"sender_user_id">>, Origin)),
    ?assertEqual(maps:get(app_a, S), maps:get(<<"application_id">>, Origin)).

assert_payload(C, Body, Payload) ->
    case maps:find(<<"object_key">>, Body) of
        {ok, Key} ->
            File = maps:get(<<"file">>, Payload),
            ?assertEqual(Key, maps:get(<<"object_key">>, File)),
            Att = ?H:one(C, <<"SELECT id FROM attachment WHERE path=$1">>, [Key]),
            ?assertEqual(maps:get(<<"id">>, Att), maps:get(<<"file_id">>, File));
        error ->
            ?assertEqual(maps:get(<<"content">>, Body), maps:get(<<"content">>, Payload))
    end.

deny(#{conn := C} = S, Path, Body, Status, Code) ->
    Before = snapshot(C),
    R = send(S, Path, Body),
    ?assertEqual(Status, maps:get(status, R)),
    ?assertEqual(
        Code, maps:get(<<"code">>, maps:get(<<"error">>, jsone:decode(maps:get(body, R))))
    ),
    ?assertEqual(Before, snapshot(C)).

snapshot(C) ->
    [
        maps:get(
            <<"n">>,
            ?H:one(C, [
                <<"SELECT COALESCE(jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text), '[]'::jsonb)::text AS n FROM ">>,
                Table,
                <<" t">>
            ])
        )
     || Table <- [
            <<"msg_c2c">>,
            <<"msg_c2g">>,
            <<"enterprise_message_origin">>,
            <<"enterprise_audit_event">>,
            <<"attachment">>,
            <<"enterprise_application_usage">>
        ]
    ].

body(Mode) ->
    Base = #{
        <<"sender_mode">> => Mode,
        <<"recipient_user_id">> => <<"intbe02-ext-h2">>,
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"synthetic authorization message">>
    },
    case Mode of
        <<"human">> -> Base#{<<"sender_user_id">> => <<"intbe02-ext-h1">>};
        _ -> Base
    end.

send(S, Path, Body) ->
    Key = <<"sender-boundary-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Headers = maps:merge(?H:auth(maps:get(cred_a, S)), ?H:idem(Key)),
    ?H:http(maps:get(port, S), <<"POST">>, Path, Body, Headers).

%% Confirmed metadata fixtures: object bytes are outside the sending ACL boundary.
files(C, S) ->
    {ok, Foreign} = enterprise_application_repo:create_tx(
        C,
        995102,
        <<"sender-boundary-foreign">>,
        <<"synthetic foreign application">>,
        {995021, [<<"files:upload">>]}
    ),
    [
        confirmed(C, Org, App)
     || {Org, App} <- [
            {995101, maps:get(app_a, S)},
            {995101, maps:get(app_c, S)},
            {995102, maps:get(<<"id">>, Foreign)}
        ]
    ].

confirmed(C, Org, App) ->
    Ref = enterprise_asset_repo:scope_ref(Org, App),
    Key =
        <<"eoa/", (integer_to_binary(Org))/binary, "/", (integer_to_binary(App))/binary,
            "/20261002/synthetic/fixture.txt">>,
    {ok, _} = enterprise_asset_repo:confirm_save_tx(C, Key, <<"text/plain">>, 3, #{
        scope_ref => Ref, <<"origin_application_id">> => App
    }),
    {ok, _} = enterprise_asset_repo:find_confirmed_tx(C, Org, App, Key),
    Key.
