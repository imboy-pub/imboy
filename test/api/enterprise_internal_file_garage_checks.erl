-module(enterprise_internal_file_garage_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    {ok, _} = application:ensure_all_started(inets),
    S = intbe02_http_support:setup_all(),
    try
        application:set_env(imboy, garage, #{
            endpoint => env("IMBOY_TEST_ENDPOINT"),
            bucket => <<"synthetic-enterprise-assets">>,
            region => <<"garage">>,
            access_key => env("IMBOY_TEST_ACCESS"),
            secret_key => env("IMBOY_TEST_SECRET")
        }),
        journey(S),
        io:format("INTERNAL_FILE_REAL_GARAGE_RESULT=ok~n")
    after
        application:unset_env(imboy, garage),
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end.

journey(S) ->
    C = maps:get(conn, S),
    Auth = intbe02_http_support:auth(maps:get(cred_a, S)),
    App = maps:get(app_a, S),
    Before = count(
        C,
        <<"SELECT count(*)::int AS n FROM enterprise_audit_event WHERE action='file.confirmed'">>,
        []
    ),
    Bytes = <<"synthetic real Internal file bytes">>,
    {Presign, Key} = presign_and_missing(S, C, Auth, Before),
    Body = #{<<"object_key">> => Key},
    real_upload(Presign, Key, Bytes),
    check_foreign(S, C, Auth, Key, Bytes, Before),
    Confirmed = confirm(S, C, Auth, Body, Key, Bytes),
    check_row(C, App, Key, Confirmed, Bytes),
    replay(S, C, Auth, Body, Key, Confirmed, Before).

presign_and_missing(S, C, Auth, Before) ->
    App = maps:get(app_a, S),
    Presign = ok_json(
        post(
            S,
            <<"/api/internal/v1/files/presign">>,
            #{
                <<"file_name">> => <<"real-internal.txt">>,
                <<"mime_type">> => <<"text/plain">>,
                <<"size_bytes">> => 1
            },
            Auth,
            <<"real-file-presign">>
        )
    ),
    Key = maps:get(<<"object_key">>, Presign),
    ?assert(enterprise_asset_repo:owned_key(Key, 995101, App)),
    Body = #{<<"object_key">> => Key},
    Missing = post(S, <<"/api/internal/v1/files/confirm">>, Body, Auth, <<"real-file-missing">>),
    error_json(Missing, <<"invalid_request">>),
    ?assertEqual(0, attachment_count(C, Key)),
    ?assertEqual(1, pending_count(C, Key)),
    ?assertEqual(
        Before,
        count(
            C,
            <<"SELECT count(*)::int AS n FROM enterprise_audit_event WHERE action='file.confirmed'">>,
            []
        )
    ),
    io:format("INTERNAL_FILE_MISSING_OBJECT_ROLLBACK=PASS~n"),
    {Presign, Key}.

real_upload(Presign, Key, Bytes) ->
    Url = binary_to_list(maps:get(<<"put_url">>, Presign)),
    {ok, {{_, PutStatus, _}, _, _}} = httpc:request(
        put,
        {Url, [], "text/plain", Bytes},
        [{timeout, 10000}],
        [{body_format, binary}]
    ),
    ?assert(PutStatus =:= 200 orelse PutStatus =:= 201 orelse PutStatus =:= 204),
    {ok, #{size := Size, content_type := Type}} = elib_oss:head_object(
        elib_oss:get_bucket(<<"enterprise">>), Key
    ),
    ?assertEqual(byte_size(Bytes), Size),
    ?assertEqual(<<"text/plain">>, Type),
    io:format("INTERNAL_FILE_PRESIGN_REAL_PUT_HEAD=PASS~n").

foreign_object(C, Bytes) ->
    {ok, ForeignApp} = enterprise_application_repo:create_tx(
        C,
        995102,
        <<"real-file-foreign-app">>,
        <<"synthetic foreign storage app">>,
        {null, [<<"files:write">>]}
    ),
    Foreign = enterprise_asset_repo:build_object_key(
        995102, maps:get(<<"id">>, ForeignApp), <<"foreign.txt">>
    ),
    Bucket = elib_oss:get_bucket(<<"enterprise">>),
    {ok, inserted} = enterprise_asset_repo:pending_add_tx(C, Foreign, Bucket, <<"enterprise">>),
    ForeignUrl = elib_oss:presign_put_for_key(Bucket, Foreign, <<"text/plain">>, 60),
    {ok, {{_, ForeignPutStatus, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(ForeignUrl), [], "text/plain", Bytes},
        [{timeout, 10000}],
        [{body_format, binary}]
    ),
    ?assert(
        ForeignPutStatus =:= 200 orelse ForeignPutStatus =:= 201 orelse ForeignPutStatus =:= 204
    ),
    {ok, #{size := ForeignSize}} = elib_oss:head_object(Bucket, Foreign),
    ?assertEqual(byte_size(Bytes), ForeignSize),
    {Bucket, Foreign}.

check_foreign(S, C, Auth, Key, Bytes, Before) ->
    Body = #{<<"object_key">> => Key},
    {Bucket, Foreign} = foreign_object(C, Bytes),
    error_json(
        post(
            S,
            <<"/api/internal/v1/files/confirm">>,
            #{<<"object_key">> => Foreign},
            Auth,
            <<"real-file-foreign">>
        ),
        <<"invalid_request">>
    ),
    ?assertEqual(0, attachment_count(C, Foreign)),
    ?assertEqual(1, pending_count(C, Foreign)),
    ?assertEqual(
        Before,
        count(
            C,
            <<"SELECT count(*)::int AS n FROM enterprise_audit_event WHERE action='file.confirmed'">>,
            []
        )
    ),
    ok = elib_oss:delete_object(Bucket, Foreign),
    error_json(
        post(
            S,
            <<"/api/internal/v1/files/confirm">>,
            Body,
            intbe02_http_support:auth(maps:get(cred_ro, S)),
            <<"real-file-scope">>
        ),
        <<"insufficient_scope">>
    ),
    ?assertEqual(0, attachment_count(C, Key)),
    io:format("INTERNAL_FILE_FOREIGN_KEY_SCOPE_DENIED=PASS~n").

confirm(S, C, Auth, Body, Key, Bytes) ->
    Confirmed = ok_json(
        post(S, <<"/api/internal/v1/files/confirm">>, Body, Auth, <<"real-file-confirm">>)
    ),
    ?assertEqual(byte_size(Bytes), maps:get(<<"size">>, Confirmed)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"mime_type">>, Confirmed)),
    ?assertEqual(Key, maps:get(<<"object_key">>, Confirmed)),
    ?assertEqual(0, pending_count(C, Key)),
    ?assertEqual(1, attachment_count(C, Key)),
    Confirmed.

check_row(C, App, Key, Confirmed, Bytes) ->
    Row = intbe02_http_support:one(
        C,
        <<"SELECT id,size,mime_type,scope,scope_ref,info,cipher IS NULL AS clear FROM attachment WHERE path=$1">>,
        [Key]
    ),
    ?assertEqual(maps:get(<<"file_id">>, Confirmed), maps:get(<<"id">>, Row)),
    ?assertEqual(byte_size(Bytes), maps:get(<<"size">>, Row)),
    ?assertEqual(<<"enterprise">>, maps:get(<<"scope">>, Row)),
    ?assertEqual(enterprise_asset_repo:scope_ref(995101, App), maps:get(<<"scope_ref">>, Row)),
    ?assertEqual(true, maps:get(<<"clear">>, Row)),
    Info =
        case maps:get(<<"info">>, Row) of
            V when is_map(V) -> V;
            V -> jsone:decode(V)
        end,
    ?assertEqual(App, maps:get(<<"origin_application_id">>, Info)),
    io:format("INTERNAL_FILE_CONFIRMED_ACTUAL_SIZE_OWNERSHIP=PASS~n").

replay(S, C, Auth, Body, Key, Confirmed, Before) ->
    Replay = ok_json(
        post(S, <<"/api/internal/v1/files/confirm">>, Body, Auth, <<"real-file-confirm">>)
    ),
    ?assertEqual(Confirmed, Replay),
    ?assertEqual(1, attachment_count(C, Key)),
    ?assertEqual(
        Before + 1,
        count(
            C,
            <<"SELECT count(*)::int AS n FROM enterprise_audit_event WHERE action='file.confirmed'">>,
            []
        )
    ),
    io:format("INTERNAL_FILE_IDEMPOTENT_REPLAY_SINGLE_ROW_AUDIT=PASS~n"),
    ok = elib_oss:delete_object(elib_oss:get_bucket(<<"enterprise">>), Key).

post(S, Path, Body, Auth, Idem) ->
    intbe02_http_support:http(
        maps:get(port, S),
        <<"POST">>,
        Path,
        Body,
        maps:merge(Auth, intbe02_http_support:idem(Idem))
    ).
ok_json(Resp) ->
    ?assertEqual(200, maps:get(status, Resp)),
    jsone:decode(maps:get(body, Resp)).
error_json(Resp, Code) ->
    ?assertEqual(enterprise_internal_error:http_status(Code), maps:get(status, Resp)),
    #{<<"error">> := #{<<"code">> := Actual}} = jsone:decode(maps:get(body, Resp)),
    ?assertEqual(Code, Actual).
attachment_count(C, Key) ->
    count(C, <<"SELECT count(*)::int AS n FROM attachment WHERE path=$1">>, [Key]).
pending_count(C, Key) ->
    count(C, <<"SELECT count(*)::int AS n FROM attach_pending WHERE object_key=$1">>, [Key]).
count(C, Sql, Params) -> maps:get(<<"n">>, intbe02_http_support:one(C, Sql, Params)).
env(Name) -> list_to_binary(os:getenv(Name)).
