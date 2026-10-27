-module(timezone_acceptance_pg_fixture).
-export([run/0, run/1, response_payload/3]).

run() ->
    run([
        review_queue_repo_tests,
        {generator, fun msg_safe_boundary_pg_tests:r02_5b_backfill_restores_full_history_test_/0},
        {generator, fun msg_safe_boundary_pg_tests:r02_5c_backfill_advances_empty_windows_test_/0},
        {generator, fun msg_safe_boundary_pg_tests:r02_accepted_at_response_rfc3339_pg_test_/0},
        {generator, fun msg_safe_boundary_pg_tests:r02_legacy_accepted_at_response_null_pg_test_/0}
    ]).

run(Tests) ->
    application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 3,
        max_count => 4,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => "127.0.0.1",
                    username => "timezone_test",
                    password => os:getenv("IMBOY_TIMEZONE_PG_PASSWORD"),
                    database => "timezone_acceptance",
                    port => 59306,
                    timeout => 2000,
                    codecs => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]}
    }),
    try
        {ok, #{<<"db">> := <<"timezone_acceptance">>, <<"role">> := <<"timezone_test">>}} =
            elib_pg:one(<<"SELECT current_database() AS db, current_user AS role">>, []),
        {ok, #{<<"version">> := PgVersion}} = elib_pg:one(
            <<"SELECT current_setting('server_version_num') AS version">>, []
        ),
        18 = binary_to_integer(PgVersion) div 10000,
        io:format("PG_VERSION_NUM=~s CODEC=epgsql_codec_rfc3339_bin~n", [PgVersion]),
        create_schema(),
        ok = meck:new(eunit_runner, [non_strict, no_link]),
        ok = meck:expect(eunit_runner, eunit_setup_with_db, fun() ->
            Conn = pooler:take_member(pgsql),
            true = is_pid(Conn),
            {ok, Conn}
        end),
        ok = meck:expect(eunit_runner, eunit_cleanup_db, fun(Conn) ->
            pooler:return_member(pgsql, Conn, ok)
        end),
        Result = eunit:test(Tests, [verbose]),
        io:format("R12_R02_PG_RESULT=~p IMBOY_STARTED=~p~n", [
            Result,
            lists:keymember(imboy, 1, application:which_applications())
        ]),
        Result
    after
        meck:unload(eunit_runner),
        pooler:rm_pool(pgsql),
        application:stop(pooler),
        application:stop(epgsql)
    end.

response_payload(Handler, Query, State) ->
    StreamId = make_ref(),
    RequestPid = self(),
    Req = #{pid => RequestPid, streamid => StreamId, method => <<"GET">>, qs => Query},
    erlang:erase({elib_param, qs_vals}),
    try
        {ok, _, _} = Handler:init(Req, State),
        receive
            {{RequestPid, StreamId}, {response, 200, Headers, Body}} ->
                #{<<"content-type">> := <<"application/json; charset=utf-8">>} = Headers,
                #{<<"code">> := 0, <<"payload">> := Payload} = jsone:decode(Body),
                Payload
        after 1000 ->
            erlang:error(response_not_emitted)
        end
    after
        erlang:erase({elib_param, qs_vals})
    end.

create_schema() ->
    Statements = [
        <<
            "CREATE TABLE IF NOT EXISTS public.review_queue (id bigint PRIMARY KEY,"
            " msg_id text, msg_type text, content text, from_id bigint, from_account text,"
            " to_id bigint, to_type text, hit_words text[], review_status text, reviewer_id bigint,"
            " reason text, reviewed_at timestamptz, created_at timestamptz, metadata jsonb)"
        >>,
        <<"CREATE INDEX IF NOT EXISTS timezone_review_created_at ON public.review_queue(created_at)">>,
        <<
            "CREATE TABLE IF NOT EXISTS public.msg_store (id bigint PRIMARY KEY, msg_id text NOT NULL,"
            " chat_type text, conv_key text, conv_seq bigint, msg_type text, from_id bigint,"
            " to_id bigint, group_id bigint, e2ee jsonb, sender_did text, payload jsonb,"
            " created_at timestamptz, server_ts timestamptz, accepted_at timestamptz,"
            " UNIQUE(msg_id, created_at), UNIQUE(conv_key, conv_seq))"
        >>,
        <<
            "CREATE TABLE IF NOT EXISTS public.msg_c2c (msg_id text, from_id bigint,"
            " to_id bigint, conv_seq bigint)"
        >>,
        <<
            "CREATE TABLE IF NOT EXISTS public.msg_store_staging (msg_id text, type text,"
            " from_id bigint, to_id bigint, conv_seq bigint, processed_at timestamptz)"
        >>,
        <<"CREATE TABLE IF NOT EXISTS public.msg_store_seq (conv_key text PRIMARY KEY, seq bigint)">>
    ],
    lists:foreach(fun(Sql) -> {ok, []} = elib_pg:query(Sql, []) end, Statements).
