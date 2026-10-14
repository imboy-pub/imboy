%% attachment_lifecycle_v5_pg_tests — W1-02 core protocol eunit suite.
%% Reused by W2-03 (confirm/GC race) and W6-01 (real fault matrix).
-module(attachment_lifecycle_v5_pg_tests).
-include_lib("eunit/include/eunit.hrl").

-define(REG_KEY, <<"v5e2e/reg/1">>).
-define(ASSET_ID, 901100101).

r20_test_() ->
    {timeout, 900, fun run/0}.

run() ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        protocol_roundtrip(Conn),
        session_link_integrity(Conn),
        worker_claim_ack(Conn),
        ?assert(true)
    after
        inttest_marker_db:release(State)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    {ok, _, _} = epgsql:equery(Conn, Sql, Params),
    ok.
one(Conn, Sql, Params) ->
    {ok, _, [Row]} = epgsql:equery(Conn, Sql, Params),
    Row.

registration() ->
    <<
        "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
        "\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/png\","
        "\"size_bytes\":\"10\",\"scope\":\"private\"}}"
    >>.

content_receipt() ->
    lists:flatten(
        [
            "{\"version\":1,\"sha256\":\"",
            lists:duplicate(64, $a),
            "\",\"size_bytes\":\"10\",\"mime\":\"image/png\"}"
        ]
    ).

locator() -> lists:duplicate(64, $b).

protocol_roundtrip(Conn) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [?REG_KEY]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [?REG_KEY]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour','garage','ns1','c1',$3)",
        [?REG_KEY, registration(), locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [?REG_KEY, content_receipt()]
    ),
    %% duplicate identical pin is idempotent
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [?REG_KEY, content_receipt()]
    ),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified) VALUES ($1,$2,'private',101,"
        "'garage','ns1','c1',$3,true)",
        [?ASSET_ID, ?REG_KEY, locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [?REG_KEY, ?ASSET_ID]
    ),
    %% replay returns original id without re-audit
    {ok, _, [{?ASSET_ID}]} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [?REG_KEY, ?ASSET_ID]
    ),
    %% confirmed session keeps link
    {ok, _, [{<<"confirmed">>}]} = epgsql:equery(
        Conn,
        "SELECT session_state FROM attach_pending WHERE object_key=$1",
        [?REG_KEY]
    ).

session_link_integrity(Conn) ->
    %% wrong owner asset cannot be linked
    q(Conn, "DELETE FROM attachment WHERE id=$1", [?ASSET_ID + 1]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id) "
        "VALUES ($1,'v5e2e/other','private',202)",
        [?ASSET_ID + 1]
    ),
    ?assertException(
        error,
        {epgsql, _},
        epgsql:equery(
            Conn,
            "SELECT fn_v5_confirm_pending($1,$2)",
            [?REG_KEY, ?ASSET_ID + 1]
        )
    ).

worker_claim_ack(Conn) ->
    %% enqueue via intent (gc_verified already true from roundtrip)
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'eunit')", [?REG_KEY]
    ),
    Token = one(Conn, "SELECT gen_random_uuid()", []),
    {ok, _, [{Path}]} = epgsql:equery(
        Conn, "SELECT out_path FROM fn_v5_claim_ordinary($1)", [Token]
    ),
    ?assertEqual(?REG_KEY, Path),
    {ok, _, [{<<"done">>}]} = epgsql:equery(
        Conn, "SELECT fn_v5_ack_ordinary($1,$2,true)", [?REG_KEY, Token]
    ),
    %% stale token cannot mutate done row
    {ok, _, [{<<"not_leased">>}]} = epgsql:equery(
        Conn, "SELECT fn_v5_ack_ordinary($1,$2,true)", [?REG_KEY, Token]
    ).
