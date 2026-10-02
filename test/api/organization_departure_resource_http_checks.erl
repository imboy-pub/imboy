%% Real exit -> resource authorization journey on disposable PG and Garage.
-module(organization_departure_resource_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").
-define(H, intbe02_http_support).
-define(U, 995017).

run(H) ->
    configure(H),
    W = ?H:fixture(ws_a1, H),
    G = ?H:fixture(grp_human, H),
    Ch = ?H:fixture(chn_a1, H),
    C = maps:get(conn, H),
    seed(C, W, G, Ch),
    Headers = customer_service_seat_http_checks:headers(#{actor_user_id => ?U}),
    Bytes = <<"synthetic retained enterprise document\n">>,
    {FileId, DownloadPath, TicketPath} = prepare_file(H, G, Headers, Bytes),
    ?assertEqual(0, code(get(H, channel_path(Ch), Headers))),
    ?assertEqual(0, code(get(H, qr_path(Ch), Headers))),
    ?assertEqual(
        0, code(get(H, <<"/api/v1/channel/by_custom_id/departure-enterprise-channel">>, Headers))
    ),
    Body = #{
        <<"application_key">> => <<"intbe02-oa-sso">>,
        <<"redirect_uri">> => ?H:fixture(redirect, H),
        <<"nonce">> => ?H:fixture(nonce, H)
    },
    Issued = post(H, <<"/api/v1/oa/sso/code">>, Body, Headers),
    ?assertEqual(0, code(Issued)),
    #{<<"code">> := SsoCode} = maps:get(<<"payload">>, json(Issued)),
    ExitPath = <<"/api/v1/organizations/995101/members/995017/offboard">>,
    ?assertEqual(0, code(post(H, ExitPath, #{}, Headers))),
    verify_departure(H, W, G, Ch, DownloadPath, TicketPath, Headers),
    verify_oa(H, Body, SsoCode, Headers),
    retain_and_record(H, FileId, DownloadPath, Bytes).

prepare_file(H, G, Headers, Bytes) ->
    C = maps:get(conn, H),
    {ok, #{<<"file_id">> := Ref}} = group_file_logic:upload(
        G, ?U, <<"departure.txt">>, Bytes, <<"text/plain">>
    ),
    #{<<"id">> := FileId} = ?H:one(C, <<"SELECT id FROM group_file WHERE file_id=$1">>, [Ref]),
    ?assertEqual(0, code(get(H, file_list(G), Headers))),
    DownloadPath = <<"/api/v1/group/file/download?file_id=", (integer_to_binary(FileId))/binary>>,
    Download = get(H, DownloadPath, Headers),
    ?assertEqual(302, maps:get(status, Download)),
    TicketPath = url_path(maps:get(<<"location">>, maps:get(headers, Download))),
    Content = get(H, TicketPath, #{}),
    ?assertEqual(200, maps:get(status, Content)),
    ?assertEqual(Bytes, maps:get(body, Content)),
    {FileId, DownloadPath, TicketPath}.

retain_and_record(H, FileId, DownloadPath, Bytes) ->
    C = maps:get(conn, H),
    OwnerHeaders = customer_service_seat_http_checks:headers(#{actor_user_id => 995011}),
    ?assertEqual(302, maps:get(status, get(H, DownloadPath, OwnerHeaders))),
    #{<<"status">> := 1, <<"uploader_id">> := ?U} = ?H:one(
        C, <<"SELECT status,uploader_id FROM group_file WHERE id=$1">>, [FileId]
    ),
    #{<<"status">> := 1} = ?H:one(C, <<"SELECT status FROM \"user\" WHERE id=$1">>, [?U]),
    Output = filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "departure-resource-result.json"),
    ok = file:write_file(
        Output,
        jsone:encode(#{
            status => <<"PASS">>,
            checks => [
                channel_id_read_revoked,
                channel_custom_id_read_revoked,
                channel_qrcode_read_revoked,
                group_file_list_revoked,
                group_file_download_revoked,
                issued_file_ticket_revoked,
                workspace_revoked,
                group_membership_revoked,
                channel_subscription_removed,
                oa_code_issue_revoked,
                issued_oa_code_exchange_revoked,
                enterprise_file_preserved,
                remaining_member_file_access,
                personal_account_preserved
            ],
            bytes => byte_size(Bytes),
            external_customer_oa => false
        })
    ).

configure(H) ->
    application:set_env(
        imboy,
        base_url,
        iolist_to_binary([
            "http://127.0.0.1:", integer_to_list(maps:get(port, H))
        ])
    ),
    application:set_env(imboy, garage, #{
        endpoint => list_to_binary(os:getenv("IMBOY_TEST_ENDPOINT")),
        bucket => <<"synthetic-enterprise-assets">>,
        region => <<"garage">>,
        access_key => list_to_binary(os:getenv("IMBOY_TEST_ACCESS")),
        secret_key => list_to_binary(os:getenv("IMBOY_TEST_SECRET"))
    }),
    app_version_ds:set_sign_key(
        <<"synthetic">>,
        <<"seat-test">>,
        <<"synthetic.seat">>,
        <<"synthetic-seat-device-key">>
    ).

seed(C, W, G, Ch) ->
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
        [W, ?U]
    ),
    ok = ?H:sql_exec(
        C, <<"INSERT INTO group_member(id,group_id,user_id,role,status) VALUES($1,$2,$3,3,1)">>, [
            cs_pg_test_fixture:id(), G, ?U
        ]
    ),
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO group_member_generation(group_id,user_id,generation_no,start_seq) VALUES($1,$2,1,1)">>,
        [G, ?U]
    ),
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO group_member_generation(group_id,user_id,generation_no,start_seq) VALUES($1,995011,1,1) ON CONFLICT DO NOTHING">>,
        [G]
    ),
    ok = ?H:sql_exec(
        C,
        <<"INSERT INTO channel_subscription(id,channel_id,user_id,status) VALUES($1,$2,$3,1)">>,
        [
            cs_pg_test_fixture:id(), Ch, ?U
        ]
    ),
    ?H:sql_exec(C, <<"UPDATE channel SET custom_id='departure-enterprise-channel' WHERE id=$1">>, [
        Ch
    ]).

verify_departure(H, W, G, Ch, DownloadPath, TicketPath, Headers) ->
    ?assertEqual(403, code(get(H, qr_path(Ch), Headers))),
    ?assertEqual(403, code(get(H, channel_path(Ch), Headers))),
    ?assertEqual(
        403, code(get(H, <<"/api/v1/channel/by_custom_id/departure-enterprise-channel">>, Headers))
    ),
    ?assertEqual(?ERR_NOT_GROUP_MEMBER, code(get(H, file_list(G), Headers))),
    ?assertEqual(?ERR_NOT_GROUP_MEMBER, code(get(H, DownloadPath, Headers))),
    ?assertEqual(403, maps:get(status, get(H, TicketPath, #{}))),
    ?assertMatch({error, {403, _}}, workspace_logic:detail(?U, W)),
    ?assertEqual(false, group_ds:is_member(?U, G)),
    ?assertEqual(
        0,
        cs_pg_test_fixture:scalar(
            <<"SELECT count(*) FROM channel_subscription WHERE channel_id=$1 AND user_id=$2 AND status=1">>,
            [Ch, ?U]
        )
    ).

verify_oa(H, Body, SsoCode, Headers) ->
    ?assertEqual(403, code(post(H, <<"/api/v1/oa/sso/code">>, Body, Headers))),
    Exchange = post(
        H,
        <<"/api/internal/v1/oa/sso/exchange">>,
        maps:remove(<<"application_key">>, Body#{<<"code">> => SsoCode}),
        ?H:auth(maps:get(cred_sso, H))
    ),
    ?assertEqual(422, maps:get(status, Exchange)),
    ?assertEqual(
        <<"identity_not_mapped">>, maps:get(<<"code">>, maps:get(<<"error">>, json(Exchange)))
    ),
    #{<<"consumed_at">> := null, <<"unexpired">> := true} = ?H:one(
        maps:get(conn, H),
        <<"SELECT consumed_at,expires_at>clock_timestamp() AS unexpired FROM enterprise_oa_sso_code WHERE code_digest=$1">>,
        [enterprise_oa_sso_code_repo:digest_hex(SsoCode)]
    ).

get(H, Path, Headers) -> ?H:http(maps:get(port, H), <<"GET">>, Path, <<>>, Headers).
post(H, Path, Body, Headers) -> ?H:http(maps:get(port, H), <<"POST">>, Path, Body, Headers).
json(R) -> jsone:decode(maps:get(body, R)).
code(R) -> maps:get(<<"code">>, json(R)).
channel_path(Id) -> <<"/api/v1/channel/", (integer_to_binary(Id))/binary>>.
qr_path(Id) ->
    Exp = integer_to_binary(elib_dt:millisecond() + 600000),
    Key = ec_cnv:to_binary(config_ds:env(solidified_key)),
    Signature = elib_hasher:md5(<<Exp/binary, "_", Key/binary>>),
    Qs = uri_string:compose_query([
        {<<"id">>, integer_to_binary(Id)}, {<<"exp">>, Exp}, {<<"tk">>, Signature}
    ]),
    <<"/api/v1/channel/qrcode?", Qs/binary>>.
file_list(Id) -> <<"/api/v1/group/file/list?gid=", (integer_to_binary(Id))/binary>>.
url_path(Url) ->
    Parsed = uri_string:parse(Url),
    <<(maps:get(path, Parsed))/binary, "?", (maps:get(query, Parsed))/binary>>.
