%% Real production HTTP authentication matrix; disposable PG only.
-module(customer_service_auth_domains_http_checks).
-export([run/2]).
-include_lib("eunit/include/eunit.hrl").

run(H, S) ->
    Uid = maps:get(actor_user_id, S),
    Did = <<"synthetic-auth-domain-device">>,
    ok = qr_login_logic:log_device_login(Uid, Did, <<"domain-test">>, <<"web">>),
    AdminId = cs_pg_test_fixture:id(),
    ok = cs_pg_test_fixture:exec(
        <<"INSERT INTO adm_user(id,account,password,nickname,role_id) VALUES($1,$2,$3,$4,ARRAY[1]::bigint[])">>,
        [AdminId, integer_to_binary(AdminId), <<"synthetic-non-login-password">>, <<"domain-test">>]
    ),
    {ok, Sig} = adm_session_ds:issue(integer_to_binary(AdminId)),
    Credentials = [
        {human, intbe02_http_support:auth(token_ds:encrypt_token(Uid, Did))},
        {seat, intbe02_http_support:auth(token_ds:encrypt_seat_token(Uid, Did))},
        {admin, #{
            <<"cookie">> => iolist_to_binary([
                <<"adm_user_id=">>, integer_to_binary(AdminId), <<"; adm_user_sig=">>, Sig
            ])
        }},
        {application, intbe02_http_support:auth(maps:get(cred_a, H))}
    ],
    Routes = [
        {human, <<"/api/v1/cs/me/seat-contexts">>},
        {seat, <<"/api/v1/seat/cs/me/seat-contexts">>},
        {admin, <<"/api/adm/current">>},
        {application, <<"/api/internal/v1/application">>}
    ],
    Results = [
        check(H, S, AdminId, Source, Headers, Target, Path)
     || {Source, Headers} <- Credentials, {Target, Path} <- Routes
    ],
    ?assertEqual(4, length([R || #{status := 200} = R <- Results])),
    ?assertEqual(12, length([R || #{status := 401} = R <- Results])),
    Output = filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "auth-domain-matrix.json"),
    ok = file:write_file(Output, jsone:encode(#{status => <<"PASS">>, results => Results})).

check(H, S, AdminId, Source, Headers, Target, Path) ->
    R = intbe02_http_support:http(maps:get(port, H), <<"GET">>, Path, <<>>, Headers),
    Status = maps:get(status, R),
    Json = jsone:decode(maps:get(body, R)),
    Expected =
        case Source =:= Target of
            true -> 200;
            false -> 401
        end,
    ?assertEqual({Source, Target, Expected}, {Source, Target, Status}),
    case Source =:= Target of
        true -> positive(Target, Json, H, S, AdminId);
        false -> negative(Target, Json)
    end,
    #{source => Source, target => Target, path => Path, status => Status}.

negative(application, Json) ->
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(<<"invalid_credential">>, maps:get(<<"code">>, Error));
negative(_Domain, Json) ->
    ?assert(maps:get(<<"code">>, Json) =/= 0).

positive(application, Json, H, _S, _AdminId) ->
    ?assertEqual(
        maps:get(app_a, H), ec_cnv:to_integer(maps:get(<<"application_id">>, Json), strict)
    ),
    ?assert(ec_cnv:to_integer(maps:get(<<"organization_id">>, Json), strict) > 0),
    ?assert(ec_cnv:to_integer(maps:get(<<"credential_id">>, Json), strict) > 0),
    ?assert(maps:get(<<"granted_scopes">>, Json) =/= []);
positive(admin, Json, _H, _S, AdminId) ->
    ?assertEqual(0, maps:get(<<"code">>, Json)),
    ?assertEqual(
        AdminId, ec_cnv:to_integer(maps:get(<<"id">>, maps:get(<<"payload">>, Json)), strict)
    );
positive(Domain, Json, _H, S, _AdminId) when Domain =:= human; Domain =:= seat ->
    ?assertEqual(0, maps:get(<<"code">>, Json)),
    Payload = maps:get(<<"payload">>, Json),
    ?assertEqual(
        maps:get(actor_user_id, S), ec_cnv:to_integer(maps:get(<<"user_id">>, Payload), strict)
    ),
    Identity = maps:get(service_identity_id, S),
    Org = maps:get(org_id, S),
    ?assert(
        lists:any(
            fun(Row) ->
                ec_cnv:to_integer(maps:get(<<"organization_id">>, Row), strict) =:= Org andalso
                    ec_cnv:to_integer(maps:get(<<"business_identity_id">>, Row), strict) =:=
                        Identity andalso
                    maps:get(<<"seat_enabled">>, Row) =:= true
            end,
            maps:get(<<"contexts">>, Payload)
        )
    ).
