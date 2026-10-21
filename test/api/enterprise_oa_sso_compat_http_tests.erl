-module(enterprise_oa_sso_compat_http_tests).
-include_lib("eunit/include/eunit.hrl").

outbound_attribution_test_() ->
    {timeout, 180, fun() ->
        State = intbe02_http_support:setup_all(),
        try
            Connection = maps:get(conn, State),
            Organization = 118149000000000001,
            ok = intbe02_http_support:sql_exec(
                Connection,
                <<
                    "INSERT INTO organization(id,name,owner_id,status,branding,settings) "
                    "VALUES($1,'synthetic-sso-compat',995017,'active','{}','{}')"
                >>,
                [Organization]
            ),
            attribution(State, Organization, <<"compat-owned">>),
            attribution(State, null, <<"compat-global">>)
        after
            intbe02_http_support:teardown_all(State),
            inttest_marker_db:release(State)
        end
    end}.

application(State, Organization, Key) ->
    Connection = maps:get(conn, State),
    Redirect = <<"https://oa.customer.example.com/sso/cb">>,
    {ok, Application} = enterprise_application_repo:create_tx(
        Connection, Organization, Key, Key, null, [Redirect]
    ),
    ApplicationId = maps:get(<<"id">>, Application),
    {ok, #{credential := Credential}} = enterprise_internal_ops:issue_credential_tx(
        Connection,
        Organization,
        ApplicationId,
        <<"synthetic_compat_secret_0123456789abcdef">>,
        undefined
    ),
    {ApplicationId, Credential}.

attribution(State, Organization, Key) ->
    Connection = maps:get(conn, State),
    Redirect = <<"https://oa.customer.example.com/sso/cb">>,
    {ApplicationId, Credential} = application(State, Organization, Key),
    Nonce = <<"synthetic_compat_nonce_0123456789">>,
    {ok, #{<<"code">> := Code}} = enterprise_oa_sso_logic:issue_code_tx(
        Connection,
        995017,
        #{
            <<"application_key">> => Key,
            <<"redirect_uri">> => Redirect,
            <<"nonce">> => Nonce
        }
    ),
    Params = #{<<"code">> => Code, <<"redirect_uri">> => Redirect, <<"nonce">> => Nonce},
    Port = maps:get(port, State),
    Path = <<"/api/internal/v1/oa/sso/exchange">>,
    Missing = intbe02_http_support:http(Port, <<"POST">>, Path, Params, #{}),
    ?assertEqual(401, maps:get(status, Missing)),
    Headers = intbe02_http_support:auth(Credential),
    Result = intbe02_http_support:http(Port, <<"POST">>, Path, Params, Headers),
    ?assertEqual(200, maps:get(status, Result)),
    Payload = jsone:decode(maps:get(body, Result)),
    ?assertEqual(Organization, maps:get(<<"organization_id">>, Payload)),
    ?assertEqual(ApplicationId, maps:get(<<"application_id">>, Payload)),
    ?assertEqual(995017, maps:get(<<"user_id">>, Payload)),
    ?assertEqual(<<"995017">>, maps:get(<<"external_user_id">>, Payload)),
    ?assert(is_binary(maps:get(<<"consumed_at">>, Payload))),
    ?assertEqual(5, map_size(Payload)),
    Replay = intbe02_http_support:http(Port, <<"POST">>, Path, Params, Headers),
    ?assertEqual(404, maps:get(status, Replay)),
    disabled_application(State, Key, ApplicationId, Headers, Params).

disabled_application(State, Key, ApplicationId, Headers, Params) ->
    Connection = maps:get(conn, State),
    Port = maps:get(port, State),
    Path = <<"/api/internal/v1/oa/sso/exchange">>,
    Redirect = maps:get(<<"redirect_uri">>, Params),
    Nonce = maps:get(<<"nonce">>, Params),
    {ok, #{<<"code">> := RevokedCode}} = enterprise_oa_sso_logic:issue_code_tx(
        Connection,
        995017,
        #{
            <<"application_key">> => Key,
            <<"redirect_uri">> => Redirect,
            <<"nonce">> => Nonce
        }
    ),
    ok = intbe02_http_support:sql_exec(
        Connection,
        <<
            "UPDATE enterprise_application SET status='disabled' WHERE id=$1"
        >>,
        [ApplicationId]
    ),
    Denied = intbe02_http_support:http(
        Port,
        <<"POST">>,
        Path,
        Params#{<<"code">> => RevokedCode},
        Headers
    ),
    ?assertEqual(403, maps:get(status, Denied)),
    #{<<"error">> := #{<<"code">> := <<"application_disabled">>}} =
        jsone:decode(maps:get(body, Denied)),
    #{<<"consumed_at">> := null} = intbe02_http_support:one(
        Connection,
        <<"SELECT consumed_at FROM enterprise_oa_sso_code WHERE code_digest=$1">>,
        [enterprise_oa_sso_code_repo:digest_hex(RevokedCode)]
    ).
