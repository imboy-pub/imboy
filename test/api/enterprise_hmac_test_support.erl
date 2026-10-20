-module(enterprise_hmac_test_support).
-moduledoc "Real credential fixtures for HMAC-only repository and logic tests.".
-export([context_tx/3, signed_context_tx/3, headers/3]).

context_tx(C, Org, App) ->
    {ok, [Row]} = elib_pg:query(
        C,
        <<"SELECT organization_id,principal_user_id FROM enterprise_application WHERE id=$1">>,
        [App]
    ),
    Cred =
        case
            elib_pg:query(
                C,
                <<"SELECT id FROM enterprise_application_credential WHERE application_id=$1 AND status='active' ORDER BY id LIMIT 1">>,
                [App]
            )
        of
            {ok, [#{<<"id">> := Id}]} ->
                Id;
            {ok, []} ->
                case config_ds:env(postgre_aes_key, <<>>) of
                    Key when is_binary(Key), byte_size(Key) > 0 -> ok;
                    Key when is_list(Key), Key =/= [] -> ok;
                    _ -> application:set_env(imboy, postgre_aes_key, crypto:strong_rand_bytes(32))
                end,
                {ok, #{credential_id := Id}} = enterprise_internal_ops:issue_credential_tx(
                    C, maps:get(<<"organization_id">>, Row), App, undefined
                ),
                Id
        end,
    #{
        authentication => hmac,
        application_id => App,
        credential_id => Cred,
        organization_id => Org,
        application_organization_id => maps:get(<<"organization_id">>, Row),
        principal_user_id => maps:get(<<"principal_user_id">>, Row)
    }.

signed_context_tx(C, Prefix, Secret) ->
    Path = <<"/api/internal/v1/application">>,
    Headers = headers(<<Prefix/binary, ".", Secret/binary>>, <<"GET">>, Path),
    case enterprise_internal_signature:parse(Headers, erlang:system_time(second)) of
        {ok, Signed} ->
            enterprise_internal_auth:authenticate_signed_tx(
                C, <<"GET">>, Path, <<>>, <<>>, Signed#{headers => Headers}
            );
        Error ->
            Error
    end.

headers(Credential, Method, Path) ->
    intbe02_http_support:signed_headers(Credential, Method, Path, <<>>, #{}, sha256).
