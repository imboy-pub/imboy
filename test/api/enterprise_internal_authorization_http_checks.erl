-module(enterprise_internal_authorization_http_checks).
-export([run/0, run/1]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    S = intbe02_http_support:setup_all(),
    try
        run(S)
    after
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end.

run(#{conn := C} = S) ->
    Routes = enterprise_internal_routes:routes(),
    ?assertEqual(49, length(Routes)),
    {ok, App} = enterprise_application_repo:create_tx(
        C,
        995101,
        <<"auth-matrix-hmac">>,
        <<"Synthetic signed authentication matrix">>,
        995014
    ),
    AppId = maps:get(<<"id">>, App),
    {ok, #{credential := Credential, credential_id := CredId}} =
        enterprise_internal_ops:issue_credential_tx(C, 995101, AppId, undefined),
    matrix(S, Routes, Credential, missing, <<"invalid_credential">>),
    matrix(S, Routes, Credential, bearer, <<"invalid_credential">>),
    matrix(S, Routes, Credential, wrong_signature, <<"invalid_credential">>),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE enterprise_application SET status='disabled' WHERE id=$1">>, [AppId]
    ),
    matrix(S, Routes, Credential, signed, <<"application_disabled">>),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE enterprise_application SET status='active' WHERE id=$1">>, [AppId]
    ),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE enterprise_application_credential SET expires_at='2020-01-01' WHERE id=$1">>, [
            CredId
        ]
    ),
    matrix(S, Routes, Credential, signed, <<"credential_expired">>),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE enterprise_application_credential SET expires_at=NULL WHERE id=$1">>, [CredId]
    ),
    ok = enterprise_internal_ops:revoke_credential_tx(C, 995101, CredId),
    matrix(S, Routes, Credential, signed, <<"invalid_credential">>).

matrix(#{port := Port}, Routes, Credential, Kind, Code) ->
    lists:foreach(
        fun(#{method := Method, path := Template}) ->
            Path = re:replace(Template, "\\{[a-z_]+\\}", "1", [global, {return, binary}]),
            Body = <<"{}">>,
            H =
                case Kind of
                    missing ->
                        #{};
                    bearer ->
                        #{<<"authorization">> => <<"Bearer ", Credential/binary>>};
                    signed ->
                        intbe02_http_support:signed_headers(
                            Credential,
                            Method,
                            Path,
                            Body,
                            #{<<"content-type">> => <<"application/json">>},
                            sha256
                        );
                    wrong_signature ->
                        Signed = intbe02_http_support:signed_headers(
                            Credential,
                            Method,
                            Path,
                            Body,
                            #{<<"content-type">> => <<"application/json">>},
                            sha256
                        ),
                        Signed#{
                            <<"x-imboy-signature">> =>
                                <<"sha256=", (binary:copy(<<"0">>, 64))/binary>>
                        }
                end,
            Reply = intbe02_http_support:http(Port, Method, Path, Body, H),
            ?assertEqual(
                enterprise_internal_error:http_status(Code),
                maps:get(status, Reply),
                {Kind, Method, Path}
            ),
            #{<<"error">> := #{<<"code">> := Actual}} = jsone:decode(maps:get(body, Reply)),
            ?assertEqual(Code, Actual, {Kind, Method, Path})
        end,
        Routes
    ).
