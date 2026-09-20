-module(api_v1_auth_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Kept local (not via error_code.hrl) so the suite builds under
%% TEST_ERLC_OPTS without project include paths; the value is the
%% production ERR_TOKEN_MALFORMED that token_ds:decrypt_token/1 returns
%% for garbled or tampered tokens.
-define(ERR_TOKEN_MALFORMED, 706).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    auth_001_valid_refreshtoken_exchange/1,
    auth_002_missing_refreshtoken/1,
    auth_003_tampered_refreshtoken/1
]).

-define(PATH, <<"/api/v1/refreshtoken">>).

all() ->
    [
        auth_001_valid_refreshtoken_exchange,
        auth_002_missing_refreshtoken,
        auth_003_tampered_refreshtoken
    ].

%% The application itself is real: config load, core dependency apps and the
%% serialized boot all go through the project's standard CT entry
%% (eunit_runner:ct_suite_setup/1). Device-sign is the production default and
%% is set explicitly here so the refresh path is exercised under
%% api_auth_switch=on. The refresh endpoint is JWT-open but signature-gated
%% (auth_middleware_api_v1.erl:74), so every case below carries valid
%% signature headers and the boundary under test stays in the handler.
init_per_suite(Config0) ->
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    Port = ranch:get_port(imboy_listener),
    SignKey = rest_fixture:ensure_sign_key(),
    [{http_port, Port}, {sign_key, SignKey} | Config].

end_per_suite(Config) ->
    ct:log("auth regression suite done"),
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% Cases
%% ===================================================================

%% Happy path: a login-issued refresh token bound to an active device
%% exchanges for a fresh access token. The credential travels in the
%% imboy-refreshtoken request header, not the body (passport_handler:303).
auth_001_valid_refreshtoken_exchange(Config) ->
    SignKey = ?config(sign_key, Config),
    #{uid := Uid} = User = rest_fixture:create_user(#{}),
    LoggedIn = rest_fixture:login(User, SignKey),
    #{did := Did, refreshtoken := Rtk} = LoggedIn,
    ok = await_device_active(Uid, Did),

    Request = #{},
    Headers = maps:merge(
        rest_fixture:signed_headers(Did, SignKey),
        #{<<"imboy-refreshtoken">> => Rtk}
    ),
    Response = rest_client:post(?config(http_port, Config), ?PATH, Request, Headers),
    verify(
        <<"AUTH-001">>,
        %% The transport header carries a live refresh token and the shared
        %% redactor only matches the bare key "refreshtoken", so the request
        %% is pre-redacted before evidence is written.
        #{<<"imboy-refreshtoken">> => <<"[REDACTED]">>},
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"msg">> => <<"success">>}, Resp),
            rest_assert:predicate([<<"payload">>, <<"token">>], fun nonempty_binary/1, Resp),
            %% White-box auxiliary proof (same node, same jwt_key): the new
            %% token is a tk for the same uid and keeps the device binding
            %% the refresh endpoint is documented to preserve (E2EE-013).
            #{body := #{<<"payload">> := #{<<"token">> := NewToken}}} = Resp,
            {ok, Uid, _ExpireDAt, <<"tk">>, Did, _Ep} = token_ds:decrypt_token(NewToken),
            ok
        end
    ).

%% Validation: the handler reads the credential from the
%% imboy-refreshtoken header; without it token_ds:decrypt_token(undefined)
%% lands in its catch branch (706) and the handler folds that into the
%% business-error envelope at HTTP 200 (elib_response:error/3).
auth_002_missing_refreshtoken(Config) ->
    SignKey = ?config(sign_key, Config),
    #{uid := Uid} = User = rest_fixture:create_user(#{}),
    LoggedIn = rest_fixture:login(User, SignKey),
    #{did := Did} = LoggedIn,
    ok = await_device_active(Uid, Did),

    Request = #{},
    Headers = rest_fixture:signed_headers(Did, SignKey),
    Response = rest_client:post(?config(http_port, Config), ?PATH, Request, Headers),
    verify(
        <<"AUTH-002">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => ?ERR_TOKEN_MALFORMED},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_TOKEN_MALFORMED, <<"payload">> => #{}}, Resp
            ),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, Resp)
        end
    ).

%% Authentication: a token whose last character is flipped no longer
%% verifies under the service jwt_key; decrypt_token returns
%% {error, 706, _} (verify-error or catch branch) and the handler answers
%% with the business-error envelope at HTTP 200.
auth_003_tampered_refreshtoken(Config) ->
    SignKey = ?config(sign_key, Config),
    #{uid := Uid} = User = rest_fixture:create_user(#{}),
    LoggedIn = rest_fixture:login(User, SignKey),
    #{did := Did, refreshtoken := Rtk} = LoggedIn,
    ok = await_device_active(Uid, Did),

    Request = #{},
    Headers = maps:merge(
        rest_fixture:signed_headers(Did, SignKey),
        #{<<"imboy-refreshtoken">> => tamper_tail(Rtk)}
    ),
    Response = rest_client:post(?config(http_port, Config), ?PATH, Request, Headers),
    verify(
        <<"AUTH-003">>,
        %% Tampered value is still token-shaped; redact it in evidence.
        #{<<"imboy-refreshtoken">> => <<"[REDACTED]">>},
        Response,
        #{<<"http_status">> => 200, <<"code">> => ?ERR_TOKEN_MALFORMED},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_TOKEN_MALFORMED, <<"payload">> => #{}}, Resp
            ),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, Resp)
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

verify(CaseId, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_auth">>,
        method => <<"POST">>,
        path => ?PATH,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

nonempty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.

%% The login success path writes the user_device row through
%% gen_server:cast (user_server {login_success, ...}), while both the
%% refresh handler and the JWT gate reject tokens whose device row is not
%% active yet. Waiting on the production predicate user_device_logic:
%% is_active/2 synchronizes the fixture without touching shared code.
await_device_active(Uid, Did) ->
    await_device_active(Uid, Did, 50).

await_device_active(_Uid, _Did, 0) ->
    ct:fail(device_row_not_visible);
await_device_active(Uid, Did, Attempts) ->
    case user_device_logic:is_active(Uid, Did) of
        true ->
            ok;
        false ->
            timer:sleep(100),
            await_device_active(Uid, Did, Attempts - 1)
    end.

%% Flip the final character of a JWT so the HMAC no longer verifies while
%% the token stays structurally parseable.
tamper_tail(Token) when byte_size(Token) > 1 ->
    Size = byte_size(Token) - 1,
    <<Head:Size/binary, Last>> = Token,
    <<Head/binary, (flip_char(Last))/binary>>.

flip_char($A) -> <<"B">>;
flip_char(_) -> <<"A">>.
