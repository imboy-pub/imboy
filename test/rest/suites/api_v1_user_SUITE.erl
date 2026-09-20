-module(api_v1_user_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Kept local (not via error_code.hrl) so the suite builds under
%% TEST_ERLC_OPTS without project include paths; the values are the
%% production codes asserted by auth_ds:do_authorization/5:
%%   401 ERR_TOKEN_MISSING / ERR_UNAUTHORIZED (missing or revoked token)
%%   706 ERR_TOKEN_MALFORMED          (tampered token)
%%   705 ERR_TOKEN_EXPIRED_REFRESHABLE (expired token)
-define(ERR_TOKEN_MISSING, 401).
-define(ERR_TOKEN_MALFORMED, 706).
-define(ERR_TOKEN_EXPIRED_REFRESHABLE, 705).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    user_001_show_with_valid_token/1,
    user_002_missing_authorization/1,
    user_003_tampered_token/1,
    user_expired_expired_token/1,
    user_005_cross_user_isolation/1,
    user_006_change_password_lifecycle/1
]).

-define(SHOW_PATH, <<"/api/v1/user/show">>).
-define(UPDATE_PATH, <<"/api/v1/user/update">>).
-define(CHANGE_PWD_PATH, <<"/api/v1/user/change_password">>).

all() ->
    [
        user_001_show_with_valid_token,
        user_002_missing_authorization,
        user_003_tampered_token,
        user_expired_expired_token,
        user_005_cross_user_isolation,
        user_006_change_password_lifecycle
    ].

%% Real app through the standard CT entry; api_auth_switch=on so
%% user/update and user/change_password run behind the production
%% device-signature + JWT middleware chain. user/show sits in
%% imboy_router:open/0 and is exercised with client-shaped headers.
init_per_suite(Config0) ->
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    Port = ranch:get_port(imboy_listener),
    SignKey = rest_fixture:ensure_sign_key(),
    [{http_port, Port}, {sign_key, SignKey} | Config].

end_per_suite(Config) ->
    ct:log("user regression suite done"),
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% Cases
%% ===================================================================

%% Happy path. user/show is a public endpoint (imboy_router:open/0); the
%% request still carries the client-shaped signature + Bearer headers and
%% asserts the minimized public payload (no account/mobile/email and no
%% account_type for a regular human account).
user_001_show_with_valid_token(Config) ->
    SignKey = ?config(sign_key, Config),
    {User, LoggedIn} = login_ready(SignKey),
    Uid = maps:get(uid, User),
    Path = show_path(Uid),
    Headers = client_headers(LoggedIn, SignKey),
    Response = rest_client:request(?config(http_port, Config), <<"GET">>, Path, <<>>, Headers),
    verify(
        <<"USER-001">>,
        Path,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"msg">> => <<"success">>}, Resp),
            rest_assert:json_path(
                [<<"payload">>, <<"id">>], integer_to_binary(Uid), Resp
            ),
            rest_assert:json_path(
                [<<"payload">>, <<"nickname">>], maps:get(nickname, User), Resp
            ),
            %% PII narrowing: the open endpoint must not leak the login
            %% account, contact fields, or a human account_type.
            rest_assert:predicate([<<"payload">>, <<"account">>], fun absent/1, Resp),
            rest_assert:predicate([<<"payload">>, <<"mobile">>], fun absent/1, Resp),
            rest_assert:predicate([<<"payload">>, <<"email">>], fun absent/1, Resp),
            rest_assert:predicate([<<"payload">>, <<"account_type">>], fun absent/1, Resp)
        end
    ).

%% Authentication: without the Authorization header the JWT gate answers
%% with a real HTTP 401 (auth_ds:do_authorization/5, error_with_status)
%% and envelope code 401 ERR_TOKEN_MISSING. The request carries a valid
%% device signature so the signature gate is not the boundary under test.
user_002_missing_authorization(Config) ->
    SignKey = ?config(sign_key, Config),
    {_User, LoggedIn} = login_ready(SignKey),
    Did = maps:get(did, LoggedIn),
    Request = update_request(),
    Headers = rest_fixture:signed_headers(Did, SignKey),
    Response = rest_client:post(?config(http_port, Config), ?UPDATE_PATH, Request, Headers),
    verify(
        <<"USER-002">>,
        Request,
        Response,
        #{<<"http_status">> => 401, <<"code">> => ?ERR_TOKEN_MISSING},
        fun(Resp) ->
            rest_assert:status(401, Resp),
            rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_TOKEN_MISSING,
                    <<"msg">> => <<"未登录，请先登录"/utf8>>,
                    <<"payload">> => #{}
                },
                Resp
            )
        end
    ).

%% Authentication: a signature-valid JWT whose signature bytes were
%% flipped fails token_ds:decrypt_token/1 with 706; do_authorization maps
%% ERR_TOKEN_MALFORMED to a real HTTP 401 while keeping envelope code 706.
user_003_tampered_token(Config) ->
    SignKey = ?config(sign_key, Config),
    {_User, LoggedIn} = login_ready(SignKey),
    Token = tamper_tail(maps:get(token, LoggedIn)),
    Request = update_request(),
    Headers = bearer_headers(LoggedIn, SignKey, Token),
    Response = rest_client:post(?config(http_port, Config), ?UPDATE_PATH, Request, Headers),
    verify(
        <<"USER-003">>,
        Request,
        Response,
        #{<<"http_status">> => 401, <<"code">> => ?ERR_TOKEN_MALFORMED},
        fun(Resp) ->
            rest_assert:status(401, Resp),
            rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Resp),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_TOKEN_MALFORMED, <<"payload">> => #{}}, Resp
            ),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, Resp)
        end
    ).

%% Authentication, expired class. Deterministic construction: token_ds
%% signs and verifies with jwerl hs256 under config jwt_key and a 300 s
%% exp_leeway, so a token signed now with exp = now - 400 is
%% signature-valid and reliably expired. do_authorization maps 705 to a
%% real HTTP 401 with "Please refresh token". The middleware stops the
%% request before user_handler:update/2, so no profile data changes.
user_expired_expired_token(Config) ->
    SignKey = ?config(sign_key, Config),
    {User, LoggedIn} = login_ready(SignKey),
    Uid = maps:get(uid, User),
    Did = maps:get(did, LoggedIn),
    JwtKey = config_ds:env(jwt_key, <<>>),
    ExpiredToken =
        jwerl:sign(
            #{
                sub => <<"tk">>,
                exp => erlang:system_time(second) - 400,
                uid => Uid,
                did => Did
            },
            hs256,
            JwtKey
        ),
    Request = update_request(),
    Headers = bearer_headers(LoggedIn, SignKey, ExpiredToken),
    Response = rest_client:post(?config(http_port, Config), ?UPDATE_PATH, Request, Headers),
    verify(
        <<"USER-EXPIRED">>,
        Request,
        Response,
        #{<<"http_status">> => 401, <<"code">> => ?ERR_TOKEN_EXPIRED_REFRESHABLE},
        fun(Resp) ->
            rest_assert:status(401, Resp),
            rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_TOKEN_EXPIRED_REFRESHABLE,
                    <<"msg">> => <<"Please refresh token">>,
                    <<"payload">> => #{}
                },
                Resp
            )
        end
    ).

%% Cross-user isolation. user_handler:update/2 derives the target from the
%% JWT-injected current_uid only; a body-level uid field pointing at
%% another user is ignored. Read-back through the public user/show
%% confirms A's nickname changed and B's did not. (user/show returns
%% whoever ?id names — it is a public lookup, not "the token owner" — so
%% isolation is asserted on the write path, where it actually lives.)
user_005_cross_user_isolation(Config) ->
    SignKey = ?config(sign_key, Config),
    {UserA, LoggedInA} = login_ready(SignKey),
    {UserB, _LoggedInB} = login_ready(SignKey),
    UidA = maps:get(uid, UserA),
    UidB = maps:get(uid, UserB),
    NickA2 = unique_nickname(<<"A2">>),

    Request = maps:merge(update_request(NickA2), #{<<"uid">> => UidB}),
    Headers = client_headers(LoggedInA, SignKey),
    Response = rest_client:post(?config(http_port, Config), ?UPDATE_PATH, Request, Headers),
    verify(
        <<"USER-005">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success.">>, <<"payload">> => #{}}, Resp
            ),
            %% A's public profile shows the new nickname...
            ShowA =
                rest_client:request(
                    ?config(http_port, Config),
                    <<"GET">>,
                    show_path(UidA),
                    <<>>,
                    client_headers(LoggedInA, SignKey)
                ),
            rest_assert:status(200, ShowA),
            rest_assert:json_path([<<"payload">>, <<"nickname">>], NickA2, ShowA),
            %% ...while B's profile is untouched.
            ShowB =
                rest_client:request(
                    ?config(http_port, Config),
                    <<"GET">>,
                    show_path(UidB),
                    <<>>,
                    client_headers(LoggedInA, SignKey)
                ),
            rest_assert:status(200, ShowB),
            rest_assert:json_path(
                [<<"payload">>, <<"nickname">>], maps:get(nickname, UserB), ShowB
            )
        end
    ).

%% Password lifecycle. A successful change bumps the session epoch inside
%% the same transaction (user_logic:update_password_with_log/4), which
%% invalidates every pre-change token; the old token is then rejected with
%% HTTP 401 / code 401 "会话已吊销，请重新登录", the old password no longer
%% logs in (code 1), and the new password logs in (code 0 + fresh token).
user_006_change_password_lifecycle(Config) ->
    SignKey = ?config(sign_key, Config),
    {User, LoggedIn} = login_ready(SignKey),
    Uid = maps:get(uid, User),
    OldPwd = maps:get(plain_password, User),
    NewPwd = <<"RestRotate-456!">>,

    Request = #{
        <<"existing_pwd">> => OldPwd,
        <<"new_pwd">> => NewPwd,
        <<"rsa_encrypt">> => <<"0">>
    },
    Headers = client_headers(LoggedIn, SignKey),
    Response =
        rest_client:post(?config(http_port, Config), ?CHANGE_PWD_PATH, Request, Headers),
    %% The shared redactor matches pwd/password but not existing_pwd/
    %% new_pwd, so the evidence request is pre-redacted.
    RedactedRequest = Request#{
        <<"existing_pwd">> => <<"[REDACTED]">>,
        <<"new_pwd">> => <<"[REDACTED]">>
    },
    verify(
        <<"USER-006">>,
        RedactedRequest,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success">>, <<"payload">> => #{}}, Resp
            ),

            %% Old token: epoch revoked.
            RevokedResp =
                rest_client:post(
                    ?config(http_port, Config),
                    ?UPDATE_PATH,
                    update_request(),
                    client_headers(LoggedIn, SignKey)
                ),
            rest_assert:status(401, RevokedResp),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_TOKEN_MISSING,
                    <<"msg">> => <<"会话已吊销，请重新登录"/utf8>>,
                    <<"payload">> => #{}
                },
                RevokedResp
            ),

            %% Old password: rejected.
            OldLogin = raw_login(?config(http_port, Config), User, OldPwd, SignKey),
            rest_assert:status(200, OldLogin),
            rest_assert:json_contains(#{<<"code">> => 1, <<"payload">> => #{}}, OldLogin),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, OldLogin),

            %% New password: accepted, and the fresh token decrypts to the
            %% fixture uid (white-box auxiliary proof, same node).
            NewLogin = raw_login(?config(http_port, Config), User, NewPwd, SignKey),
            rest_assert:status(200, NewLogin),
            rest_assert:json_contains(#{<<"code">> => 0}, NewLogin),
            rest_assert:predicate([<<"payload">>, <<"token">>], fun nonempty_binary/1, NewLogin),
            #{body := #{<<"payload">> := #{<<"token">> := FreshToken}}} = NewLogin,
            {ok, Uid, _ExpireDAt, <<"tk">>, _Did, _Ep} = token_ds:decrypt_token(FreshToken),
            ok
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

verify(CaseId, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_user">>,
        method => method_for(CaseId),
        path => path_for(CaseId),
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

path_for(<<"USER-001">>) -> ?SHOW_PATH;
path_for(<<"USER-005">>) -> ?UPDATE_PATH;
path_for(<<"USER-006">>) -> ?CHANGE_PWD_PATH;
path_for(_) -> ?UPDATE_PATH.

method_for(<<"USER-001">>) -> <<"GET">>;
method_for(_) -> <<"POST">>.

%% Create a user and log it in through the real passport/login, then wait
%% for the asynchronously written user_device row. Without the wait the
%% JWT gate and the refresh handler would both reject the fresh token with
%% "device removed" (user_device_ds:is_active/2).
login_ready(SignKey) ->
    User = rest_fixture:create_user(#{nickname => unique_nickname(<<"base">>)}),
    LoggedIn = rest_fixture:login(User, SignKey),
    #{uid := Uid, did := Did} = LoggedIn,
    ok = await_device_active(Uid, Did),
    {User, LoggedIn}.

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

show_path(Uid) ->
    <<?SHOW_PATH/binary, "?id=", (integer_to_binary(Uid))/binary>>.

update_request() ->
    update_request(<<"REST should-not-land">>).

update_request(Value) ->
    #{<<"field">> => <<"nickname">>, <<"value">> => Value}.

%% Device-signature headers plus the Bearer header of the given token:
%% the shape a real client sends to JWT-gated endpoints.
client_headers(LoggedIn, SignKey) ->
    bearer_headers(LoggedIn, SignKey, maps:get(token, LoggedIn)).

bearer_headers(LoggedIn, SignKey, Token) ->
    maps:merge(
        rest_fixture:signed_headers(maps:get(did, LoggedIn), SignKey),
        #{<<"authorization">> => <<"Bearer ", Token/binary>>}
    ).

%% Login through the real passport route without fixture assertions, so
%% expected-failure logins (old password) can be inspected.
raw_login(Port, User, Password, SignKey) ->
    Did = rest_fixture:unique_id(<<"d">>),
    Body = #{
        <<"type">> => <<"account">>,
        <<"account">> => maps:get(account, User),
        <<"pwd">> => Password,
        <<"rsa_encrypt">> => <<"0">>,
        <<"did">> => Did
    },
    rest_client:post(
        Port,
        <<"/api/v1/passport/login">>,
        Body,
        rest_fixture:signed_headers(Did, SignKey)
    ).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

nonempty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.

%% rest_assert:get_path/2 yields the atom `missing` for absent JSON keys.
absent(missing) -> true;
absent(_) -> false.

tamper_tail(Token) when byte_size(Token) > 1 ->
    Size = byte_size(Token) - 1,
    <<Head:Size/binary, Last>> = Token,
    <<Head/binary, (flip_char(Last))/binary>>.

flip_char($A) -> <<"B">>;
flip_char(_) -> <<"A">>.

unique_nickname(Tag) ->
    <<"REST-User-", (rest_fixture:unique_id(Tag))/binary>>.
