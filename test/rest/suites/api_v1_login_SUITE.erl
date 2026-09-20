-module(api_v1_login_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Kept local (not via error_code.hrl) so the suite builds under
%% TEST_ERLC_OPTS without project include paths; the value is the
%% production ERR_SIGNATURE_INVALID.
-define(ERR_SIGNATURE_INVALID, 902).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    login_001_valid_credentials/1,
    login_002_wrong_password/1,
    login_003_unknown_account/1,
    login_004_malformed_json/1,
    login_005_device_signature_boundary/1
]).

-define(PATH, <<"/api/v1/passport/login">>).
-define(TEST_COS, <<"android">>).
-define(TEST_VSN, <<"rest-golden">>).
-define(TEST_PKG, <<"pub.imboy.rest">>).

all() ->
    [
        login_001_valid_credentials,
        login_002_wrong_password,
        login_003_unknown_account,
        login_004_malformed_json,
        login_005_device_signature_boundary
    ].

%% The application itself is real: config load, core dependency apps and
%% the serialized boot all go through the project's standard CT entry
%% (eunit_runner:ct_suite_setup/1); migrations run against the scratch
%% database during that start, and Ranch binds an ephemeral port
%% (TEST_HTTP_PORT=0). Device-sign is the production default and is set
%% explicitly here so the golden path is exercised under api_auth_switch=on.
init_per_suite(Config0) ->
    ok = rest_fixture:ensure_ct_priv_alias(),
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    Port = ranch:get_port(imboy_listener),

    %% Independent per-run signing key (RTF-03 task 3): registered through
    %% the production key lookup. LOGIN-001 (accepted) and LOGIN-005
    %% (rejected) prove the injection took.
    SignKey = rest_fixture:ensure_sign_key(),

    Suffix = run_suffix(),
    User = rest_fixture:create_user(#{
        account => <<"rest-login-", Suffix/binary>>,
        email => <<"rest-login-", Suffix/binary, "@example.invalid">>,
        nickname => <<"REST Golden ", Suffix/binary>>
    }),
    [
        {http_port, Port},
        {user, User},
        {suffix, Suffix},
        {sign_key, SignKey}
        | Config
    ].

end_per_suite(Config) ->
    ct:log("login golden suite done"),
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% Cases
%% ===================================================================

login_001_valid_credentials(Config) ->
    User = ?config(user, Config),
    Did = did(Config, <<"001">>),
    Request = login_request(User, Did),
    Response = post(Config, Did, Request),
    verify(
        <<"LOGIN-001">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"msg">> => <<"success">>}, Resp),
            rest_assert:json_contains(#{<<"account">> => maps:get(account, User)}, Resp),
            rest_assert:predicate([<<"payload">>, <<"account">>], fun nonempty_binary/1, Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"uid">>],
                matches_uid(maps:get(uid, User)),
                Resp
            ),
            rest_assert:predicate([<<"payload">>, <<"token">>], fun nonempty_binary/1, Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"refreshtoken">>], fun nonempty_binary/1, Resp
            )
        end
    ).

login_002_wrong_password(Config) ->
    User = ?config(user, Config),
    Did = did(Config, <<"002">>),
    Request = login_request(User, Did, <<"WrongPassword123!">>),
    Response = post(Config, Did, Request),
    verify(
        <<"LOGIN-002">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 1, <<"payload">> => #{}}, Resp),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, Resp)
        end
    ).

login_003_unknown_account(Config) ->
    Suffix = ?config(suffix, Config),
    Did = did(Config, <<"003">>),
    Account = <<"rest-login-missing-", Suffix/binary>>,
    Request = #{
        <<"type">> => <<"account">>,
        <<"account">> => Account,
        <<"pwd">> => <<"RestLogin123!">>,
        <<"rsa_encrypt">> => <<"0">>
    },
    Response = post(Config, Did, Request),
    verify(
        <<"LOGIN-003">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1, <<"msg">> => <<"账号不存在"/utf8>>},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"账号不存在"/utf8>>, <<"payload">> => #{}},
                Resp
            )
        end
    ).

login_004_malformed_json(Config) ->
    Did = did(Config, <<"004">>),
    Request = <<"{not-json">>,
    Response = post(Config, Did, Request),
    verify(
        <<"LOGIN-004">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1},
        fun(Resp) ->
            %% The malformed body must not crash the handler or the
            %% listener; the current convention folds it into the
            %% business-error envelope.
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 1, <<"payload">> => #{}}, Resp)
        end
    ).

%% passport is JWT-open but device-sign gated: with api_auth_switch=on a
%% missing or tampered signature is rejected at the middleware boundary
%% (code 902), and a correctly signed request still passes through to the
%% business layer in LOGIN-001.
login_005_device_signature_boundary(Config) ->
    User = ?config(user, Config),

    Missing = login_request(User, did(Config, <<"005a">>)),
    ResponseMissing = rest_client:post(
        ?config(http_port, Config),
        ?PATH,
        Missing,
        unsigned_headers(Missing)
    ),

    DidB = did(Config, <<"005b">>),
    Tampered = login_request(User, DidB),
    TamperHeaders = signed_headers(DidB, <<"tampered-key-not-the-one-stored">>),
    ResponseTampered = rest_client:post(?config(http_port, Config), ?PATH, Tampered, TamperHeaders),

    verify(
        <<"LOGIN-005">>,
        Missing,
        ResponseMissing,
        #{<<"http_status">> => 200, <<"code">> => 902},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 902, <<"msg">> => <<"签名验证失败，请更新客户端"/utf8>>},
                Resp
            ),
            %% The tampered-signature variant hits the same boundary.
            rest_assert:status(200, ResponseTampered),
            rest_assert:json_contains(#{<<"code">> => 902}, ResponseTampered)
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

verify(CaseId, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_login">>,
        method => <<"POST">>,
        path => ?PATH,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

post(Config, Did, Body) ->
    Headers = signed_headers(Did, ?config(sign_key, Config)),
    rest_client:post(?config(http_port, Config), ?PATH, Body, Headers).

login_request(User, Did) ->
    login_request(User, Did, maps:get(plain_password, User)).

login_request(User, Did, Password) ->
    #{
        <<"type">> => <<"account">>,
        <<"account">> => maps:get(account, User),
        <<"pwd">> => Password,
        <<"rsa_encrypt">> => <<"0">>,
        <<"did">> => Did
    }.

%% Signature follows auth_ds:verify_sign/2 exactly: the plain text is
%% Did|Vsn|Cos|Pkg and the key comes from the same app_version_ds lookup
%% the server side uses (no second algorithm implementation).
signed_headers(Did, Key) ->
    Sign = elib_hasher:hmac_sha256(sign_plain(Did), Key),
    Base = unsigned_headers(#{<<"did">> => Did}),
    Base#{<<"sign">> => Sign, <<"method">> => <<"sha256">>}.

unsigned_headers(Body) ->
    Did = maps:get(<<"did">>, Body, <<"rest-login-device">>),
    #{
        <<"cos">> => ?TEST_COS,
        <<"did">> => Did,
        <<"dname">> => <<"REST Golden Suite">>,
        <<"vsn">> => ?TEST_VSN,
        <<"pkg">> => ?TEST_PKG
    }.

sign_plain(Did) ->
    <<Did/binary, "|", ?TEST_VSN/binary, "|", ?TEST_COS/binary, "|", ?TEST_PKG/binary>>.

did(Config, CaseTag) ->
    Suffix = ?config(suffix, Config),
    <<"rest-", Suffix/binary, "-", CaseTag/binary>>.

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

matches_uid(Uid) ->
    fun
        (Value) when is_binary(Value); is_integer(Value) ->
            try
                ec_cnv:to_integer(Value) =:= Uid
            catch
                _:_ -> false
            end;
        (_) ->
            false
    end.

nonempty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.

run_suffix() ->
    Raw = os:getenv("REST_RUN_ID", "manual"),
    Sanitized = list_to_binary(lists:filter(fun alnum/1, Raw)),
    Padded = <<Sanitized/binary, "000000000000">>,
    <<S:12/binary, _/binary>> = Padded,
    string:lowercase(S).

alnum(C) when C >= $a, C =< $z -> true;
alnum(C) when C >= $A, C =< $Z -> true;
alnum(C) when C >= $0, C =< $9 -> true;
alnum(_) -> false.

unique_secret(Bytes) ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(Bytes)),
    <<"rest-", Hex/binary>>.
