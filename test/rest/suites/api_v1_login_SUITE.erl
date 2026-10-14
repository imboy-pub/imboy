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
    login_005_device_signature_boundary/1,
    limiter_real_failures/1
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
        login_005_device_signature_boundary,
        limiter_real_failures
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
    _ = rest_fixture:ensure_sign_key(),

    Suffix = run_suffix(),
    User = rest_fixture:create_user(#{
        account => <<"rest-login-", Suffix/binary>>,
        email => <<"rest-login-", Suffix/binary, "@example.invalid">>,
        nickname => <<"REST Golden ", Suffix/binary>>
    }),
    %% Common Test writes init_per_suite's return value into every suite
    %% log page (review P1): the raw user map carries plain_password and
    %% the password hash, so the config carries a sanitized handle and the
    %% login cases rehydrate the credential-bearing map through the
    %% fixture session store.
    ok = rest_fixture:store_session(golden, User),
    [
        {http_port, Port},
        {user, rest_fixture:sanitize_user(User, golden)},
        {suffix, Suffix}
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
    %% The request itself needs the plain fixture password, which the
    %% sanitized config handle does not carry (review P1): rehydrate the
    %% full map from the session store instead.
    Request = login_request(rest_fixture:session(golden), Did),
    Response = post(Config, Did, Request),
    verify(
        <<"LOGIN-001">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"msg">> => <<"success.">>}, Resp),
            %% account lives inside the success payload, not on the
            %% envelope top level (envelope is {code,msg,payload,sv_ts}).
            rest_assert:json_contains(
                #{<<"payload">> => #{<<"account">> => maps:get(account, User)}}, Resp
            ),
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
    %% Same credential handling as LOGIN-001 (review P1): the plain
    %% password lives in the session store, not in the suite config.
    Golden = rest_fixture:session(golden),

    Missing = login_request(Golden, did(Config, <<"005a">>)),
    ResponseMissing = rest_client:post(
        ?config(http_port, Config),
        ?PATH,
        Missing,
        unsigned_headers(Missing)
    ),

    DidB = did(Config, <<"005b">>),
    Tampered = login_request(Golden, DidB),
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
%% Limiter V1 L0 缺陷复现（计划 docs/plans/2026-10-13-limiter-v1-*
%% §5.F1、§9.2 AC-01）。真实 handler → passport_logic →
%% login_security_logic → login_attempt_ds 全链（本 suite 起的就是真实
%% 应用），不 mock 任何生产模块。
%%
%% L0 旧侧 oracle：F 次真实错误密码与 F 次真实错误验证码后，账户失败
%% 计数恒为 0、不锁定、第 F+1 次请求照常进入认证——失败计数链因空 IP
%% 断开（record_login_failure 先做 check_ip_rate_limit(<<>>)，invalid_ip
%% 提前返回，账户 ETS 从未被写）。这是缺陷复现成功，不是产品 PASS；
%% L3 修复后本 case 按 AC-01 翻转 oracle（计数 1..F，F 后锁定拒绝）。
%% ===================================================================
limiter_real_failures(Config) ->
    %% IP 外层预算足够（fixture 标准做法：本 case 需要 2F+1 次真实
    %% passport POST，默认 10/min 不够；这同时是 AC-14 揭示的"重复 setup
    %% 即调高限额"机制的正当使用面）
    ok = rest_fixture:ensure_login_throttle_capacity(),
    application:set_env(imboy, login_max_attempts, 5),
    Golden = rest_fixture:session(golden),
    Account = maps:get(account, Golden),
    AttemptKey = login_attempt_ds:cache_key(Account, <<>>),
    try
        ets:delete(login_attempt_ets, AttemptKey)
    catch
        _:_ -> ok
    end,

    WrongPwdRequests = [
        login_request(Golden, did(Config, <<"l0p">>), <<"LimiterWrongPwd!">>)
     || _ <- lists:seq(1, 5)
    ],
    %% F 次真实错误密码：每次读真实 S 快照
    PwdSnapshots =
        [
            begin
                Response = post(Config, did(Config, <<"l0p">>), Req),
                #{<<"code">> := 1} = maps:get(body, Response),
                {ok, LockInfo} = login_security_logic:get_lock_info(Account, <<>>),
                maps:get(<<"attempts">>, LockInfo)
            end
         || Req <- WrongPwdRequests
        ],
    %% L0 旧侧 oracle：真实链上失败计数恒 0（从未写入）
    [0 = Count || Count <- PwdSnapshots],

    %% 第 F+1 次错误密码照常进入认证（未被锁定拒绝）
    Resp6 = post(
        Config,
        did(Config, <<"l0p">>),
        login_request(Golden, did(Config, <<"l0p">>), <<"LimiterWrongPwd!">>)
    ),
    #{<<"code">> := 1} = maps:get(body, Resp6),

    %% F 次真实错误验证码（code 非空 → do_login_by_code 路径）；
    %% 最后一次经 rest_evidence:verify 产出证据文件（runner 的
    %% evidence cross-check 要求每个 case 落盘）
    CodeBody = #{
        <<"type">> => <<"account">>,
        <<"account">> => Account,
        <<"code">> => <<"000000">>,
        <<"rsa_encrypt">> => <<"0">>,
        <<"did">> => did(Config, <<"l0c">>)
    },
    CodeSnapshots =
        [
            begin
                Response = post(Config, did(Config, <<"l0c">>), CodeBody),
                #{<<"code">> := 1} = maps:get(body, Response),
                {ok, LockInfo} = login_security_logic:get_lock_info(Account, <<>>),
                maps:get(<<"attempts">>, LockInfo)
            end
         || _ <- lists:seq(1, 5)
        ],
    [0 = Count || Count <- CodeSnapshots],

    %% 最终仍不锁定
    false = login_attempt_ds:is_locked(Account, <<>>),
    {ok, FinalInfo} = login_security_logic:get_lock_info(Account, <<>>),
    0 = maps:get(<<"attempts">>, FinalInfo),

    %% 证据：第 5 次错误验证码响应（业务错误照常返回，计数仍 0）
    LastResp = post(Config, did(Config, <<"l0c">>), CodeBody),
    verify(
        <<"AC-01">>,
        CodeBody,
        LastResp,
        #{<<"http_status">> => 200, <<"code">> => 1},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 1, <<"payload">> => #{}}, Resp),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, Resp)
        end
    ),

    %% 清理本 case 的合成状态
    true = ets:delete(login_attempt_ets, AttemptKey),
    {comment, "L0 baseline: real-chain failure accounting is broken (all 0); see plan AC-01"}.

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
    Headers = signed_headers(Did, rest_fixture:sign_key()),
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
