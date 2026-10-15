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
%% Limiter V1 AC-01 fixed oracle（计划 docs/plans/2026-10-13-limiter-v1-*
%% §9.2 AC-01）。真实 handler → passport_logic → login_security_logic →
%% login_attempt_ds 全链（本 suite 起的就是真实应用），不 mock 任何生产
%% 模块。
%%
%% fixed 侧 oracle（L3 后翻转）：F=5 次真实错误密码顺序完成，每次读真实
%% D 快照断言计数递增 1..F；失败记账按稳定身份键 {user, Uid} 归并（D1：
%% 跨 IP 合并），因此每次请求用不同的唯一 X-Forwarded-For IP 也不影响
%% 计数——这同时证明空 IP 断链已修复。第 F 次后锁定拒绝：下一次请求
%% 即使给正确密码也返回 code1+锁定消息（不再触碰凭据——正确密码未被
%% 校验），错误验证码同样过同一锁定门被拒。测试用唯一合成账户与唯一
%% XFF IP，不污染其他用例；finally 清理该测试键。
%%
%% 历史：L0 基线时本 case 是缺陷复现 oracle（旧侧计数恒 0），L3 落地后
%% 按计划 §9.2 翻转为上述 fixed 断言。
%% ===================================================================
limiter_real_failures(Config) ->
    %% IP 外层预算足够（fixture 标准做法：放大 limiter_ip_rates 三键，
    %% 见 rest_fixture:ensure_login_throttle_capacity/0）
    ok = rest_fixture:ensure_login_throttle_capacity(),
    application:set_env(imboy, login_max_attempts, 5),
    F = 5,
    %% 唯一合成账户：不复用 golden（本 case 会把它推进锁定，不能污染
    %% 其他用例）；identity 按 {user, Uid} 稳定键归并（D1 修复）
    Suffix = ?config(suffix, Config),
    User = rest_fixture:create_user(#{
        account => <<"rest-lim-", Suffix/binary>>,
        email => <<"rest-lim-", Suffix/binary, "@example.invalid">>,
        nickname => <<"REST Limiter ", Suffix/binary>>
    }),
    Identity = {user, maps:get(uid, User)},
    try
        %% F 次真实错误密码：每次换唯一 XFF IP，真实 D 快照断言计数
        %% 1..F 递增（跨 IP 归并 = 空 IP 断链已修复的直接证据）
        lists:foreach(
            fun(I) ->
                Did = did(Config, <<"l0p">>),
                Req = wrong_pwd_request(User, Did),
                Response = post_xff(Config, Did, xff_ip(I), Req),
                Body = maps:get(body, Response),
                #{<<"code">> := 1} = Body,
                false = (maps:get(<<"msg">>, Body, <<>>) =:= lock_msg()),
                {ok, Snap} = login_attempt_ds:snapshot(Identity),
                I = maps:get(count, Snap),
                %% 达到 F 即锁定（locked = count >= F）：仅前 F-1 次未锁定；
                %% 第 F 次的锁定翻转由下方 SnapLocked 段断言
                case I < F of
                    true -> false = maps:get(locked, Snap);
                    false -> true = maps:get(locked, Snap)
                end
            end,
            lists:seq(1, F)
        ),
        {ok, SnapF} = login_attempt_ds:snapshot(Identity),
        F = maps:get(count, SnapF),

        %% 第 F 次后锁定拒绝：下一次请求即使给正确密码也拒绝（正确密码
        %% 未被触碰——校验未发生），HTTP200 + code1 + 锁定消息
        DidOk = did(Config, <<"l0ok">>),
        CorrectReq = login_request(User, DidOk),
        RespLocked = post_xff(Config, DidOk, xff_ip(F + 1), CorrectReq),
        verify(
            <<"AC-01">>,
            CorrectReq,
            RespLocked,
            #{<<"http_status">> => 200, <<"code">> => 1},
            fun(Resp) ->
                common_assertions(Resp),
                rest_assert:json_contains(
                    #{<<"code">> => 1, <<"msg">> => lock_msg(), <<"payload">> => #{}},
                    Resp
                )
            end
        ),
        {ok, SnapLocked} = login_attempt_ds:snapshot(Identity),
        true = maps:get(locked, SnapLocked),
        F = maps:get(count, SnapLocked),

        %% 错误验证码走同一锁定门：同样拒绝、同样锁定消息
        DidCode = did(Config, <<"l0c">>),
        CodeReq = wrong_code_request(User, DidCode),
        RespCode = post_xff(Config, DidCode, xff_ip(F + 2), CodeReq),
        #{<<"code">> := 1, <<"msg">> := CodeMsg} = maps:get(body, RespCode),
        CodeMsg = lock_msg()
    after
        %% 清理本 case 的合成状态（锁定行与计数），不给其他用例留状态
        ok = login_attempt_ds:reset(Identity)
    end,
    {comment,
        "AC-01 fixed: real-chain failure accounting counts 1..F, locks, and never touches "
        "credentials after lock; see plan AC-01"}.

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

%% post 变体：带唯一 X-Forwarded-For。peer 127.0.0.1 在 trusted_proxy_ips
%% 缺省内，get_client_ip 取 XFF 第 1 跳（elib_req:get_client_ip/1）——AC-01
%% 每次失败请求落进不同 IP 桶，账户域 {user, Uid} 跨 IP 归并由此被真实验证。
post_xff(Config, Did, XffIp, Body) ->
    Signed = signed_headers(Did, rest_fixture:sign_key()),
    Headers = Signed#{<<"x-forwarded-for">> => XffIp},
    rest_client:post(?config(http_port, Config), ?PATH, Body, Headers).

%% Per-attempt unique client IP (private range).
xff_ip(I) ->
    list_to_binary(io_lib:format("10.77.~b.10", [I])).

%% 生产 lock_error_msg 文案（passport_logic；本地复制以避免测试构建的
%% include 路径依赖，与 ?ERR_SIGNATURE_INVALID 同例）。
lock_msg() ->
    <<"失败次数过多，账户已临时锁定，请稍后再试"/utf8>>.

wrong_pwd_request(User, Did) ->
    (login_request(User, Did))#{<<"pwd">> => <<"LimiterWrongPwd!">>}.

wrong_code_request(User, Did) ->
    #{
        <<"type">> => <<"account">>,
        <<"account">> => maps:get(account, User),
        <<"code">> => <<"000000">>,
        <<"rsa_encrypt">> => <<"0">>,
        <<"did">> => Did
    }.

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
