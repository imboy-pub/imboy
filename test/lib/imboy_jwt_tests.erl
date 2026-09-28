-module(imboy_jwt_tests).

%%%
% imboy_jwt 单元测试（纯 jose HS256 签发/验签）
% Unit tests for imboy_jwt (pure-jose HS256 sign/verify).
%
% 覆盖：往返、错误 secret、篡改、alg 混淆（HS512 / none）、时间 claims
% （exp/nbf/iat + leeway + 值类型边界）、畸形输入、类型收紧 guard、
% 空 secret 跨秘钥隔离、64-bit TSID 精度，以及 jwerl-shim 时代存量 token
% 的兼容金丝雀（样本由 shim 于 2026-09-28 签发，Python hmac 独立复核过签名）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, <<"canary_secret_for_compat_test_0123456789">>).
-define(OTHER_SECRET, <<"another_secret_0123456789abcdef">>).
-define(FUTURE, 9999999999).

%%%-------------------------------------------------------------------
%%% 签发/验签往返
%%%-------------------------------------------------------------------

roundtrip_test() ->
    Claims = #{<<"sub">> => <<"tk">>, <<"uid">> => 42, <<"exp">> => ?FUTURE},
    Token = imboy_jwt:sign(Claims, ?SECRET),
    ?assert(is_binary(Token)),
    %% compact JWS 形状：header.payload.signature 共 3 段
    ?assertEqual(3, length(binary:split(Token, <<".">>, [global]))),
    {ok, Got} = imboy_jwt:verify(Token, ?SECRET),
    ?assertEqual(Claims, Got).

roundtrip_nested_claims_test() ->
    %% LiveKit 形状的嵌套 claims（rtc_room_logic 实际签发结构）
    Now = erlang:system_time(second),
    Claims = #{
        <<"iss">> => <<"testkey">>,
        <<"sub">> => <<"5_devA">>,
        <<"name">> => <<"测试用户"/utf8>>,
        <<"nbf">> => Now - 60,
        <<"exp">> => Now + 600,
        <<"video">> => #{
            <<"room">> => <<"rtc_group_200">>,
            <<"roomJoin">> => true,
            <<"canPublish">> => true,
            <<"canSubscribe">> => true,
            <<"canPublishData">> => true
        }
    },
    Token = imboy_jwt:sign(Claims, ?SECRET),
    {ok, Got} = imboy_jwt:verify(Token, ?SECRET),
    ?assertEqual(Claims, Got).

tsid_64bit_precision_test() ->
    %% TSID 是 64-bit 整数，JSON 往返不得丢精度
    Tsid = 759982649382793728,
    Token = imboy_jwt:sign(#{<<"uid">> => Tsid, <<"exp">> => ?FUTURE}, ?SECRET),
    {ok, Claims} = imboy_jwt:verify(Token, ?SECRET),
    ?assertEqual(Tsid, maps:get(<<"uid">>, Claims)).

header_is_jwt_hs256_test() ->
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, ?SECRET),
    [H, _, _] = binary:split(Token, <<".">>, [global]),
    %% 手工 base64url 解 header 断言 alg/typ（decode 用 OTP 内置 urlsafe 模式）
    Header = jsone:decode(base64:decode(H, #{mode => urlsafe, padding => false})),
    ?assertEqual(<<"HS256">>, maps:get(<<"alg">>, Header)),
    ?assertEqual(<<"JWT">>, maps:get(<<"typ">>, Header)).

%%%-------------------------------------------------------------------
%%% 验签失败路径（篡改/混淆类均为精确断言，锁定错误 tag 防回归）
%%%-------------------------------------------------------------------

wrong_secret_test() ->
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, ?SECRET),
    ?assertEqual(
        {error, invalid_signature},
        imboy_jwt:verify(Token, ?OTHER_SECRET)
    ).

tampered_payload_test() ->
    Claims = #{<<"sub">> => <<"tk">>, <<"uid">> => 1, <<"exp">> => ?FUTURE},
    Token = imboy_jwt:sign(Claims, ?SECRET),
    [H, _, S] = binary:split(Token, <<".">>, [global]),
    %% 重签一个 uid 不同的 token，取其 payload 段（base64url 合法）嫁接到原签名上
    [_, OtherP, _] = binary:split(
        imboy_jwt:sign(#{<<"sub">> => <<"tk">>, <<"uid">> => 2, <<"exp">> => ?FUTURE}, ?SECRET),
        <<".">>,
        [global]
    ),
    ?assertEqual(
        {error, invalid_signature},
        imboy_jwt:verify(<<H/binary, ".", OtherP/binary, ".", S/binary>>, ?SECRET)
    ).

tampered_signature_test() ->
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, ?SECRET),
    [H, P, S] = binary:split(Token, <<".">>, [global]),
    Flip =
        case S of
            <<First, Rest/binary>> when First >= $A, First =< $Z ->
                <<($a + First - $A), Rest/binary>>;
            <<First, Rest/binary>> when First >= $a, First =< $z ->
                <<($A + First - $a), Rest/binary>>;
            Other ->
                Other
        end,
    ?assertEqual(
        {error, invalid_signature},
        imboy_jwt:verify(<<H/binary, ".", P/binary, ".", Flip/binary>>, ?SECRET)
    ).

alg_confusion_hs512_test() ->
    %% 用同一 secret 以 HS512 签发（header 声明 HS512），HS256-only 白名单必须
    %% 拒绝；jose verify_strict 对白名单外 alg 返回 false -> invalid_signature
    JWK = jose_jwk:from_oct(?SECRET),
    {_, SignedMap} = jose_jwt:sign(JWK, #{<<"alg">> => <<"HS512">>}, #{<<"exp">> => ?FUTURE}),
    {_, Token} = jose_jws:compact(SignedMap),
    ?assertEqual({error, invalid_signature}, imboy_jwt:verify(Token, ?SECRET)).

alg_none_rejected_test() ->
    %% alg=none 无签名 token（经典 JWT 攻击形态；旧 jwerl shim 是不验签直接
    %% 放行的，本模块修复为拒绝——用精确断言钉死该收紧点）
    H = base64:encode(
        <<"{\"alg\":\"none\",\"typ\":\"JWT\"}">>,
        #{mode => urlsafe, padding => false}
    ),
    P = base64:encode(
        <<"{\"sub\":\"tk\",\"uid\":1,\"exp\":9999999999}">>,
        #{mode => urlsafe, padding => false}
    ),
    ?assertEqual(
        {error, invalid_signature},
        imboy_jwt:verify(<<H/binary, ".", P/binary, ".">>, ?SECRET)
    ).

malformed_inputs_test() ->
    ?assertEqual({error, invalid}, imboy_jwt:verify(<<"not.a.jwt">>, ?SECRET)),
    ?assertEqual({error, invalid}, imboy_jwt:verify(<<"two.segments">>, ?SECRET)),
    ?assertEqual({error, invalid}, imboy_jwt:verify(<<>>, ?SECRET)),
    ?assertEqual({error, invalid}, imboy_jwt:verify(<<"!!!.@@@.###">>, ?SECRET)),
    %% 前后空白（防 jose 未来对空白容错的变化）
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, ?SECRET),
    ?assertEqual({error, invalid}, imboy_jwt:verify(<<$\s, Token/binary>>, ?SECRET)),
    ?assertEqual({error, invalid}, imboy_jwt:verify(<<Token/binary, $\n>>, ?SECRET)).

%%%-------------------------------------------------------------------
%%% 类型收紧 guard 回归锁
%%%-------------------------------------------------------------------

map_form_token_guard_test() ->
    %% jose verify_strict 原生接受 general-JWS map 输入且验签通过；
    %% imboy_jwt 的 is_binary(Token) guard 是唯一防线（guard 在函数头，
    %% 不会被函数体内 try 捕获）。若未来 guard 被放宽，此用例立刻红。
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, ?SECRET),
    [H, P, S] = binary:split(Token, <<".">>, [global]),
    MapForm = #{<<"protected">> => H, <<"payload">> => P, <<"signature">> => S},
    ?assertException(error, function_clause, imboy_jwt:verify(MapForm, ?SECRET)).

empty_secret_cross_isolation_test() ->
    %% 空 secret 在 jose 下完全可用（自洽签验）；锁定"空 secret 不能跨验
    %% 真实 key 的 token"——若部署漏配 jwt_key（缺省 <<>>），攻击者不能用
    %% 空 key 伪造的 token 通过真实 key 的验签，反之亦然。
    RealToken = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, ?SECRET),
    ?assertEqual(
        {error, invalid_signature},
        imboy_jwt:verify(RealToken, <<>>)
    ),
    EmptyKeyToken = imboy_jwt:sign(#{<<"exp">> => ?FUTURE}, <<>>),
    ?assertEqual(
        {error, invalid_signature},
        imboy_jwt:verify(EmptyKeyToken, ?SECRET)
    ).

%%%-------------------------------------------------------------------
%%% 时间 claims 校验
%%%-------------------------------------------------------------------

expired_test() ->
    Token = imboy_jwt:sign(#{<<"exp">> => erlang:system_time(second) - 3600}, ?SECRET),
    ?assertEqual({error, expired}, imboy_jwt:verify(Token, ?SECRET)).

exp_leeway_test() ->
    Exp = erlang:system_time(second) - 100,
    Token = imboy_jwt:sign(#{<<"exp">> => Exp}, ?SECRET),
    %% leeway 内放行
    ?assertMatch({ok, _}, imboy_jwt:verify(Token, ?SECRET, #{exp_leeway => 200})),
    %% leeway 0（默认）拒绝
    ?assertEqual({error, expired}, imboy_jwt:verify(Token, ?SECRET)).

exp_value_type_edges_test() ->
    %% float exp（JSON 1.5 可解码）-> invalid（旧链路放行 float，此处收紧）
    T1 = imboy_jwt:sign(#{<<"exp">> => 9999999999.5}, ?SECRET),
    ?assertEqual({error, invalid}, imboy_jwt:verify(T1, ?SECRET)),
    %% 0 / 负数 exp -> expired（归 705 可刷新，而非 invalid——
    %% 锁定该语义防止未来误改破坏客户端 refresh 链）
    T2 = imboy_jwt:sign(#{<<"exp">> => 0}, ?SECRET),
    ?assertEqual({error, expired}, imboy_jwt:verify(T2, ?SECRET)),
    T3 = imboy_jwt:sign(#{<<"exp">> => -1}, ?SECRET),
    ?assertEqual({error, expired}, imboy_jwt:verify(T3, ?SECRET)).

nbf_in_future_test() ->
    Now = erlang:system_time(second),
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE, <<"nbf">> => Now + 3600}, ?SECRET),
    ?assertEqual({error, {invalid_claim, nbf}}, imboy_jwt:verify(Token, ?SECRET)),
    ?assertMatch({ok, _}, imboy_jwt:verify(Token, ?SECRET, #{nbf_leeway => 7200})).

iat_in_future_test() ->
    Now = erlang:system_time(second),
    Token = imboy_jwt:sign(#{<<"exp">> => ?FUTURE, <<"iat">> => Now + 3600}, ?SECRET),
    ?assertEqual({error, {invalid_claim, iat}}, imboy_jwt:verify(Token, ?SECRET)),
    ?assertMatch({ok, _}, imboy_jwt:verify(Token, ?SECRET, #{iat_leeway => 7200})).

missing_exp_test() ->
    %% 缺 exp -> expired（较旧链路收紧：旧 token_ds 的 `<<>> > Now` 在项序下
    %% 恒 true，缺 exp 实际被放行；自家签发恒写 exp，无兼容影响）
    Token = imboy_jwt:sign(#{<<"sub">> => <<"tk">>, <<"uid">> => 9}, ?SECRET),
    ?assertEqual({error, expired}, imboy_jwt:verify(Token, ?SECRET)).

non_integer_time_claims_test() ->
    T1 = imboy_jwt:sign(#{<<"exp">> => <<"not-an-int">>}, ?SECRET),
    ?assertEqual({error, invalid}, imboy_jwt:verify(T1, ?SECRET)),
    T2 = imboy_jwt:sign(#{<<"exp">> => ?FUTURE, <<"nbf">> => <<"x">>}, ?SECRET),
    ?assertEqual({error, invalid}, imboy_jwt:verify(T2, ?SECRET)),
    T3 = imboy_jwt:sign(#{<<"exp">> => ?FUTURE, <<"iat">> => <<"x">>}, ?SECRET),
    ?assertEqual({error, invalid}, imboy_jwt:verify(T3, ?SECRET)).

%%%-------------------------------------------------------------------
%%% 存量 token 兼容金丝雀（jwerl-shim 时代签发，2026-09-28 固化）
%%% secret = ?SECRET；移除 shim 后此测试持续守护新旧互验
%%%-------------------------------------------------------------------

legacy_valid_token_test() ->
    Token = <<
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
        ".eyJleHAiOjk5OTk5OTk5OTksInN1YiI6InRrIiwidWlkIjo0MiwiZGlkIjoiZGV2WCIsImVwIjozfQ"
        ".4SBoYu0oQFzwTRRDXtPBZy958CxKQi9enRP8SHkWGrU"
    >>,
    ?assertMatch(
        {ok, #{
            <<"sub">> := <<"tk">>,
            <<"uid">> := 42,
            <<"did">> := <<"devX">>,
            <<"ep">> := 3
        }},
        imboy_jwt:verify(Token, ?SECRET)
    ).

legacy_rtk_token_test() ->
    Token = <<
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
        ".eyJleHAiOjk5OTk5OTk5OTksInN1YiI6InJ0ayIsInVpZCI6MTMsImRpZCI6ImRldlkiLCJlcCI6MX0"
        ".q_9amaFs6ccrPicexkk7VeIjU0eC86gaZ3s5FNRqMDM"
    >>,
    ?assertMatch(
        {ok, #{
            <<"sub">> := <<"rtk">>,
            <<"uid">> := 13,
            <<"did">> := <<"devY">>,
            <<"ep">> := 1
        }},
        imboy_jwt:verify(Token, ?SECRET)
    ).

legacy_expired_token_test() ->
    Token = <<
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
        ".eyJleHAiOjEwMDAwMDAwMDAsInN1YiI6InRrIiwidWlkIjo3fQ"
        ".1RJsOlpbwvLmD34gnx40UvYJpR6aYY9Q2zntR3rw2TY"
    >>,
    ?assertEqual({error, expired}, imboy_jwt:verify(Token, ?SECRET)).

legacy_noexp_token_test() ->
    %% 旧链路对缺 exp token 实际放行（项序缺陷）；新链路收紧为 expired
    Token = <<
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
        ".eyJzdWIiOiJ0ayIsInVpZCI6OX0"
        ".ucdcsPigu5YIyXzKgAEFflodNqbWvIH1zAvS-uPkblA"
    >>,
    ?assertEqual({error, expired}, imboy_jwt:verify(Token, ?SECRET)).

legacy_badexp_token_test() ->
    Token = <<
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
        ".eyJleHAiOiJub3QtYW4taW50Iiwic3ViIjoidGsiLCJ1aWQiOjExfQ"
        ".8gm7weR0uyXY787fQGlqipKFOu2OUyxSBAKuRpk-vFs"
    >>,
    ?assertEqual({error, invalid}, imboy_jwt:verify(Token, ?SECRET)).
