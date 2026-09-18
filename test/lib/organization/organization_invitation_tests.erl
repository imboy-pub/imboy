-module(organization_invitation_tests).

%% C11 Invitation 领域规则纯函数测试（无 IO / 无 mock）：
%% token/digest 形态与确定性、状态机、expiry、accept 裁决分支、create 参量校验。

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% token / digest（C11：明文只返回一次，库中只有 digest）
%%--------------------------------------------------------------------

new_token_test_() ->
    [
        {"明文 token 为 64 hex 字符",
            ?_assertMatch(<<_:64/binary>>, organization_invitation:new_token())},
        {"明文 token 全部为小写 hex",
            ?_assert(
                organization_invitation:valid_token_digest(
                    organization_invitation:new_token()
                )
            )},
        {"两次生成不重复（256-bit 熵）",
            ?_assertNotEqual(
                organization_invitation:new_token(),
                organization_invitation:new_token()
            )}
    ].

token_digest_test_() ->
    KnownVector = <<"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad">>,
    [
        {"sha256(\"abc\") 标准测试向量",
            ?_assertEqual(KnownVector, organization_invitation:token_digest(<<"abc">>))},
        {"digest 确定性（同输入同输出）",
            ?_assertEqual(
                organization_invitation:token_digest(<<"t1">>),
                organization_invitation:token_digest(<<"t1">>)
            )},
        {"digest 与明文不同且长度 64", begin
            Token2 = organization_invitation:new_token(),
            D2 = organization_invitation:token_digest(Token2),
            [?_assertNotEqual(Token2, D2), ?_assertEqual(64, byte_size(D2))]
        end},
        {"digest 不含明文任何子串前 16 字节", begin
            Token3 = organization_invitation:new_token(),
            D3 = organization_invitation:token_digest(Token3),
            ?_assertEqual(nomatch, binary:match(D3, binary:part(Token3, 0, 16)))
        end}
    ].

valid_token_digest_test_() ->
    Good = organization_invitation:token_digest(<<"x">>),
    <<_, Rest63/binary>> = Good,
    UpperFirst = <<"F", Rest63/binary>>,
    [
        {"合法 digest 放行", ?_assert(organization_invitation:valid_token_digest(Good))},
        {"大写 hex 拒绝（迁移 CHECK 同口径小写）",
            ?_assertNot(organization_invitation:valid_token_digest(UpperFirst))},
        {"长度不足 64 拒绝",
            ?_assertNot(organization_invitation:valid_token_digest(binary:part(Good, 0, 63)))},
        {"非 hex 字符拒绝",
            ?_assertNot(
                organization_invitation:valid_token_digest(
                    <<"g2345678901234567890123456789012345678901234567890123456789012345">>
                )
            )},
        {"非 binary 拒绝", ?_assertNot(organization_invitation:valid_token_digest(12345))}
    ].

%%--------------------------------------------------------------------
%% 状态机（C11：pending → 终态，一次性）
%%--------------------------------------------------------------------

is_terminal_test_() ->
    [
        {"pending 非终态", ?_assertNot(organization_invitation:is_terminal(pending))},
        {"accepted/rejected/expired/revoked 均终态",
            ?_assert(
                lists:all(
                    fun(S) ->
                        organization_invitation:is_terminal(S),
                        organization_invitation:is_terminal(atom_to_binary(S))
                    end,
                    [accepted, rejected, expired, revoked]
                )
            )},
        {"未知状态非终态", ?_assertNot(organization_invitation:is_terminal(unknown))}
    ].

can_transition_test_() ->
    [
        {"pending → accepted 合法",
            ?_assert(organization_invitation:can_transition(pending, accepted))},
        {"pending → expired / revoked / rejected 合法",
            ?_assert(
                organization_invitation:can_transition(<<"pending">>, <<"expired">>) andalso
                    organization_invitation:can_transition(<<"pending">>, <<"revoked">>) andalso
                    organization_invitation:can_transition(<<"pending">>, <<"rejected">>)
            )},
        {"终态间迁移非法（accepted → revoked）",
            ?_assertNot(organization_invitation:can_transition(accepted, revoked))},
        {"终态回 pending 非法", ?_assertNot(organization_invitation:can_transition(rejected, pending))}
    ].

%%--------------------------------------------------------------------
%% expiry（lazy expire）
%%--------------------------------------------------------------------

expiry_test_() ->
    Now = 1700000000,
    [
        {"默认有效期 = 7 天", ?_assertEqual(7 * 86400, organization_invitation:default_expiry_seconds())},
        {"expires_at_from_now = now + 7 天",
            ?_assertEqual(
                Now + 7 * 86400,
                organization_invitation:expires_at_from_now(Now)
            )},
        {"expires_at <= now 视为过期", [
            ?_assert(organization_invitation:is_expired(Now, Now)),
            ?_assert(organization_invitation:is_expired(Now - 1, Now))
        ]},
        {"expires_at > now 未过期", ?_assertNot(organization_invitation:is_expired(Now + 1, Now))},
        {"未知 expires_at 不判过期（由 DB sweep 裁决）",
            ?_assertNot(organization_invitation:is_expired(undefined, Now))}
    ].

classify_accept_test_() ->
    Now = 1700000000,
    Row = fun(Status, ExpiresAt) ->
        #{<<"status">> => Status, <<"expires_at">> => ExpiresAt}
    end,
    [
        {"pending 未过期 → ok",
            ?_assertEqual(
                ok, organization_invitation:classify_accept(Now, Row(<<"pending">>, Now + 1))
            )},
        {"pending 已到期 → expired",
            ?_assertEqual(
                expired, organization_invitation:classify_accept(Now, Row(<<"pending">>, Now))
            )},
        {"accepted → replay_accepted（幂等分支）",
            ?_assertEqual(
                replay_accepted,
                organization_invitation:classify_accept(Now, Row(<<"accepted">>, Now + 100))
            )},
        {"rejected → rejected",
            ?_assertEqual(
                rejected,
                organization_invitation:classify_accept(Now, Row(<<"rejected">>, Now + 100))
            )},
        {"revoked → revoked",
            ?_assertEqual(
                revoked,
                organization_invitation:classify_accept(Now, Row(<<"revoked">>, Now + 100))
            )},
        {"expired → expired",
            ?_assertEqual(
                expired,
                organization_invitation:classify_accept(Now, Row(<<"expired">>, Now - 5))
            )}
    ].

%%--------------------------------------------------------------------
%% create 参量校验
%%--------------------------------------------------------------------

valid_create_test_() ->
    [
        {"正整数三元组放行", ?_assertEqual(ok, organization_invitation:valid_create(1, 2, 3))},
        {"零/负数被 400 拒绝",
            ?_assertMatch(
                {error, {400, _}},
                organization_invitation:valid_create(0, 2, 3)
            )},
        {"非整数被 400 拒绝",
            ?_assertMatch(
                {error, {400, _}},
                organization_invitation:valid_create(<<"1">>, 2, 3)
            )}
    ].
