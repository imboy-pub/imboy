-module(eb_asset_download_ticket_tests).
-moduledoc "V3-17：EB 域下载票据单元断言（purpose 分离/过期/篡改/claims 形状）".
%%%
%%% G17 的票据面窄口径：签发→验证往返、可选 claims（绑定门）往返、
%%% 非法 claims fail-closed、跨 purpose（主域票据）互不接受、
%%% 篡改/畸形输入统一 invalid。端到端见 eb_asset_edge_gate（真 Garage）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, <<"synthetic-secret-32-bytes-aaaaaaaaaa">>).

roundtrip_test() ->
    Claims = #{
        org_id => 4782288301000046,
        workspace_id => 4782288301000047,
        asset_id => 121353613336643584,
        actor_kind => member,
        actor_id => 990123
    },
    {ok, T} = eb_asset_download_ticket:issue(Claims, ?SECRET),
    {ok, Verified} = eb_asset_download_ticket:verify(T, ?SECRET),
    ?assertEqual(
        #{
            org_id => 4782288301000046,
            workspace_id => 4782288301000047,
            asset_id => 121353613336643584,
            actor_kind => member,
            actor_id => 990123
        },
        Verified
    ).

optional_claims_roundtrip_test() ->
    Claims = #{
        org_id => 1,
        workspace_id => 2,
        asset_id => 3,
        actor_kind => contact,
        actor_id => 4,
        conversation_id => 5,
        visitor_session_id => 6,
        visitor_token_id => 7,
        visitor_installation_id => 8
    },
    {ok, T} = eb_asset_download_ticket:issue(Claims, ?SECRET),
    {ok, Verified} = eb_asset_download_ticket:verify(T, ?SECRET),
    %% 绑定门重放键（atom）必须原样回传
    ?assertEqual(5, maps:get(conversation_id, Verified)),
    ?assertEqual(6, maps:get(visitor_session_id, Verified)),
    ?assertEqual(7, maps:get(visitor_token_id, Verified)),
    ?assertEqual(8, maps:get(visitor_installation_id, Verified)),
    ?assertEqual(contact, maps:get(actor_kind, Verified)).

invalid_claims_fail_closed_test() ->
    Base = #{org_id => 1, workspace_id => 2, asset_id => 3, actor_kind => member, actor_id => 4},
    ?assertEqual(
        {error, invalid_claims}, eb_asset_download_ticket:issue(Base#{actor_kind => weird}, ?SECRET)
    ),
    ?assertEqual(
        {error, invalid_claims}, eb_asset_download_ticket:issue(Base#{org_id => 0}, ?SECRET)
    ),
    ?assertEqual(
        {error, invalid_claims}, eb_asset_download_ticket:issue(Base#{asset_id => <<"x">>}, ?SECRET)
    ),
    ?assertMatch({error, _}, eb_asset_download_ticket:issue(Base#{}, <<>>)).

visitor_claims_required_test() ->
    Base = #{
        org_id => 1,
        workspace_id => 2,
        asset_id => 3,
        actor_kind => contact,
        actor_id => 4,
        conversation_id => 5,
        visitor_session_id => 6
    },
    ?assertEqual({error, invalid_claims}, eb_asset_download_ticket:issue(Base, ?SECRET)).

cross_purpose_rejected_test() ->
    %% 主域票据（attachment_download purpose）不得被 EB verify 接受
    {ok, MainTicket} = attachment_download_ticket:issue(990123, <<"u1/x">>, ?SECRET),
    ?assertEqual({error, invalid}, eb_asset_download_ticket:verify(MainTicket, ?SECRET)).

tampered_and_malformed_rejected_test() ->
    Claims = #{org_id => 1, workspace_id => 2, asset_id => 3, actor_kind => member, actor_id => 4},
    {ok, T} = eb_asset_download_ticket:issue(Claims, ?SECRET),
    %% 错密钥（换 purpose 派生即不同 HMAC）
    ?assertEqual(
        {error, invalid}, eb_asset_download_ticket:verify(T, <<"other-secret-32-bytes-bbbbbbbbb">>)
    ),
    %% 篡改载荷（翻转中段一字节：签名/载荷任一破坏都必须拒绝）
    Tampered = flip_byte(T),
    ?assertMatch({error, _}, eb_asset_download_ticket:verify(Tampered, ?SECRET)),
    %% 畸形输入
    ?assertEqual({error, invalid}, eb_asset_download_ticket:verify(<<>>, ?SECRET)),
    ?assertEqual({error, invalid}, eb_asset_download_ticket:verify(<<"not-a-jwt">>, ?SECRET)).

flip_byte(<<A:10/binary, B:8, Rest/binary>>) ->
    <<A/binary, (B bxor 16#01), Rest/binary>>.
