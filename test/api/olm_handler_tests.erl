%%% E2EE-013 设备所有权守卫（DT-01/02）：olm_handler:device_write_decision/2 纯谓词。
%%%
%%% 复现旧漏洞：crypto 写端点 device_id 取自 body，未与 token 绑定 DID 校验，
%%% 同账号任一设备 token 可覆盖别设备密钥。新守卫要求 body device_id 必须等于
%%% token 绑定 DID，legacy 无 DID token 一律 fail-closed。
-module(olm_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%% DT-02：token 绑定设备 A（DidA），body device_id=B（DidB）→ device_mismatch（对应 403）。
a_token_cannot_write_b_device_test() ->
    ?assertEqual(
        device_mismatch,
        olm_handler:device_write_decision(<<"dev-A">>, <<"dev-B">>)
    ).

%% DT-01：token 绑定 DID 与 body device_id 一致 → 允许。
matching_device_allowed_test() ->
    ?assertEqual(
        ok,
        olm_handler:device_write_decision(<<"dev-A">>, <<"dev-A">>)
    ).

%% legacy 无绑定 token（DID 空）→ fail-closed，即使 body 提供 device_id。
legacy_unbound_token_fail_closed_test() ->
    ?assertEqual(
        device_binding_required,
        olm_handler:device_write_decision(<<>>, <<"dev-A">>)
    ),
    ?assertEqual(
        device_binding_required,
        olm_handler:device_write_decision(<<>>, <<>>)
    ).

%% body device_id 缺失/空（绕过尝试）→ device_mismatch，不允许空设备写。
empty_body_device_rejected_test() ->
    ?assertEqual(
        device_mismatch,
        olm_handler:device_write_decision(<<"dev-A">>, <<>>)
    ).

%% 长/Unicode 混淆的 body device_id 只要 ≠ 绑定 DID 即拒绝（不做归一化放行）。
unicode_confusable_mismatch_test() ->
    ?assertEqual(
        device_mismatch,
        olm_handler:device_write_decision(<<"dev-A">>, <<"dev-А"/utf8>>)
    ).
