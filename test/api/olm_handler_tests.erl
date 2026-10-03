%%% E2EE-013 设备所有权守卫（DT-01/02）：olm_handler:device_write_decision/2 纯谓词。
%%%
%%% 复现旧漏洞：crypto 写端点 device_id 取自 body，未与 token 绑定 DID 校验，
%%% 同账号任一设备 token 可覆盖别设备密钥。新守卫要求 body device_id 必须等于
%%% token 绑定 DID，legacy 无 DID token 一律 fail-closed。
%%%
%%% C01-wire（run-20261003-094804）：identity 上报端点换根双签名 HTTP 接线。
%%% wire 合同：POST /api/v1/e2ee/olm/identity body 携带非空
%%% transition_signature（snake_case，binary/list 均接受）→ handler 调
%%% olm_identity_logic:report_identity/7（第 6 位参数 = 过渡签名）；缺失/空串
%%% → 保持 /6 兼容入口（同根/首注册行为不变，换根由 logic 层显式拒绝
%%% key_rotation_requires_old_key_proof 并经 handler 原样透传）。
%%% 全部离线（?WITH_MECKS mock logic，无 PG 依赖；logic 层签名分流语义由
%%% olm_identity_logic_tests 承载，本文件只测 wire 分流与透传）。
-module(olm_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

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

%% ===================================================================
%% C01-wire：identity 上报双签名 HTTP 分流（离线，mock logic 层）
%% ===================================================================

-define(WIRE_UID, 100).
-define(WIRE_DID, <<"dev-A">>).

wire_identity_mocks(PostVals, LogicExpectations) ->
    [
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> true end}
        ]},
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> ?WIRE_UID end},
            {'current_did', 1, fun(_State) -> ?WIRE_DID end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> PostVals end}
        ]},
        {olm_identity_logic, LogicExpectations},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
            {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
        ]}
    ].

wire_identity_body(TransitionSig) ->
    #{
        <<"device_id">> => ?WIRE_DID,
        <<"device_type">> => <<"android">>,
        <<"ed25519_key">> => <<"new-ed-key">>,
        <<"curve25519_key">> => <<"new-curve-key">>,
        <<"signature">> => <<"new-self-sig">>,
        <<"transition_signature">> => TransitionSig
    }.

%% 换根 + 双签名经 HTTP 成功：带非空 transition_signature → 调 /7（第 6 位
%% 参数 = 过渡签名），不触碰 /6，HTTP 200。
rotation_with_double_signature_routes_to_arity7_test_() ->
    TransitionSig = <<"old-key-transition-sig">>,
    ?WITH_MECKS(
        wire_identity_mocks(
            wire_identity_body(TransitionSig),
            [
                {'report_identity', 7, fun(_Uid, _Did, _Ed, _Curve, _Sig, _TS, _Dt) ->
                    ok
                end},
                {'report_identity', 6, fun(_Uid, _Did, _Ed, _Curve, _Sig, _Dt) ->
                    {ok, should_not_reach_arity6}
                end}
            ]
        ),
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => report_identity}),
            ?assertEqual({responded, success}, Result),
            ?assertEqual(
                TransitionSig,
                meck:capture(first, olm_identity_logic, report_identity, 7, 6)
            ),
            ?assertEqual(0, meck:num_calls(olm_identity_logic, report_identity, 6))
        end
    ).

%% 换根无过渡签名被拒：不带 transition_signature → 走 /6，logic 拒绝
%% key_rotation_requires_old_key_proof，错误语义经 handler 原样透传（400）。
rotation_without_transition_signature_rejected_via_6_test_() ->
    ?WITH_MECKS(
        wire_identity_mocks(
            maps:remove(<<"transition_signature">>, wire_identity_body(<<"ignored">>)),
            [
                {'report_identity', 6, fun(_Uid, _Did, _Ed, _Curve, _Sig, _Dt) ->
                    {error, <<"key_rotation_requires_old_key_proof">>}
                end},
                {'report_identity', 7, fun(_, _, _, _, _, _, _) ->
                    ok
                end}
            ]
        ),
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => report_identity}),
            ?assertEqual(
                {responded, error, <<"key_rotation_requires_old_key_proof">>, ?ERR_BAD_REQUEST},
                Result
            ),
            ?assertEqual(0, meck:num_calls(olm_identity_logic, report_identity, 7))
        end
    ).

%% 同根/首注册不带过渡签名不受影响：旧客户端形态（无 transition_signature
%% 字段）→ /6 行为不变，成功路径照常 200。
same_root_first_registration_without_transition_unchanged_test_() ->
    ?WITH_MECKS(
        wire_identity_mocks(
            maps:remove(<<"transition_signature">>, wire_identity_body(<<"ignored">>)),
            [
                {'report_identity', 6, fun(
                    ?WIRE_UID,
                    ?WIRE_DID,
                    <<"new-ed-key">>,
                    <<"new-curve-key">>,
                    <<"new-self-sig">>,
                    <<"android">>
                ) ->
                    ok
                end},
                {'report_identity', 7, fun(_, _, _, _, _, _, _) -> ok end}
            ]
        ),
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => report_identity}),
            ?assertEqual({responded, success}, Result),
            ?assertEqual(1, meck:num_calls(olm_identity_logic, report_identity, 6)),
            ?assertEqual(0, meck:num_calls(olm_identity_logic, report_identity, 7))
        end
    ).

%% 显式空串 transition_signature 视为「无」：与字段缺失等价，走 /6
%% （logic 层 TransitionSignature = <<>> 语义）。
empty_transition_signature_treated_as_absent_test_() ->
    ?WITH_MECKS(
        wire_identity_mocks(
            wire_identity_body(<<>>),
            [
                {'report_identity', 6, fun(_Uid, _Did, _Ed, _Curve, _Sig, _Dt) -> ok end},
                {'report_identity', 7, fun(_, _, _, _, _, _, _) -> ok end}
            ]
        ),
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => report_identity}),
            ?assertEqual({responded, success}, Result),
            ?assertEqual(1, meck:num_calls(olm_identity_logic, report_identity, 6)),
            ?assertEqual(0, meck:num_calls(olm_identity_logic, report_identity, 7))
        end
    ).

%% list（string）形态的 transition_signature 经 to_bin 归一后按非空处理，
%% 走 /7 且 logic 收到 binary（与 fallback signature 同款归一化）。
string_form_transition_signature_normalized_test_() ->
    TransitionSig = <<"old-key-transition-sig">>,
    ?WITH_MECKS(
        wire_identity_mocks(
            wire_identity_body(binary_to_list(TransitionSig)),
            [
                {'report_identity', 7, fun(_Uid, _Did, _Ed, _Curve, _Sig, _TS, _Dt) ->
                    ok
                end},
                {'report_identity', 6, fun(_, _, _, _, _, _) -> ok end}
            ]
        ),
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => report_identity}),
            ?assertEqual({responded, success}, Result),
            ?assertEqual(
                TransitionSig,
                meck:capture(first, olm_identity_logic, report_identity, 7, 6)
            )
        end
    ).
