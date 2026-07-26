%%% S1.3 设备所有权守卫扩展（DT-03）：e2ee_handler:report_device_key 必须校验
%%% body device_id == token 绑定 DID。
%%%
%%% 复现旧漏洞：e2ee_handler 的 report_device_key 端点 device_id 取自 body，
%%% 未与 token 绑定 DID 校验。同账号设备 A 的 token 可以为设备 B 上报公钥，
%%% 造成密钥覆盖/身份混淆。
%%%
%%% 修复后行为：
%%%   - token DID=A, body device_id=B → 403 device_mismatch
%%%   - token DID 空（legacy）→ 403 device_binding_required
%%%   - token DID=A, body device_id=A → 允许
-module(e2ee_handler_device_binding_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% DT-03：report_device_key 设备所有权
%% ===================================================================

%% 核心漏洞复现：token 绑定 dev-A，body 声称 dev-B → 必须 403。
%% 当前代码不校验，此测试在修复前 FAIL。
report_key_cross_device_must_403_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 123 end},
                {'current_did', 1, fun(_State) -> <<"dev-A">> end}
            ]},
            {throttle, [
                {'check', 2, fun(_Key, _Uid) -> ok end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"device_id">> => <<"dev-B">>,
                        <<"device_type">> => <<"android">>,
                        <<"device_name">> => <<"Pixel">>,
                        <<"public_key">> => <<"pk-base64">>,
                        <<"key_id">> => <<"kid-1">>
                    }
                end}
            ]},
            {e2ee_logic, [
                {'report_device_key', 6, fun(_Uid, _Did, _Type, _Name, _Pk, _Kid) ->
                    %% 如果走到这里说明守卫缺失——不应该到达
                    {ok, 0}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = e2ee_handler:init(Req0, #{
                action => report_device_key, current_uid => 123, current_did => <<"dev-A">>
            }),
            %% 修复后期望：返回 403 device_mismatch，不调用 e2ee_logic
            ?assertEqual({responded, error, <<"device_mismatch">>, 403}, Result)
        end
    ).

%% legacy 无 DID token → fail-closed 403 device_binding_required。
report_key_legacy_unbound_must_403_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 123 end},
                {'current_did', 1, fun(_State) -> <<>> end}
            ]},
            {throttle, [
                {'check', 2, fun(_Key, _Uid) -> ok end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"device_id">> => <<"dev-A">>,
                        <<"device_type">> => <<"ios">>,
                        <<"device_name">> => <<"iPhone">>,
                        <<"public_key">> => <<"pk-base64">>,
                        <<"key_id">> => <<"kid-2">>
                    }
                end}
            ]},
            {e2ee_logic, [
                {'report_device_key', 6, fun(_Uid, _Did, _Type, _Name, _Pk, _Kid) ->
                    {ok, 0}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = e2ee_handler:init(Req0, #{
                action => report_device_key, current_uid => 123, current_did => <<>>
            }),
            ?assertEqual({responded, error, <<"device_binding_required">>, 403}, Result)
        end
    ).

%% 正向：token DID == body device_id → 允许，调用 logic 层。
report_key_matching_device_allowed_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 123 end},
                {'current_did', 1, fun(_State) -> <<"dev-A">> end}
            ]},
            {throttle, [
                {'check', 2, fun(_Key, _Uid) -> ok end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"device_id">> => <<"dev-A">>,
                        <<"device_type">> => <<"android">>,
                        <<"device_name">> => <<"Pixel">>,
                        <<"public_key">> => <<"pk-base64">>,
                        <<"key_id">> => <<"kid-3">>
                    }
                end}
            ]},
            {e2ee_logic, [
                {'report_device_key', 6, fun(123, <<"dev-A">>, _Type, _Name, _Pk, _Kid) ->
                    {ok, 2}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = e2ee_handler:init(Req0, #{
                action => report_device_key, current_uid => 123, current_did => <<"dev-A">>
            }),
            ?assertEqual({responded, success}, Result)
        end
    ).
