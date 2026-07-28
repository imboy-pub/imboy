%%% E2EE-062 第五刀：**OTK 余量查询端点**（残留 ③ 的第一小刀）。
%%%
%%% == 缺口 ==
%%%
%%% 前四刀把幂等租约与目标级限流铺满了 claim 路径。其中「限流只拖慢、靠补传
%%% 恢复」这条取舍（见 evidence/E2EE-062-per-target-throttle.md §1.1 取舍二）
%%% 有一个**前提**：客户端能知道自己的 OTK 池见底并及时补传。
%%%
%%% 该前提今天不成立。客户端 `OlmApi.countPrekeys`（imboyapp
%%% lib/store/api/olm_api.dart）是**恒返回 0 的桩实现**，注释自承
%%% 「准确的服务端计数需后端补 count 端点」。服务端 `olm_identity_repo` /
%%% `olm_identity_ds` 的 `count_one_time_keys/2` 早已存在（真 PG 集成测试在用），
%%% 但**没有 logic 与 handler 承载点，也没有路由**——能力有，出口没有。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. `GET /api/v1/e2ee/olm/prekey_count` 必须返回**当前用户当前设备**的余量；
%%% 2. 【安全】设备标识只取自 token，**不接受任何入参**——否则该端点就是一个
%%%    「探测谁的池快空了」的接口，正好给耗尽攻击提供命中时机；
%%% 3. 【安全 / fail-closed】token 未绑定设备（legacy）→ 403，且**不得到达 logic**；
%%% 4. e2ee 关闭时该端点一并禁用（与其余 olm 端点同一 capability gate）；
%%% 5. 【对照组】未知 action 仍返回 404。
-module(e2ee_otk_count_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 5001).
-define(DID, <<"dev-self-A">>).

%% ===================================================================
%% 对照组：未知 action 仍走 404 分支（改前改后都必须绿）
%% ===================================================================

unknown_action_still_404_test_() ->
    ?WITH_MECKS(
        [
            {elib_response, [
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => no_such_action}),
            ?assertEqual({responded, error, <<"not_found">>, 404}, Result)
        end
    ).

%% ===================================================================
%% 1. 正向：返回当前用户当前设备的余量
%% ===================================================================

prekey_count_returns_own_count_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> ?UID end},
                {'current_did', 1, fun(_State) -> ?DID end}
            ]},
            {olm_identity_logic, [
                {'count_one_time_keys', 2, fun(Uid, Did) ->
                    ?assertEqual(?UID, Uid),
                    ?assertEqual(?DID, Did),
                    {ok, 17}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => prekey_count}),
            ?assertEqual({responded, #{<<"count">> => 17}}, Result)
        end
    ).

%% 余量为 0 也必须是成功响应（这正是补传要等的信号，不能当错误吞掉）
prekey_count_zero_is_success_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> ?UID end},
                {'current_did', 1, fun(_State) -> ?DID end}
            ]},
            {olm_identity_logic, [
                {'count_one_time_keys', 2, fun(_Uid, _Did) -> {ok, 0} end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => prekey_count}),
            ?assertEqual({responded, #{<<"count">> => 0}}, Result)
        end
    ).

%% ===================================================================
%% 2. 安全：设备只取自 token，入参一律不得影响查询对象
%% ===================================================================

%% 请求里塞入别人的 uid / device_id，查询对象必须仍是 token 的那一对。
%% 否则该端点就是「探测目标池余量」的接口，直接配合耗尽攻击择时。
prekey_count_ignores_request_params_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> ?UID end},
                {'current_did', 1, fun(_State) -> ?DID end}
            ]},
            {elib_param, [
                {'get', 3, fun(_K, _Req, Default) -> Default end},
                {'post', 1, fun(_Req) ->
                    #{<<"uid">> => <<"9999">>, <<"device_id">> => <<"dev-victim">>}
                end}
            ]},
            {olm_identity_logic, [
                {'count_one_time_keys', 2, fun(Uid, Did) ->
                    ?assertEqual(
                        {?UID, ?DID},
                        {Uid, Did}
                    ),
                    {ok, 3}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => prekey_count}),
            ?assertEqual({responded, #{<<"count">> => 3}}, Result)
        end
    ).

%% ===================================================================
%% 3. fail-closed：token 未绑定设备 → 403，且不得到达 logic
%% ===================================================================

prekey_count_requires_device_binding_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> ?UID end},
                %% legacy token：未绑定 DID
                {'current_did', 1, fun(_State) -> <<>> end}
            ]},
            {olm_identity_logic, [
                {'count_one_time_keys', 2, fun(_Uid, _Did) ->
                    erlang:error(must_not_reach_logic_without_device_binding)
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => prekey_count}),
            ?assertEqual({responded, error, <<"device_binding_required">>, 403}, Result)
        end
    ).

%% ===================================================================
%% 4. capability gate：e2ee 关闭时一并禁用
%% ===================================================================

prekey_count_respects_e2ee_gate_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> false end}]},
            {olm_identity_logic, [
                {'count_one_time_keys', 2, fun(_Uid, _Did) ->
                    erlang:error(must_not_reach_logic_when_e2ee_disabled)
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => prekey_count}),
            %% 必须是 capability gate 的 5190，不能是「action 不存在」的 404——
            %% 只断言「是个 error」的话，端点根本没实现时也会假绿。
            ?assertMatch({responded, error, _, ?ERR_FEATURE_DISABLED}, Result)
        end
    ).

%% ===================================================================
%% 5. 下层出错不得泄漏细节，也不得伪装成 count=0
%% ===================================================================

%% count=0 是「该补传了」的信号；把查询失败也报成 0 会触发无谓的全量补传，
%% 更糟的是让真正的池见底与数据库故障无法区分。
prekey_count_error_is_not_zero_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> ?UID end},
                {'current_did', 1, fun(_State) -> ?DID end}
            ]},
            {olm_identity_logic, [
                {'count_one_time_keys', 2, fun(_Uid, _Did) -> {error, <<"internal_error">>} end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => prekey_count}),
            ?assertMatch({responded, error, <<"internal_error">>, _}, Result)
        end
    ).
