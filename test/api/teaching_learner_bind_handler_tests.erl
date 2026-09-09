%% teaching_learner_bind_handler_tests
%% Step 16（B 接线）— 学员账号绑定 handler 契约测试 + logic 层审计接线断言。
%%
%% 直接模块调用（不起 cowboy，模式照 project_member_handler_tests）：
%%   handler —— bind/unbind 参数解析（path :id、body user_id 整数/字符串）、
%%              logic reason → HTTP status + envelope code 映射
%%              （403/5429、404/404、409/5427、422/5428、409/409、500/1）
%%   logic  —— bind/unbind 成功路径在同一事务调用 teaching_admin_audit
%%              INSERT（00000099 接线）；被拒路径不调；审计写失败 fail-closed
%%
%% 全部 meck，零真实网络、零真实库。

-module(teaching_learner_bind_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 990001).
-define(LEARNER_ID, 994001).
-define(TARGET_UID, 990006).

%%%===================================================================
%%% Handler 契约：mock 基建
%%%===================================================================

%% BindingId：path :id 的返回值；Body：elib_param:post 的返回值
handler_mocks(BindingId, Body) ->
    [
        {cowboy_req, [
            {'binding', 2, fun(id, _Req) -> BindingId end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload, _Msg) ->
                #{resp => success, payload => Payload}
            end},
            {'error_with_status', 4, fun(_Req, Status, _Msg, Code) ->
                #{resp => error, http => Status, code => Code}
            end}
        ]}
    ].

bind_logic_mock(Return) ->
    {teaching_learner_bind_logic, [
        {'bind_learner', 3, fun(Uid, LearnerId, TargetUid) ->
            self() ! {logic_bind, Uid, LearnerId, TargetUid},
            Return
        end}
    ]}.

unbind_logic_mock(Return) ->
    {teaching_learner_bind_logic, [
        {'unbind_learner', 2, fun(Uid, LearnerId) ->
            self() ! {logic_unbind, Uid, LearnerId},
            Return
        end}
    ]}.

ok_row() ->
    #{<<"id">> => ?LEARNER_ID, <<"user_id">> => ?TARGET_UID}.

%%%===================================================================
%%% Handler：bind 成功 + 参数解析
%%%===================================================================

bind_success_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"994001">>, #{<<"user_id">> => ?TARGET_UID}) ++
            [bind_logic_mock({ok, ok_row()})],
        fun() ->
            Req = teaching_learner_bind_handler:handle_action(
                bind,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            %% 契约硬规则1：64-bit ID 字段一律 string（R8 实测修正后断言）
            ?assertEqual(integer_to_binary(?LEARNER_ID), maps:get(<<"id">>, Payload)),
            receive
                {logic_bind, ?UID, ?LEARNER_ID, ?TARGET_UID} -> ok
            after 0 -> ?assert(false, "logic bind_learner not called with parsed args")
            end
        end
    ).

%% body user_id 字符串形态（JSON 数字字符串）→ 解析为 integer 传 logic
bind_user_id_string_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"994001">>, #{<<"user_id">> => <<"990006">>}) ++
            [bind_logic_mock({ok, ok_row()})],
        fun() ->
            _ = teaching_learner_bind_handler:handle_action(
                bind,
                req0,
                #{current_uid => ?UID}
            ),
            receive
                {logic_bind, ?UID, ?LEARNER_ID, ?TARGET_UID} -> ok
            after 0 -> ?assert(false, "string user_id not parsed to integer")
            end
        end
    ).

bind_missing_user_id_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"994001">>, #{}) ++ [bind_logic_mock({ok, ok_row()})],
        fun() ->
            Req = teaching_learner_bind_handler:handle_action(
                bind,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req)),
            ?assertEqual(?ERR_MISSING_PARAM, maps:get(code, Req))
        end
    ).

bind_bad_path_id_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"not-an-int">>, #{<<"user_id">> => ?TARGET_UID}) ++
            [bind_logic_mock({ok, ok_row()})],
        fun() ->
            Req = teaching_learner_bind_handler:handle_action(
                bind,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req))
        end
    ).

%%%===================================================================
%%% Handler：错误映射矩阵（reason → HTTP status + envelope code）
%%%===================================================================

bind_error_mapping_test_() ->
    Cases = [
        {not_authorized, 403, ?ERR_TEACHING_BIND_NOT_AUTHORIZED},
        {learner_not_found, 404, ?ERR_NOT_FOUND},
        {learner_inactive, 404, ?ERR_NOT_FOUND},
        {duplicate_bind_in_org, 409, ?ERR_TEACHING_BIND_DUPLICATE_IN_ORG},
        {invalid_target_user, 422, ?ERR_TEACHING_BIND_INVALID_TARGET},
        {db_error, 500, ?ERR_ERROR}
    ],
    [
        begin
            {Reason, Http, Code} = Case,
            {
                lists:flatten(io_lib:format("bind ~p -> ~p/~p", [Reason, Http, Code])),
                ?WITH_MECKS(
                    handler_mocks(<<"994001">>, #{<<"user_id">> => ?TARGET_UID}) ++
                        [bind_logic_mock({error, Reason})],
                    fun() ->
                        Req = teaching_learner_bind_handler:handle_action(
                            bind,
                            req0,
                            #{current_uid => ?UID}
                        ),
                        ?assertEqual(error, maps:get(resp, Req)),
                        ?assertEqual(Http, maps:get(http, Req)),
                        ?assertEqual(Code, maps:get(code, Req))
                    end
                )
            }
        end
     || Case <- Cases
    ].

unbind_success_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"994001">>, #{}) ++
            [unbind_logic_mock({ok, #{<<"id">> => ?LEARNER_ID, <<"user_id">> => null}})],
        fun() ->
            Req = teaching_learner_bind_handler:handle_action(
                unbind,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_unbind, ?UID, ?LEARNER_ID} -> ok
            after 0 -> ?assert(false, "logic unbind_learner not called")
            end
        end
    ).

unbind_error_mapping_test_() ->
    Cases = [
        {not_authorized, 403, ?ERR_TEACHING_BIND_NOT_AUTHORIZED},
        {learner_not_found, 404, ?ERR_NOT_FOUND},
        {not_bound, 409, ?ERR_CONFLICT},
        {db_error, 500, ?ERR_ERROR}
    ],
    [
        begin
            {Reason, Http, Code} = Case,
            {
                lists:flatten(io_lib:format("unbind ~p -> ~p/~p", [Reason, Http, Code])),
                ?WITH_MECKS(
                    handler_mocks(<<"994001">>, #{}) ++
                        [unbind_logic_mock({error, Reason})],
                    fun() ->
                        Req = teaching_learner_bind_handler:handle_action(
                            unbind,
                            req0,
                            #{current_uid => ?UID}
                        ),
                        ?assertEqual(error, maps:get(resp, Req)),
                        ?assertEqual(Http, maps:get(http, Req)),
                        ?assertEqual(Code, maps:get(code, Req))
                    end
                )
            }
        end
     || Case <- Cases
    ].

%%%===================================================================
%%% Logic 审计接线（00000099 teaching_admin_audit，Coordinator R6 通报）：
%%% meck with_tx（直接执行 Tx）/repo 守卫与写入/execute 捕获 INSERT
%%%===================================================================

logic_mocks(ExecuteReturn) ->
    [
        {elib_pg, [
            {'with_tx', 2, fun(Tx, _Opts) -> Tx(audit_test_conn) end},
            {'execute', 3, fun(_Conn, Sql, Params) ->
                put(last_audit_sql, Sql),
                put(last_audit_params, Params),
                ExecuteReturn
            end}
        ]},
        {elib_tsid, [
            {'generate', 0, fun() -> 995555555 end}
        ]},
        {teaching_learner_bind_repo, [
            {'operator_role_tx', 3, fun(Conn, Uid, LearnerId) ->
                case {Conn, Uid, LearnerId} of
                    {audit_test_conn, denied_uid, _} -> unauthorized;
                    _ -> owner
                end
            end},
            {'bind_tx', 4, fun(_Conn, _L, _T, _O) -> {ok, #{<<"id">> => ?LEARNER_ID}} end},
            {'unbind_tx', 3, fun(_Conn, _L, _O) -> {ok, #{<<"id">> => ?LEARNER_ID}} end}
        ]}
    ].

audit_insert_on_bind_test_() ->
    ?WITH_MECKS(logic_mocks({ok, 1}), fun() ->
        {ok, _} = teaching_learner_bind_logic:bind_learner(?UID, ?LEARNER_ID, ?TARGET_UID),
        Sql = get(last_audit_sql),
        Params = get(last_audit_params),
        ?assert(binary:match(Sql, <<"teaching_admin_audit">>) =/= nomatch),
        %% 参数形态：[TSID, action(binary), operator, learner, target, detail jsonb]
        [995555555, <<"bind_learner">>, ?UID, ?LEARNER_ID, ?TARGET_UID, Detail] = Params,
        ?assertEqual(#{<<"role">> => <<"owner">>}, jsone:decode(Detail)),
        erase(last_audit_sql),
        erase(last_audit_params)
    end).

audit_insert_on_unbind_target_null_test_() ->
    ?WITH_MECKS(logic_mocks({ok, 1}), fun() ->
        {ok, _} = teaching_learner_bind_logic:unbind_learner(?UID, ?LEARNER_ID),
        [_, <<"unbind_learner">>, ?UID, ?LEARNER_ID, null, _Detail] = get(last_audit_params),
        erase(last_audit_params)
    end).

audit_not_inserted_when_denied_test_() ->
    ?WITH_MECKS(logic_mocks({ok, 1}), fun() ->
        ?assertEqual(
            {error, not_authorized},
            teaching_learner_bind_logic:bind_learner(denied_uid, ?LEARNER_ID, ?TARGET_UID)
        ),
        ?assertEqual(undefined, get(last_audit_params))
    end).

%% 审计写失败 → fail-closed（异常抛出；生产路径由 with_tx reraise:false
%% 捕获回滚，此处 mock 直传验证审计层主动 error 的语义）
audit_insert_failed_fail_closed_test_() ->
    ?WITH_MECKS(logic_mocks({error, audit_db_down}), fun() ->
        ?assertException(
            error,
            {audit_insert_failed, audit_db_down},
            teaching_learner_bind_logic:bind_learner(?UID, ?LEARNER_ID, ?TARGET_UID)
        )
    end).
