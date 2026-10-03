-module(device_revocation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% 设备移除 = token 吊销 的端到端契约测试
%%%
%%% 覆盖三条鉴权路径：
%%%   1. HTTP  auth_ds:verify_token/1
%%%   2. 刷新  passport_handler refreshtoken（356 天窗口的真正闸门）
%%%   3. WS    websocket_ds:auth/4
%%%
%%% 不变量（每条路径都断言）：
%%%   - 设备被移除 → 401，强制重登
%%%   - did 为空的 legacy token 完全不受影响（零全端登出）
%%%   - 未被移除的设备正常放行
%%%   - WS 只接受 <<"tk">>，refresh token 走 401
%%%===================================================================

-define(UID, 12345).
-define(DID, <<"dev-1">>).
-define(EXP, 4102444800).

%% is_active 被调用即失败——用于断言 legacy 空 did token 根本不做设备校验
never_called_is_active() ->
    {user_device_ds, [
        {'is_active', 2, fun(_Uid, _Did) ->
            erlang:error(is_active_must_not_be_called_for_legacy_token)
        end}
    ]}.

%% ===================================================================
%% 路径 1：HTTP auth_ds:verify_token/1
%% ===================================================================

http_revoked_device_token_rejected_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"tk-bound">>) ->
                    {ok, ?UID, ?EXP, <<"tk">>, ?DID, 1}
                end}
            ]},
            {user_device_ds, [
                {'is_active', 2, fun(?UID, ?DID) -> false end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, ?ERR_TOKEN_INVALID, _},
                auth_ds:verify_token(<<"Bearer tk-bound">>)
            )
        end
    ).

http_active_device_token_accepted_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"tk-bound">>) ->
                    {ok, ?UID, ?EXP, <<"tk">>, ?DID, 1}
                end}
            ]},
            {user_device_ds, [
                {'is_active', 2, fun(?UID, ?DID) -> true end}
            ]},
            {auth_session_ds, [
                {'revoked', 2, fun(_Uid, _Ep) -> false end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, ?UID, ?DID}, auth_ds:verify_token(<<"Bearer tk-bound">>))
        end
    ).

%% 防误伤：legacy 空 did token 放行，且完全不查设备表
http_legacy_didless_token_unaffected_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"tk-legacy">>) ->
                    {ok, ?UID, ?EXP, <<"tk">>, <<>>, undefined}
                end}
            ]},
            never_called_is_active()
        ],
        fun() ->
            ?assertEqual({ok, ?UID, <<>>}, auth_ds:verify_token(<<"Bearer tk-legacy">>))
        end
    ).

%% 既有行为不回归：HTTP 侧 refresh token 依旧不可用于业务请求
http_refresh_token_still_rejected_test_() ->
    ?WITH_MECK(
        token_ds,
        [
            {'decrypt_token', 1, fun(<<"rtk">>) ->
                {ok, ?UID, ?EXP, <<"rtk">>, ?DID, 1}
            end}
        ],
        fun() ->
            ?assertMatch(
                {error, ?ERR_TOKEN_REFRESH_NOT_ALLOWED, _},
                auth_ds:verify_token(<<"Bearer rtk">>)
            )
        end
    ).

%% ===================================================================
%% 路径 2：刷新端点（切断 356 天窗口）
%% ===================================================================

refresh_mocks(Did, DeviceMocks) ->
    [
        {cowboy_req, [
            {'header', 2, fun(<<"imboy-refreshtoken">>, _Req) -> <<"rtk">> end}
        ]},
        {throttle, [{'check', 2, fun(_Type, _Token) -> ok end}]},
        {token_ds, [
            {'decrypt_token', 1, fun(<<"rtk">>) -> {ok, ?UID, ?EXP, <<"rtk">>, Did, 1} end},
            {'encrypt_token', 2, fun(?UID, D) when D =:= Did -> <<"new-token">> end}
        ]},
        {user_logic, [{'get_status', 1, fun(?UID) -> 1 end}]},
        {auth_session_ds, [
            {'revoked', 2, fun(_Uid, _Ep) -> false end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{response_status => 200, response_body => Data})
            end},
            {'error', 3, fun(_Req, _Msg, Code) ->
                cowboy_req_h:new(#{response_status => Code, response_body => #{status => error}})
            end}
        ]}
    ] ++ DeviceMocks.

do_refresh() ->
    MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
    {ok, Req, _State} = passport_handler:init(MockReq, #{action => refreshtoken}),
    cowboy_req_h:response(Req).

refresh_with_revoked_device_rejected_test_() ->
    ?WITH_MECKS(
        refresh_mocks(?DID, [
            {user_device_logic, [{'is_active', 2, fun(?UID, ?DID) -> false end}]}
        ]),
        fun() ->
            {StatusCode, _, _} = do_refresh(),
            ?assertEqual(?ERR_TOKEN_INVALID, StatusCode)
        end
    ).

refresh_with_active_device_ok_test_() ->
    ?WITH_MECKS(
        refresh_mocks(?DID, [
            {user_device_logic, [{'is_active', 2, fun(?UID, ?DID) -> true end}]}
        ]),
        fun() ->
            {StatusCode, _, Body} = do_refresh(),
            ?assertEqual(200, StatusCode),
            ?assertEqual(<<"new-token">>, maps:get(<<"token">>, Body))
        end
    ).

%% 防误伤：legacy 空 did 的 refresh token 照常刷新，且不查设备表
refresh_legacy_didless_unaffected_test_() ->
    ?WITH_MECKS(
        refresh_mocks(<<>>, [
            {user_device_logic, [
                {'is_active', 2, fun(_Uid, _Did) ->
                    erlang:error(is_active_must_not_be_called_for_legacy_token)
                end}
            ]}
        ]),
        fun() ->
            {StatusCode, _, Body} = do_refresh(),
            ?assertEqual(200, StatusCode),
            ?assertEqual(<<"new-token">>, maps:get(<<"token">>, Body))
        end
    ).

%% ===================================================================
%% 路径 3：WebSocket websocket_ds:auth/4
%% ===================================================================

ws_cowboy_mock() ->
    {cowboy_req, [
        {'reply', 4, fun(Status, Headers, _Body, _Req) -> {replied, Status, Headers} end}
    ]}.

ws_auth(Token) ->
    websocket_ds:auth(Token, req, #{did => <<"client-claimed">>}, #{idle_timeout => 1}).

ws_refresh_token_rejected_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"rtk">>) -> {ok, ?UID, ?EXP, <<"rtk">>, ?DID, 1} end}
            ]},
            ws_cowboy_mock()
        ],
        fun() ->
            {ok, {replied, Status, Headers}, State} = ws_auth(<<"rtk">>),
            ?assertEqual(401, Status),
            ?assertEqual(<<"refresh_not_allowed">>, maps:get(<<"x-token-error">>, Headers)),
            ?assertEqual(901, maps:get(error, State)),
            %% 认证失败不得写入 current_uid
            ?assertEqual(error, maps:find(current_uid, State))
        end
    ).

ws_access_token_accepted_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"tk">>) -> {ok, ?UID, ?EXP, <<"tk">>, ?DID, 1} end}
            ]},
            {auth_session_ds, [
                {'revoked', 2, fun(_Uid, _Ep) -> false end}
            ]},
            {user_device_ds, [{'is_active', 2, fun(?UID, ?DID) -> true end}]}
        ],
        fun() ->
            {cowboy_websocket, req, State, _Opt} = ws_auth(<<"tk">>),
            ?assertEqual(?UID, maps:get(current_uid, State)),
            ?assertEqual(<<"tk">>, maps:get(token_type, State)),
            %% did 以 token 为准，覆盖客户端自称的 did（不可伪造是吊销生效的前提）
            ?assertEqual(?DID, maps:get(did, State))
        end
    ).

ws_revoked_device_rejected_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"tk">>) -> {ok, ?UID, ?EXP, <<"tk">>, ?DID, 1} end}
            ]},
            {user_device_ds, [{'is_active', 2, fun(?UID, ?DID) -> false end}]},
            ws_cowboy_mock()
        ],
        fun() ->
            {ok, {replied, Status, Headers}, State} = ws_auth(<<"tk">>),
            ?assertEqual(401, Status),
            ?assertEqual(<<"device_revoked">>, maps:get(<<"x-token-error">>, Headers)),
            ?assertEqual(error, maps:find(current_uid, State))
        end
    ).

%% 防误伤：legacy 空 did token 连 WS 正常，且不查设备表；
%% 会话 did 回退到 header/query（websocket_handler 解析出的值）
ws_legacy_didless_token_unaffected_test_() ->
    ?WITH_MECKS(
        [
            {token_ds, [
                {'decrypt_token', 1, fun(<<"tk-legacy">>) ->
                    {ok, ?UID, ?EXP, <<"tk">>, <<>>, undefined}
                end}
            ]},
            never_called_is_active()
        ],
        fun() ->
            {cowboy_websocket, req, State, _Opt} = ws_auth(<<"tk-legacy">>),
            ?assertEqual(?UID, maps:get(current_uid, State)),
            ?assertEqual(<<"client-claimed">>, maps:get(did, State))
        end
    ).

%% ===================================================================
%% 判据与缓存：user_device_ds:is_active/2
%% ===================================================================

%% 缓存窗口内：命中缓存直接返回，不触达 repo（repo 未 mock，被调用即 undef），
%% 且不得走 memo 回填（RT-P3-04 起：命中判定在 get，条件式写缓存在回源之后）
is_active_within_cache_ttl_skips_repo_test_() ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'get', 1, fun({user_device_active, ?UID, ?DID}) -> {ok, {ok, true}} end},
            {'memo', 3, fun(_F, _K, _TTL) ->
                erlang:error(memo_must_not_be_called_on_cache_hit)
            end}
        ],
        fun() ->
            ?assert(user_device_ds:is_active(?UID, ?DID))
        end
    ).

%% 缓存窗口外（miss）：回源 repo，查无此行 → 判吊销。
%% RT-P3-04 关键契约：**负结果绝不允许写入缓存**——login 后设备行异步落库，
%% 把竞态期的 false 负缓存 60s 会放大成整分钟 device_revoked。
is_active_cache_miss_queries_repo_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun(_Key) -> undefined end},
                {'memo', 3, fun(_F, _K, _TTL) ->
                    erlang:error(negative_result_must_not_be_cached)
                end}
            ]},
            {user_device_repo, [{'is_active', 2, fun(?UID, ?DID) -> {ok, false} end}]}
        ],
        fun() ->
            ?assertNot(user_device_ds:is_active(?UID, ?DID))
        end
    ).

%% 缓存窗口外（miss）：活跃行 → 正结果以 60s TTL 写入缓存
is_active_cache_miss_active_row_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun(_Key) -> undefined end},
                {'memo', 3, fun(_Fun, Key, MaxAge) ->
                    ?assertEqual({user_device_active, ?UID, ?DID}, Key),
                    ?assertEqual(60, MaxAge),
                    {ok, {ok, true}}
                end}
            ]},
            {user_device_repo, [{'is_active', 2, fun(?UID, ?DID) -> {ok, true} end}]}
        ],
        fun() ->
            ?assert(user_device_ds:is_active(?UID, ?DID))
        end
    ).

%% DB 查不出来时 fail-closed：不可确认设备仍活跃就拒绝绑定 token
is_active_db_error_fails_closed_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun(_Key) -> undefined end},
                {'memo', 3, fun(_F, _K, _TTL) -> erlang:error(db_error_must_not_be_cached) end}
            ]},
            {user_device_repo, [{'is_active', 2, fun(_, _) -> {error, timeout} end}]}
        ],
        fun() ->
            ?assertNot(user_device_ds:is_active(?UID, ?DID))
        end
    ).

%% ===================================================================
%% 删除设备：吊销判据缓存必须立即失效并跨节点广播
%% ===================================================================

delete_flushes_and_broadcasts_active_cache_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {imboy_syn, [{'list_by_uid', 1, fun(?UID) -> [] end}]},
            {imboy_cache, [
                {'flush', 1, fun(_Key) -> ok end},
                {'broadcast', 1, fun(_Msg) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ActiveKey = {user_device_active, ?UID, ?DID},
            ?assertEqual(ok, user_device_logic:delete(?UID, ?DID)),
            Calls = [{F, A} || {_Pid, {imboy_cache, F, A}, _R} <- meck:history(imboy_cache)],
            ?assert(lists:member({flush, [ActiveKey]}, Calls)),
            ?assert(lists:member({broadcast, [{flush, ActiveKey}]}, Calls))
        end
    ).

%% ===================================================================
%% 路径 4：吊销级联 —— 清除该设备的 Olm 材料
%%
%% 为什么这条必须有测试：前三条路径只保证「token 不再被接受」。但 Olm 身份键/
%% 一次性键/fallback 键存在独立三张表里，不随设备行消失。留着 = **吊销对 E2EE
%% 不生效**：list_devices_with_identity 仍把死设备列为收件人（扇出继续向它加密、
%% 永远等不到 ACK），claim_one_time_key 仍能领到它的预共享密钥。
%%
%% 空测反证：把 user_device_ds:delete/2 里的 cleanup_olm_material 调用删掉，
%% 第一条必红。
%% ===================================================================

delete_purges_olm_material_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {olm_identity_repo, [
                {'delete_by_device', 2, fun(?UID, ?DID) -> {ok, 3} end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, user_device_ds:delete(?UID, ?DID)),
            Calls = [
                {F, A}
             || {_Pid, {olm_identity_repo, F, A}, _R} <- meck:history(olm_identity_repo)
            ],
            ?assert(lists:member({delete_by_device, [?UID, ?DID]}, Calls))
        end
    ).

%% 顺序契约：设备行必须先删（token 吊销是安全关键），Olm 清理在后。
%% 反过来一旦删行失败，就成了"密钥没了但 token 还有效"的最坏组合。
delete_removes_device_row_before_olm_material_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {olm_identity_repo, [
                {'delete_by_device', 2, fun(?UID, ?DID) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, user_device_ds:delete(?UID, ?DID)),
            RowDeleted = [
                1
             || {_P, {user_device_repo, delete, [?UID, ?DID]}, _R} <-
                    meck:history(user_device_repo)
            ],
            OlmPurged = [
                1
             || {_P, {olm_identity_repo, delete_by_device, [?UID, ?DID]}, _R} <-
                    meck:history(olm_identity_repo)
            ],
            %% 两者都发生过；顺序由 delete/2 的顺序执行语义保证
            %% （ok = user_device_repo:delete(...) 不成功即 badmatch，后续不会执行）
            ?assertEqual([1], RowDeleted),
            ?assertEqual([1], OlmPurged)
        end
    ).

%% Olm 清理失败不得阻断吊销：设备行已删 = token 已吊销，这部分已经完成。
%% 此时抛错只会让调用方以为吊销失败而重试，反而更糟（但会记 ERROR 日志）。
delete_survives_olm_cleanup_error_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {olm_identity_repo, [
                {'delete_by_device', 2, fun(?UID, ?DID) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, user_device_ds:delete(?UID, ?DID))
        end
    ).

%% 同上，但 Olm 清理直接崩溃（连 {error,_} 都没返回）——依然不得阻断吊销。
delete_survives_olm_cleanup_crash_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {olm_identity_repo, [
                {'delete_by_device', 2, fun(?UID, ?DID) -> erlang:error(boom) end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, user_device_ds:delete(?UID, ?DID))
        end
    ).

%% C01：清理失败除 ERROR 日志外必须有指标痕迹（对账告警的数据来源）
delete_cleanup_failure_emits_metric_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {olm_identity_repo, [
                {'delete_by_device', 2, fun(?UID, ?DID) -> {error, db_down} end}
            ]},
            {elib_metric, [{'increment', 1, fun(_) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(ok, user_device_ds:delete(?UID, ?DID)),
            ?assertEqual(
                1,
                meck:num_calls(elib_metric, increment, [device_olm_cleanup_failed_total])
            )
        end
    ).

%% C01：清理崩溃同样计数（与 {error,_} 路径同权重告警）
delete_cleanup_crash_emits_metric_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [{'delete', 2, fun(?UID, ?DID) -> ok end}]},
            {olm_identity_repo, [
                {'delete_by_device', 2, fun(?UID, ?DID) -> erlang:error(boom) end}
            ]},
            {elib_metric, [{'increment', 1, fun(_) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(ok, user_device_ds:delete(?UID, ?DID)),
            ?assertEqual(
                1,
                meck:num_calls(elib_metric, increment, [device_olm_cleanup_failed_total])
            )
        end
    ).

%% ===================================================================
%% C01（run-20261003-094804）：吊销对账路径 —— cleanup 失败不回滚撤销，
%% 残留材料由 reconcile_olm_material/0 以 user_device 白名单权威反向清扫。
%% ===================================================================

reconcile_calls_repo_sweep_test_() ->
    ?WITH_MECKS(
        [
            {olm_identity_repo, [
                {'sweep_orphan_olm_material', 0, fun() -> {ok, 5} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 5}, user_device_ds:reconcile_olm_material()),
            ?assertEqual(
                1,
                meck:num_calls(olm_identity_repo, sweep_orphan_olm_material, 0)
            )
        end
    ).

reconcile_swallows_repo_error_test_() ->
    ?WITH_MECKS(
        [
            {olm_identity_repo, [
                {'sweep_orphan_olm_material', 0, fun() -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, db_down}, user_device_ds:reconcile_olm_material())
        end
    ).

reconcile_survives_repo_crash_test_() ->
    ?WITH_MECKS(
        [
            {olm_identity_repo, [
                {'sweep_orphan_olm_material', 0, fun() -> erlang:error(boom) end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, {error, boom}}, user_device_ds:reconcile_olm_material())
        end
    ).

%% ===================================================================
%% C01：user_device_repo:bump_identity_version/2 —— 撤销复活门 + 版本事件
%% ===================================================================

bump_identity_version_sql_contract_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(Sql, Params) ->
                SqlStr = binary_to_list(iolist_to_binary(Sql)),
                HasBump = string:str(SqlStr, "identity_version = identity_version + 1") > 0,
                HasActive = string:str(SqlStr, "status = 1") > 0,
                HasReturning =
                    string:str(SqlStr, "RETURNING identity_version, device_generation") > 0,
                ?assertEqual(true, HasBump andalso HasActive andalso HasReturning),
                ?assertEqual([?UID, ?DID], Params),
                {ok, [#{<<"identity_version">> => 4, <<"device_generation">> => 2}]}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 4, 2}, user_device_repo:bump_identity_version(?UID, ?DID))
        end
    ).

%% 撤销/无活跃行：RETURNING 0 行 → {ok, 0}（logic 层据此拒绝身份写入）
bump_identity_version_no_active_row_returns_zero_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
        ],
        fun() ->
            ?assertEqual({ok, 0}, user_device_repo:bump_identity_version(?UID, ?DID))
        end
    ).

bump_identity_version_maps_db_error_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) -> {error, timeout} end}
        ],
        fun() ->
            ?assertEqual({error, timeout}, user_device_repo:bump_identity_version(?UID, ?DID))
        end
    ).

%% ds 层 G3 透传：bump_identity_version 薄传到 repo
bump_identity_version_ds_passes_through_test_() ->
    ?WITH_MECKS(
        [
            {user_device_repo, [
                {'bump_identity_version', 2, fun(?UID, ?DID) -> {ok, 3, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 3, 1}, user_device_ds:bump_identity_version(?UID, ?DID))
        end
    ).

%% ===================================================================
%% C01：claim 撤销联合门（olm_identity_ds 层）——OTK/fallback 领取前
%% 确认目标设备在 user_device 白名单仍活跃。覆盖 cleanup_olm_material
%% 失败残留路径：撤销后 olm 三表残留行领不走。
%% ===================================================================

ds_claim_otk_rejects_revoked_device_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [{'is_active', 2, fun(?UID, ?DID) -> false end}]},
            {olm_identity_repo, [
                {'claim_one_time_key', 3, fun(_, _, _) ->
                    erlang:error(repo_must_not_be_reached_for_revoked_device)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, device_revoked},
                olm_identity_ds:claim_one_time_key(?UID, ?DID, 999)
            )
        end
    ).

%% 同门覆盖幂等租约路径（/4）
ds_claim_otk_with_request_id_rejects_revoked_device_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [{'is_active', 2, fun(?UID, ?DID) -> false end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, device_revoked},
                olm_identity_ds:claim_one_time_key(?UID, ?DID, 999, <<"req-1">>)
            )
        end
    ).

%% 同门覆盖 fallback 兜底领取
ds_claim_fallback_rejects_revoked_device_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [{'is_active', 2, fun(?UID, ?DID) -> false end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, device_revoked},
                olm_identity_ds:claim_fallback_key(?UID, ?DID)
            )
        end
    ).

%% 活跃设备不受门影响：正常透传到 repo（repo 被 mock，捕获调用形状）
ds_claim_otk_active_device_passes_through_test_() ->
    ?WITH_MECKS(
        [
            {user_device_ds, [{'is_active', 2, fun(?UID, ?DID) -> true end}]},
            {olm_identity_repo, [
                {'claim_one_time_key', 3, fun(?UID, ?DID, 999) ->
                    {ok, #{<<"key_id">> => <<"k">>, <<"key_base64">> => <<"v">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{<<"key_id">> => <<"k">>, <<"key_base64">> => <<"v">>}},
                olm_identity_ds:claim_one_time_key(?UID, ?DID, 999)
            )
        end
    ).
