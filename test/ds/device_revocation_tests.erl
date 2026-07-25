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
                    {ok, ?UID, ?EXP, <<"tk">>, ?DID}
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
                    {ok, ?UID, ?EXP, <<"tk">>, ?DID}
                end}
            ]},
            {user_device_ds, [
                {'is_active', 2, fun(?UID, ?DID) -> true end}
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
                    {ok, ?UID, ?EXP, <<"tk">>, <<>>}
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
                {ok, ?UID, ?EXP, <<"rtk">>, ?DID}
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
            {'decrypt_token', 1, fun(<<"rtk">>) -> {ok, ?UID, ?EXP, <<"rtk">>, Did} end},
            {'encrypt_token', 2, fun(?UID, D) when D =:= Did -> <<"new-token">> end}
        ]},
        {user_logic, [{'get_status', 1, fun(?UID) -> 1 end}]},
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
                {'decrypt_token', 1, fun(<<"rtk">>) -> {ok, ?UID, ?EXP, <<"rtk">>, ?DID} end}
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
                {'decrypt_token', 1, fun(<<"tk">>) -> {ok, ?UID, ?EXP, <<"tk">>, ?DID} end}
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
                {'decrypt_token', 1, fun(<<"tk">>) -> {ok, ?UID, ?EXP, <<"tk">>, ?DID} end}
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
                {'decrypt_token', 1, fun(<<"tk-legacy">>) -> {ok, ?UID, ?EXP, <<"tk">>, <<>>} end}
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

%% 缓存窗口内：命中缓存直接返回，不触达 repo（repo 未 mock，被调用即 undef）
is_active_within_cache_ttl_skips_repo_test_() ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'memo', 3, fun(_Fun, Key, MaxAge) ->
                ?assertEqual({user_device_active, ?UID, ?DID}, Key),
                ?assertEqual(60, MaxAge),
                {ok, true}
            end}
        ],
        fun() ->
            ?assert(user_device_ds:is_active(?UID, ?DID))
        end
    ).

%% 缓存窗口外（miss）：回源 repo，查无此行 → 判吊销
is_active_cache_miss_queries_repo_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [{'memo', 3, fun(Fun, _Key, 60) -> Fun() end}]},
            {user_device_repo, [{'is_active', 2, fun(?UID, ?DID) -> {ok, false} end}]}
        ],
        fun() ->
            ?assertNot(user_device_ds:is_active(?UID, ?DID))
        end
    ).

is_active_cache_miss_active_row_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [{'memo', 3, fun(Fun, _Key, 60) -> Fun() end}]},
            {user_device_repo, [{'is_active', 2, fun(?UID, ?DID) -> {ok, true} end}]}
        ],
        fun() ->
            ?assert(user_device_ds:is_active(?UID, ?DID))
        end
    ).

%% DB 查不出来 ≠ 查无此行：fail-open，避免一次抖动把所有人踢下线
is_active_db_error_fails_open_test_() ->
    ?WITH_MECKS(
        [
            {imboy_cache, [{'memo', 3, fun(Fun, _Key, 60) -> Fun() end}]},
            {user_device_repo, [{'is_active', 2, fun(_, _) -> {error, timeout} end}]}
        ],
        fun() ->
            ?assert(user_device_ds:is_active(?UID, ?DID))
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
