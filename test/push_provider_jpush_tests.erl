-module(push_provider_jpush_tests).

%%%
%%% EPGZ-07 — JPush provider 测试套件（W2 实现后全绿）
%%%
%%% W1 骨架（12 RED + 3 GREEN）已在 W2 转绿：adapter
%%% `push_provider_jpush` / HTTP seam `push_provider_jpush_http` /
%%% `push_notification_ds:do_send_push` jpush 分派均已实现，合同见
%%% docs/reference/push-provider-jpush-research-2026-09-21.md。
%%% D 组为 W2 新增的 F3 防御校验用例（handler 400 而非 DB 500）。
%%%
%%% 合同来源：.Codex/runs/enterprise-internal-20260921T043945Z/control/
%%% plan-gz.snapshot.md §7.4（JPush）。合同条目编号：
%%%   J1 注册/刷新  J2 注销  J3 请求构造  J4 响应解析
%%%   J5 错误分类  J6 多设备 fan-out 分派与失效 token 下线
%%%   F3 push_register 参数校验（device_type 非空 + 值域）
%%%
%%% 硬性纪律：
%%%   - HTTP 层全部经 `push_provider_jpush_http`（W2 薄封装，默认 gun），
%%%     测试用 meck 桩拦截；测试 URL 一律用 RFC 2606 `.invalid` 域，
%%%     绝不触达 api.jpush.cn 等真实端点。
%%%   - 凭证一律占位符（`*-placeholder`），严禁引入真实 AppKey/MasterSecret。
%%%   - title/body 走 push_notification_logic 现有 fail-closed 常量哲学：
%%%     推送通道视为不可信第三方，payload 不含消息正文/密文/发送者身份。
%%%
%%% 运行：make eunit-local t=push_provider_jpush_tests
%%%（与 test/ds/push_notification_logic_tests.erl 同款 meck 风格）
%%%

-include_lib("eunit/include/eunit.hrl").

-define(JPUSH_HTTP, push_provider_jpush_http).
-define(JPUSH_ADAPTER, push_provider_jpush).

%% 占位凭证（合同文档同款占位符，非真实凭据）
-define(TEST_APP_KEY, <<"test-jpush-appkey-placeholder">>).
-define(TEST_MASTER_SECRET, <<"test-jpush-master-secret-placeholder">>).
%% RFC 2606 保留域，永不解析；真实默认 URL 由 W2 在 adapter 内定义
-define(TEST_PUSH_URL, <<"https://push.invalid.test/v3/push">>).

%% 现有隐私常量（push_notification_logic 的 fail-closed 不变量）
-define(PUSH_TITLE, <<"新消息"/utf8>>).
-define(PUSH_BODY, <<"发来一条消息"/utf8>>).

%% ===================================================================
%% Helpers
%% ===================================================================

%% meck 包装：兼容「目标模块尚未实现」的 RED 阶段与 W2 实现后的 GREEN 阶段。
%% RED 阶段 push_provider_jpush / push_provider_jpush_http 均不存在，
%% 必须用 non_strict 建 mock（本仓 meck 1.0.0：mock 不存在模块用
%% non_strict；不能带 unstick）；W2 实现后模块存在，退回常规 mock，
%% 严格匹配真实导出函数。
mock_new(Module) ->
    case code:which(Module) of
        non_existing ->
            meck:new(Module, [non_strict, no_link]);
        _File ->
            meck:new(Module, [no_link, unstick])
    end.

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        [mock_new(M) || M <- Modules],
        try
            Fun()
        after
            meck:unload()
        end
    end)()
).

%% 从进程邮箱取回 mock 捕获的参数（mock 内 self() ! Msg）
recv_captured(Tag) ->
    receive
        {Tag, Value} -> Value
    after 2000 ->
        erlang:error({capture_timeout, Tag})
    end.

%% 注入占位 jpush 配置；用例收尾 application:unset_env(imboy, push)
set_jpush_env() ->
    application:set_env(imboy, push, [
        {jpush_app_key, ?TEST_APP_KEY},
        {jpush_master_secret, ?TEST_MASTER_SECRET},
        {jpush_push_url, ?TEST_PUSH_URL}
    ]).

%% 解码 adapter 请求 body JSON（jsone，与生产 send_fcm/send_apns 一致）
decode_json(Bin) when is_binary(Bin) ->
    jsone:decode(Bin).

%% ===================================================================
%% A 组 · Token 生命周期合同（J1 注册/刷新、J2 注销）
%% ===================================================================

%% J1 注册合同：device_type=android、provider(platform)=jpush 的 token
%% 记录经 push_notification_logic:register_token 落库，字段形状不变。
%% 依赖：A1 migration 扩展 chk_push_token_platform CHECK 纳入 'jpush'
%%（本用例 meck 掉 elib_pg/elib_pg_sql，不触发 DB CHECK；真 DB 合同由
%% migration gate 把关）。
j1_register_jpush_token_contract_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql, elib_tsid], fun() ->
        meck:expect(elib_dt, now, fun() -> <<"2026-09-21T00:00:00Z">> end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_tsid, generate, fun(push_token) -> 4242 end),
        meck:expect(elib_pg_sql, insert, fun(_Tb, Data) ->
            self() ! {j1_insert, Data},
            {<<"INSERT INTO ...">>, [1]}
        end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 0} end),
        meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual(
            ok,
            push_notification_logic:register_token(
                1, <<"did-android-1">>, <<"android">>, <<"jpush">>, <<"rid-jpush-001">>
            )
        ),
        Data = recv_captured(j1_insert),
        ?assertEqual(1, maps:get(<<"user_id">>, Data)),
        ?assertEqual(<<"did-android-1">>, maps:get(<<"device_id">>, Data)),
        ?assertEqual(<<"android">>, maps:get(<<"device_type">>, Data)),
        ?assertEqual(<<"jpush">>, maps:get(<<"platform">>, Data)),
        ?assertEqual(<<"rid-jpush-001">>, maps:get(<<"token">>, Data)),
        ?assertEqual(1, maps:get(<<"status">>, Data))
    end).

%% J1 刷新合同：token 刷新 = 同一 upsert 路径——先 deactivate 同设备旧
%% token（UPDATE ... status=0），再插入新 token 行；同设备仅保留一条活跃。
%%
%% FULL-06 口径更新：断电语句的**参数形状**由 [Now, Uid, DeviceId] 变为
%% [Now, Token, Uid, DeviceId]（谓词加 token 维度，见 push_token_repo:upsert
%% 文档：同 token 换主人/换设备也必须断电）。本用例的断言随之同步——
%% 这是本次唯一的既有断言改动（1 处，changelog 见 checkpoints/FULL-06.md）。
j1_refresh_replaces_previous_token_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql, elib_tsid], fun() ->
        meck:expect(elib_dt, now, fun() -> <<"2026-09-21T00:00:00Z">> end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_tsid, generate, fun(push_token) -> 4243 end),
        meck:expect(elib_pg_sql, insert, fun(_Tb, Data) ->
            self() ! {j1r_insert, Data},
            {<<"INSERT INTO ...">>, [1]}
        end),
        meck:expect(elib_pg, execute, fun(_Sql, Params) ->
            self() ! {j1r_deactivate, Params},
            {ok, 1}
        end),
        meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual(
            ok,
            push_notification_logic:register_token(
                1, <<"did-android-1">>, <<"android">>, <<"jpush">>, <<"rid-jpush-002-new">>
            )
        ),
        %% 先 deactivate 旧 token：参数形状 [Now, Token, Uid, DeviceId]
        DeactParams = recv_captured(j1r_deactivate),
        ?assertMatch(
            [_Now, <<"rid-jpush-002-new">>, 1, <<"did-android-1">>], DeactParams
        ),
        %% 再插入新 token 行：token 值为新 registration_id
        InsertData = recv_captured(j1r_insert),
        ?assertEqual(<<"jpush">>, maps:get(<<"platform">>, InsertData)),
        ?assertEqual(<<"rid-jpush-002-new">>, maps:get(<<"token">>, InsertData)),
        ?assertEqual(1, maps:get(<<"status">>, InsertData))
    end).

%% J2 注销合同：登出链路 push_notification_logic:unregister_token 对
%% jpush token 同样走 push_token_repo:deactivate(uid, device_id)。
j2_unregister_deactivates_jpush_token_test() ->
    ?WITH_MECKS([push_token_repo], fun() ->
        meck:expect(push_token_repo, deactivate, fun(1, <<"did-android-1">>) ->
            {ok, 1}
        end),
        ?assertEqual(
            ok,
            push_notification_logic:unregister_token(1, <<"did-android-1">>)
        ),
        ?assert(
            meck:called(
                push_token_repo, deactivate, [1, <<"did-android-1">>]
            )
        )
    end).

%% ===================================================================
%% B 组 · Adapter 合同（J3 请求构造 / J4 响应解析 / J5 错误分类）
%% 模块 push_provider_jpush（W2 实现，src/），当前 RED。
%% ===================================================================

%% J3 provider 常量：push_token.platform 存 'jpush'；一期仅 Android。
j3_provider_constants_test() ->
    ?assertEqual(<<"jpush">>, ?JPUSH_ADAPTER:provider()),
    ?assertEqual(<<"android">>, ?JPUSH_ADAPTER:device_type()).

%% J3 fail-closed：jpush 配置缺失时返回 {error, not_configured}，
%% 与 send_fcm/send_apns 同语义；且绝不发出任何 HTTP 请求。
j3_send_not_configured_fail_closed_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        application:unset_env(imboy, push),
        meck:expect(?JPUSH_HTTP, post, 3, {error, unexpected_call}),
        ?assertEqual(
            {error, not_configured},
            ?JPUSH_ADAPTER:send(<<"rid-1">>, ?PUSH_TITLE, ?PUSH_BODY)
        ),
        ?assertEqual(0, meck:num_calls(?JPUSH_HTTP, post, '_'))
    end).

%% J3 请求构造（RED 核心）：Basic base64(AppKey:MasterSecret) 鉴权头 +
%% 最小 Android 通知 body。隐私不变量：不含消息正文/密文/extras/发送者。
j3_send_builds_minimal_android_request_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        set_jpush_env(),
        meck:expect(?JPUSH_HTTP, post, fun(Url, Headers, Body) ->
            self() ! {j3_captured, {Url, Headers, Body}},
            {ok, 200, <<"{\"sendno\":\"1\",\"msg_id\":\"1\"}">>}
        end),
        ?assertEqual(ok, ?JPUSH_ADAPTER:send(<<"rid-jpush-001">>, ?PUSH_TITLE, ?PUSH_BODY)),
        {Url, Headers, Body} = recv_captured(j3_captured),
        %% URL 必须来自配置（测试注入 .invalid 占位域）
        ?assertEqual(?TEST_PUSH_URL, Url),
        %% Basic 鉴权：base64(AppKey:MasterSecret)，占位凭证
        ExpectedBasic = <<
            "Basic ",
            (base64:encode(<<
                ?TEST_APP_KEY/binary, ":", ?TEST_MASTER_SECRET/binary
            >>))/binary
        >>,
        ?assert(lists:member({<<"authorization">>, ExpectedBasic}, Headers)),
        ?assert(lists:member({<<"content-type">>, <<"application/json">>}, Headers)),
        %% 最小 body 形状
        Json = decode_json(Body),
        ?assertEqual([<<"android">>], maps:get(<<"platform">>, Json)),
        ?assertEqual(
            [<<"rid-jpush-001">>],
            maps:get(<<"registration_id">>, maps:get(<<"audience">>, Json))
        ),
        Android = maps:get(<<"android">>, maps:get(<<"notification">>, Json)),
        ?assertEqual(?PUSH_TITLE, maps:get(<<"title">>, Android)),
        ?assertEqual(?PUSH_BODY, maps:get(<<"alert">>, Android)),
        %% fail-closed：notification.android 仅 title/alert（+可选 builder_id），
        %% 不得携带 extras / 消息正文 / 密文 / 发送者身份
        AllowedKeys = [<<"title">>, <<"alert">>, <<"builder_id">>],
        ?assert(
            lists:all(fun(K) -> lists:member(K, AllowedKeys) end, maps:keys(Android))
        ),
        %% body 顶层仅 platform/audience/notification（+可选 options），无业务字段
        TopAllowed = [<<"platform">>, <<"audience">>, <<"notification">>, <<"options">>],
        ?assert(
            lists:all(fun(K) -> lists:member(K, TopAllowed) end, maps:keys(Json))
        ),
        application:unset_env(imboy, push)
    end).

%% J4 响应解析：HTTP 200 → ok。
j4_send_ok_on_200_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        set_jpush_env(),
        meck:expect(?JPUSH_HTTP, post, fun(_Url, _Headers, _Body) ->
            {ok, 200, <<"{\"sendno\":\"1\",\"msg_id\":\"1828256752\"}">>}
        end),
        ?assertEqual(ok, ?JPUSH_ADAPTER:send(<<"rid-1">>, ?PUSH_TITLE, ?PUSH_BODY)),
        application:unset_env(imboy, push)
    end).

%% send/4（组织邀请触达）：固定常量路由数据落 notification.android.extras。
%% extras 仅允许常量键值（notify_type），title/alert 之外不得出现动态内容。
j3_send_with_data_builds_extras_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        set_jpush_env(),
        meck:expect(?JPUSH_HTTP, post, fun(_Url, _Headers, Body) ->
            self() ! {j3d_captured, Body},
            {ok, 200, <<"{\"sendno\":\"1\",\"msg_id\":\"1\"}">>}
        end),
        Data = #{<<"notify_type">> => <<"org_invite">>},
        ?assertEqual(
            ok, ?JPUSH_ADAPTER:send(<<"rid-jpush-001">>, ?PUSH_TITLE, ?PUSH_BODY, Data)
        ),
        Body = recv_captured(j3d_captured),
        Json = decode_json(Body),
        Android = maps:get(<<"android">>, maps:get(<<"notification">>, Json)),
        ?assertEqual(?PUSH_TITLE, maps:get(<<"title">>, Android)),
        ?assertEqual(?PUSH_BODY, maps:get(<<"alert">>, Android)),
        ?assertEqual(Data, maps:get(<<"extras">>, Android)),
        application:unset_env(imboy, push)
    end).

%% /3 与 /4 的边界锁：/3（Data 为空 map）payload 不含 extras 键——
%% 历史请求形状逐字节不变（消息推送隐私红线回归锚）。
j3_send_without_data_has_no_extras_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        set_jpush_env(),
        meck:expect(?JPUSH_HTTP, post, fun(_Url, _Headers, Body) ->
            self() ! {j3n_captured, Body},
            {ok, 200, <<"{\"sendno\":\"1\",\"msg_id\":\"1\"}">>}
        end),
        ?assertEqual(ok, ?JPUSH_ADAPTER:send(<<"rid-1">>, ?PUSH_TITLE, ?PUSH_BODY)),
        Json = decode_json(recv_captured(j3n_captured)),
        Android = maps:get(<<"android">>, maps:get(<<"notification">>, Json)),
        ?assertEqual(false, is_map_key(<<"extras">>, Android)),
        application:unset_env(imboy, push)
    end).

%% J5 错误分类：400 + error.code=1003（registration_id 无效）→
%% {jpush_error, invalid_token}。调用方（fan-out 层）据此调
%% push_token_repo:deactivate_by_token —— 对齐 FCM 404/410 语义。
j5_classify_invalid_token_test() ->
    ?assertEqual(
        {jpush_error, invalid_token},
        ?JPUSH_ADAPTER:classify(
            400,
            <<
                "{\"error\":{\"code\":1003,"
                "\"message\":\"cannot find user by this registration_id\"}}"
            >>
        )
    ).

%% J5 分类为 invalid_token 的 send 路径（含 body 无 message 字段的兜底形态）。
%% 错误返回值必须只含语义原子，不得回带 AppKey/MasterSecret 明文。
j5_send_invalid_token_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        set_jpush_env(),
        meck:expect(?JPUSH_HTTP, post, fun(_Url, _Headers, _Body) ->
            {ok, 400, <<"{\"error\":{\"code\":1003}}">>}
        end),
        Result = ?JPUSH_ADAPTER:send(<<"rid-1">>, ?PUSH_TITLE, ?PUSH_BODY),
        ?assertEqual({error, {jpush_error, invalid_token}}, Result),
        %% 错误值不含占位凭证（更不会含真实凭证）
        ResultBin = iolist_to_binary(io_lib:format("~p", [Result])),
        ?assertEqual(nomatch, binary:match(ResultBin, [?TEST_APP_KEY])),
        ?assertEqual(
            nomatch, binary:match(ResultBin, [?TEST_MASTER_SECRET])
        ),
        application:unset_env(imboy, push)
    end).

%% J5 错误分类：401（Basic 鉴权失败）→ unauthorized。
%% 配置类错误：不重试、不 deactivate token。
j5_classify_unauthorized_test() ->
    ?assertEqual(
        {jpush_error, unauthorized},
        ?JPUSH_ADAPTER:classify(401, <<"{\"error\":{\"code\":1004}}">>)
    ).

%% J5 错误分类：429 / 400+code=1011 → rate_limited。可重试、不 deactivate。
j5_classify_rate_limited_test() ->
    ?assertEqual(
        {jpush_error, rate_limited},
        ?JPUSH_ADAPTER:classify(429, <<"{}">>)
    ),
    ?assertEqual(
        {jpush_error, rate_limited},
        ?JPUSH_ADAPTER:classify(400, <<"{\"error\":{\"code\":1011}}">>)
    ).

%% J5 错误分类：其他非 200 → {jpush_error, {status, N}}，可重试。
j5_classify_server_error_test() ->
    ?assertEqual(
        {jpush_error, {status, 500}},
        ?JPUSH_ADAPTER:classify(500, <<"internal error">>)
    ).

%% J5 网络层错误：HTTP seam 返回 {error, Reason} 时原样透传（可重试），
%% 不吞错、不误判为 invalid_token。
j5_send_propagates_network_error_test() ->
    ?WITH_MECKS([?JPUSH_HTTP], fun() ->
        set_jpush_env(),
        meck:expect(?JPUSH_HTTP, post, fun(_Url, _Headers, _Body) ->
            {error, timeout}
        end),
        ?assertEqual(
            {error, timeout},
            ?JPUSH_ADAPTER:send(<<"rid-1">>, ?PUSH_TITLE, ?PUSH_BODY)
        ),
        application:unset_env(imboy, push)
    end).

%% ===================================================================
%% C 组 · 多设备 fan-out 分派合同（J6）
%% 当前 RED：push_notification_ds:do_send_push 只认 fcm/apns，
%% platform=jpush 落入 `_ ->` 静默跳过。W2 需加分派分支。
%% ===================================================================

%% J6 分派合同：send_to_user 对 platform=jpush 的行调用
%% push_provider_jpush:send(Token, Title, Body)（复用 async_retry 重试壳）。
j6_fanout_dispatches_jpush_to_adapter_test() ->
    ?WITH_MECKS([push_token_repo, elib_async, ?JPUSH_ADAPTER], fun() ->
        Rows = [
            #{
                <<"device_id">> => <<"did-1">>,
                <<"device_type">> => <<"android">>,
                <<"platform">> => <<"jpush">>,
                <<"token">> => <<"rid-jpush-001">>
            }
        ],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, Rows} end),
        %% async_retry 直接同步执行任务体，便于断言分派发生
        meck:expect(elib_async, async_retry, fun(Fun, _Retry, _Delay) ->
            Fun(),
            self()
        end),
        meck:expect(?JPUSH_ADAPTER, send, fun(<<"rid-jpush-001">>, ?PUSH_TITLE, ?PUSH_BODY, _Data) ->
            ok
        end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, ?PUSH_TITLE, ?PUSH_BODY)),
        ?assert(
            meck:called(
                ?JPUSH_ADAPTER, send, [<<"rid-jpush-001">>, ?PUSH_TITLE, ?PUSH_BODY, '_']
            )
        )
    end).

%% J6 失效 token 下线合同：adapter 返回 {jpush_error, invalid_token} 时，
%% fan-out 层调 push_token_repo:deactivate_by_token(Token)（对齐 FCM 404/410
%% 的 maybe_deactivate_token 语义）。
j6_fanout_invalid_token_deactivates_test() ->
    ?WITH_MECKS([push_token_repo, elib_async, ?JPUSH_ADAPTER], fun() ->
        Rows = [
            #{
                <<"device_id">> => <<"did-1">>,
                <<"device_type">> => <<"android">>,
                <<"platform">> => <<"jpush">>,
                <<"token">> => <<"rid-stale-001">>
            }
        ],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, Rows} end),
        meck:expect(elib_async, async_retry, fun(Fun, _Retry, _Delay) ->
            Fun(),
            self()
        end),
        meck:expect(?JPUSH_ADAPTER, send, fun(<<"rid-stale-001">>, _T, _B, _Data) ->
            {error, {jpush_error, invalid_token}}
        end),
        meck:expect(push_token_repo, deactivate_by_token, fun(<<"rid-stale-001">>) ->
            {ok, 1}
        end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, ?PUSH_TITLE, ?PUSH_BODY)),
        ?assert(
            meck:called(
                push_token_repo, deactivate_by_token, [<<"rid-stale-001">>]
            )
        )
    end).

%% J6 非失效错误不下线：unauthorized（凭证配置错误）绝不 deactivate token。
j6_fanout_unauthorized_keeps_token_test() ->
    ?WITH_MECKS([push_token_repo, elib_async, ?JPUSH_ADAPTER], fun() ->
        Rows = [
            #{
                <<"device_id">> => <<"did-1">>,
                <<"device_type">> => <<"android">>,
                <<"platform">> => <<"jpush">>,
                <<"token">> => <<"rid-keep-001">>
            }
        ],
        meck:expect(push_token_repo, list_by_uid, fun(1) -> {ok, Rows} end),
        meck:expect(elib_async, async_retry, fun(Fun, _Retry, _Delay) ->
            Fun(),
            self()
        end),
        meck:expect(?JPUSH_ADAPTER, send, fun(<<"rid-keep-001">>, _T, _B, _Data) ->
            {error, {jpush_error, unauthorized}}
        end),
        meck:expect(push_token_repo, deactivate_by_token, fun(_) ->
            {ok, should_not_be_called}
        end),
        ?assertEqual(ok, push_notification_ds:send_to_user(1, ?PUSH_TITLE, ?PUSH_BODY)),
        %% 前置：分派必须已发生（否则下面的"未下线"是消极断言恒真）
        ?assert(
            meck:called(?JPUSH_ADAPTER, send, ['_', ?PUSH_TITLE, ?PUSH_BODY, '_'])
        ),
        ?assertEqual(0, meck:num_calls(push_token_repo, deactivate_by_token, '_'))
    end).

%% ===================================================================
%% D 组 · push_register 参数校验合同（EPGZ-07 F3：错值 400 而非 DB 炸 500）
%% user_device_handler:push_register 校验 device_type 非空 + 双字段值域
%%（值域 = push_token 表 CHECK：device_type {android,ios,web}、
%% platform {fcm,apns,web_push,jpush}）。
%% ===================================================================

%% 公共桩：捕获 register_token 调用参数与响应分支
expect_push_register_stubs(PostVals) ->
    meck:expect(auth_ds, current_uid, fun(_State) -> 1 end),
    meck:expect(elib_param, post, fun(_Req) -> PostVals end),
    meck:expect(push_notification_logic, register_token, fun(Uid, Did, DType, Pf, Tk) ->
        self() ! {f3_register, {Uid, Did, DType, Pf, Tk}},
        ok
    end),
    meck:expect(elib_response, success, fun(Req) ->
        self() ! {f3_resp, success},
        Req
    end),
    meck:expect(elib_response, error, fun(Req, _Msg, Code) ->
        self() ! {f3_resp, {error, Code}},
        Req
    end).

%% F3：缺 device_type（App 旧版 body）→ 400，不触达 logic 层
f3_missing_device_type_rejected_400_test() ->
    ?WITH_MECKS([auth_ds, elib_param, push_notification_logic, elib_response], fun() ->
        expect_push_register_stubs(#{
            <<"device_id">> => <<"did-1">>,
            <<"platform">> => <<"jpush">>,
            <<"token">> => <<"rid-1">>
        }),
        _ = user_device_handler:handle_action(push_register, req, #{current_uid => 1}),
        ?assertEqual({error, 400}, recv_captured(f3_resp)),
        ?assertEqual(0, meck:num_calls(push_notification_logic, register_token, '_'))
    end).

%% F3：platform 误传 device_type 值（错位 1 的服务端防御）→ 400
f3_platform_device_type_value_rejected_400_test() ->
    ?WITH_MECKS([auth_ds, elib_param, push_notification_logic, elib_response], fun() ->
        expect_push_register_stubs(#{
            <<"device_id">> => <<"did-1">>,
            <<"device_type">> => <<"android">>,
            <<"platform">> => <<"android">>,
            <<"token">> => <<"rid-1">>
        }),
        _ = user_device_handler:handle_action(push_register, req, #{current_uid => 1}),
        ?assertEqual({error, 400}, recv_captured(f3_resp)),
        ?assertEqual(0, meck:num_calls(push_notification_logic, register_token, '_'))
    end).

%% F3：device_type 越界值（如 desktop/macOS 客户端值）→ 400
f3_invalid_device_type_rejected_400_test() ->
    ?WITH_MECKS([auth_ds, elib_param, push_notification_logic, elib_response], fun() ->
        expect_push_register_stubs(#{
            <<"device_id">> => <<"did-1">>,
            <<"device_type">> => <<"macos">>,
            <<"platform">> => <<"jpush">>,
            <<"token">> => <<"rid-1">>
        }),
        _ = user_device_handler:handle_action(push_register, req, #{current_uid => 1}),
        ?assertEqual({error, 400}, recv_captured(f3_resp)),
        ?assertEqual(0, meck:num_calls(push_notification_logic, register_token, '_'))
    end).

%% F3：合法 jpush 组合透传 logic 层（四字段原样、200 响应）
f3_valid_jpush_params_pass_through_test() ->
    ?WITH_MECKS([auth_ds, elib_param, push_notification_logic, elib_response], fun() ->
        expect_push_register_stubs(#{
            <<"device_id">> => <<"did-1">>,
            <<"device_type">> => <<"android">>,
            <<"platform">> => <<"jpush">>,
            <<"token">> => <<"rid-1">>
        }),
        _ = user_device_handler:handle_action(push_register, req, #{current_uid => 1}),
        ?assertEqual(success, recv_captured(f3_resp)),
        ?assertEqual(
            {1, <<"did-1">>, <<"android">>, <<"jpush">>, <<"rid-1">>},
            recv_captured(f3_register)
        )
    end).
