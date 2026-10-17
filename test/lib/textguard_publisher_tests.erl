-module(textguard_publisher_tests).

%%%
%%% T16 — TextGuard 指定发布者定向测试（AC-073 + 故障族）。
%%%
%%% 运行前置：同 textguard_export_tests（TG16_PG_* 环境变量注入的隔离
%%% 一次性 PG；缺失显式 FAIL，禁止静默 skip/pass）。
%%%
%%% 发布传输注入：本模块自身实现 mock 传输（get_active_ruleset/1 +
%%% publish_package/2，状态放命名 ETS `tg_pub_mock`）；另有一组用例用
%%% 真实 `moderation_detector_http` 适配器 + ewh_http_fixture 真 socket
%%% 走完整发布栈（PUT 精确字节 / If-Match / Bearer publish_token / 409
%%% 映射 / GET not_found / 缺 publish_token fail-closed）。
%%%
%%% 覆盖：
%%%   * 初始发布与 status 区分（published/pending/last_published）；
%%%   * AC-073 丢通知：DB 变更不发通知 → 无发布；manual reconcile() 补上；
%%%   * AC-073 mark_dirty 触发即时发布（不等满周期）；
%%%   * AC-073 发布者崩溃重启：状态从零恢复，收敛到最新包；
%%%   * CAS：409 后从全新导出重试一次（If-Match 换新）；耗尽 → failed
%%%     可见且有界（恰好 1+1 次 PUT）；
%%%   * 网络错误 → state_unknown（不盲回滚），下一 tick GET 对账恢复；
%%%   * AC-075 发布门：携带悬空引用的包绝不发出（mock 收包即校验）。
%%%
%%% 运行：TG16_PG_* 注入后 `make eunit t=textguard_publisher_tests`。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(PUB, textguard_publisher).
-define(MOCK_TAB, tg_pub_mock).
-define(T2S, <<"test-t2s-v1">>).
-define(SHORT_TICK_MS, 200).
-define(LONG_TICK_MS, 60000).

%% mock 传输（发布者经配置注入调用）
-export([get_active_ruleset/1, publish_package/2]).

%% ===================================================================
%% mock 传输（ets 驱动；发布者进程内调用）
%% ===================================================================

get_active_ruleset(_Ctx) ->
    case mock_get(get_error, undefined) of
        undefined ->
            case mock_get(active, undefined) of
                undefined ->
                    {error, not_found};
                #{revision := Rev, generation := Gen, digest := Dig} ->
                    {ok, #{revision => Rev, generation => Gen, digest => Dig}}
            end;
        Reason ->
            {error, Reason}
    end.

publish_package(Body, Opts) ->
    %% 全部 PUT 尝试先记账（含将被 409 拒绝的），供 CAS 断言
    mock_append(put_attempts, #{
        body => Body,
        if_match => maps:get(if_match, Opts, none),
        at => os:system_time(millisecond)
    }),
    case no_dangling(Body) of
        ok ->
            do_publish(Body);
        dangling ->
            %% AC-075 发布门：Rust 侧对悬空引用整包拒绝——mock 同款语义，
            %% 绝不让悬空包通过（违规另有 ets 标记可断言）
            mock_set(dangling_seen, true),
            {error, protocol_error}
    end.

do_publish(Body) ->
    Left = mock_get(put_conflicts_left, 0),
    Err = mock_get(put_error, undefined),
    if
        Err =:= cas_conflict_forever ->
            advance_revision(),
            {error, cas_conflict};
        Left > 0 ->
            mock_set(put_conflicts_left, Left - 1),
            advance_revision(),
            {error, cas_conflict};
        Err =/= undefined ->
            {error, Err};
        true ->
            accept_package(Body)
    end.

accept_package(Body) ->
    Gen = mock_get(generation, 0) + 1,
    Rev = <<"rev-", (integer_to_binary(Gen))/binary>>,
    mock_set(generation, Gen),
    mock_set(active, #{revision => Rev, generation => Gen, digest => sha_hex(Body)}),
    {ok, #{revision => Rev, generation => Gen}}.

%% CAS 冲突语义：远端被「另一写者」覆盖 —— revision/generation 前进，
%%% 活跃包 digest 变成别人的（迫使发布者重导出重比对）
advance_revision() ->
    Gen = mock_get(generation, 0) + 1,
    mock_set(generation, Gen),
    mock_set(active, #{
        revision => <<"rev-", (integer_to_binary(Gen))/binary>>,
        generation => Gen,
        digest => <<"remote-advanced-digest">>
    }).

%% AC-075 发布门：悬空引用包整包拒绝（Rust 侧行为的 mock 形态）
-spec no_dangling(binary()) -> ok | dangling.
no_dangling(Body) ->
    try jsone:decode(Body) of
        #{<<"literals">> := Literals, <<"exceptions">> := Exceptions} ->
            Ids = sets:from_list([maps:get(<<"id">>, L) || L <- Literals]),
            case [X || X <- Exceptions, not sets:is_element(maps:get(<<"rule_id">>, X), Ids)] of
                [] -> ok;
                _ -> dangling
            end;
        _ ->
            ok
    catch
        _:_ -> ok
    end.

%% ===================================================================
%% mock ets 基础
%% ===================================================================

mock_set(Key, Value) ->
    ets:insert(?MOCK_TAB, {Key, Value}),
    ok.

mock_get(Key, Default) ->
    case ets:lookup(?MOCK_TAB, Key) of
        [{_, V}] -> V;
        [] -> Default
    end.

mock_append(Key, Value) ->
    ets:insert(?MOCK_TAB, {Key, [Value | mock_get(Key, [])]}),
    ok.

mock_attempts() ->
    lists:reverse(mock_get(put_attempts, [])).

-spec sha_hex(binary()) -> binary().
sha_hex(Bin) ->
    Digest = crypto:hash(sha256, Bin),
    <<<<(hex_digit(N bsr 4)), (hex_digit(N band 15))>> || <<N:8>> <= Digest>>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

%% ===================================================================
%% 套件组织
%% ===================================================================

textguard_publisher_test_() ->
    %% 外层：PG 池只建一次；内层：每个用例独立快照（词表重置 + mock ets
    %% 重建 + 发布者让位），杜绝用例间状态串扰（mock active/digest 与
    %% 词表残留会让 CAS/收敛断言互相污染）
    {setup, fun pg_only_setup/0, fun pg_only_cleanup/1, fun(_) ->
        [
            with_case(
                "initial publish converges; status distinguishes published/pending",
                fun t_initial_publish/0
            ),
            with_case(
                "AC-073: lost notification stays silent; manual reconcile() publishes",
                fun t_lost_notification/0
            ),
            with_case(
                "AC-073: mark_dirty triggers publish without waiting for tick",
                fun t_dirty_triggers/0
            ),
            with_case(
                "AC-073: crash + restart recovers and converges within ticks",
                fun t_crash_restart/0
            ),
            with_case(
                "CAS: 409 retries once from fresh export with new If-Match",
                fun t_cas_retry_once/0
            ),
            with_case(
                "CAS: exhausted retries leave failed state visible (bounded puts)",
                fun t_cas_exhausted/0
            ),
            with_case(
                "network error -> state_unknown, reconcile recovers (no rollback)",
                fun t_state_unknown_recovery/0
            ),
            with_case(
                "http adapter: publish endpoints over real socket",
                fun t_http_adapter_publish/0
            )
        ]
    end}.

-spec with_case(binary(), fun()) -> term().
with_case(Desc, TestFun) ->
    {setup, fun case_setup/0, fun case_cleanup/1, fun(_) -> {Desc, TestFun} end}.

-spec pg_only_setup() -> ok.
pg_only_setup() ->
    ok = textguard_export_tests:pg_setup(),
    ok.

-spec pg_only_cleanup(ok) -> ok.
pg_only_cleanup(ok) ->
    ok = textguard_export_tests:pg_teardown(ok),
    ok.

-spec case_setup() -> ok.
case_setup() ->
    textguard_export_tests:trunc(),
    textguard_export_tests:seed_fixed(),
    ok = application:set_env(imboy, textguard_export, [{t2s_table_id, ?T2S}]),
    ?MOCK_TAB = ets:new(?MOCK_TAB, [named_table, public, set]),
    ok.

-spec case_cleanup(ok) -> ok.
case_cleanup(ok) ->
    ?PUB:stop(),
    try
        ets:delete(?MOCK_TAB)
    catch
        _:_ -> ok
    end,
    application:unset_env(imboy, textguard_export),
    application:unset_env(imboy, moderation_detector),
    ok.

%% 起一个短周期发布者（注入本模块为传输）。
%% 单例名字逐用例复用：先停掉上一用例遗留实例（eunit 组级 setup 只在
%% 整组结束后 cleanup，测试间需自行让位）。
-spec start_short() -> {ok, pid()}.
start_short() ->
    ?PUB:stop(),
    ?PUB:start_link(#{
        transport => ?MODULE,
        reconcile_interval_ms => ?SHORT_TICK_MS,
        cas_retry_backoff_ms => 20,
        publish_timeout_ms => 2000
    }).

-spec start_long() -> {ok, pid()}.
start_long() ->
    ?PUB:stop(),
    ?PUB:start_link(#{
        transport => ?MODULE,
        reconcile_interval_ms => ?LONG_TICK_MS,
        cas_retry_backoff_ms => 20,
        publish_timeout_ms => 2000
    }).

%% 轮询直到条件成立（超时 FAIL）
-spec eventually(fun(() -> boolean()), non_neg_integer(), atom()) -> ok.
eventually(Check, TimeoutMs, Label) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    eventually_loop(Check, Deadline, Label).

eventually_loop(Check, Deadline, Label) ->
    case Check() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    erlang:error({eventually_timeout, Label});
                false ->
                    timer:sleep(25),
                    eventually_loop(Check, Deadline, Label)
            end
    end.

-spec fresh_export_digest() -> binary().
fresh_export_digest() ->
    {ok, E} = textguard_export:export_package(),
    maps:get(package_sha256, E).

-spec mock_active_digest() -> binary() | undefined.
mock_active_digest() ->
    case mock_get(active, undefined) of
        undefined -> undefined;
        #{digest := D} -> D
    end.

-spec wait_published() -> ok.
wait_published() ->
    eventually(
        fun() ->
            case ?PUB:status() of
                {ok, #{state := published}} -> true;
                _ -> false
            end
        end,
        3000,
        published
    ).

%% ===================================================================
%% 用例
%% ===================================================================

t_initial_publish() ->
    {ok, _} = start_short(),
    wait_published(),
    {ok, Status} = ?PUB:status(),
    ?assertEqual(false, maps:get(pending, Status)),
    ?assertMatch(
        #{revision := <<"rev-1">>, generation := 1, digest := _}, maps:get(last_published, Status)
    ),
    ?assertEqual(fresh_export_digest(), maps:get(digest, maps:get(last_published, Status))),
    ?assertEqual(fresh_export_digest(), mock_active_digest()),
    %% 收敛后周期 tick 不再制造新 PUT（GET digest 比对免重发）
    AttemptsBefore = length(mock_attempts()),
    timer:sleep(?SHORT_TICK_MS * 3),
    ?assertEqual(AttemptsBefore, length(mock_attempts())),
    ?assertEqual(false, mock_get(dangling_seen, false)).

t_lost_notification() ->
    {ok, _} = start_long(),
    wait_published(),
    AttemptsBefore = length(mock_attempts()),
    %% 变更词表但**不发脏通知**（通知丢失）：tick 极长 → 无发布发生
    textguard_export_tests:insert_word(99000901, <<"lost-notify-word">>, <<"pub">>, <<"low">>),
    timer:sleep(300),
    ?assertEqual(AttemptsBefore, length(mock_attempts())),
    {ok, Stale} = ?PUB:status(),
    ?assertEqual(false, maps:get(pending, Stale)),
    %% manual reconcile() 补上发布（AC-073 对账机制）
    {ok, #{state := published}} = ?PUB:reconcile(),
    ?assertEqual(AttemptsBefore + 1, length(mock_attempts())),
    ?assertEqual(fresh_export_digest(), mock_active_digest()).

t_dirty_triggers() ->
    {ok, _} = start_long(),
    wait_published(),
    AttemptsBefore = length(mock_attempts()),
    textguard_export_tests:insert_word(99000902, <<"dirty-word">>, <<"pub">>, <<"low">>),
    ok = ?PUB:mark_dirty(),
    eventually(fun() -> length(mock_attempts()) > AttemptsBefore end, 2000, dirty_publish),
    {ok, Status} = ?PUB:status(),
    ?assertMatch(#{state := published, pending := false}, Status),
    ?assertEqual(fresh_export_digest(), mock_active_digest()).

t_crash_restart() ->
    {ok, _} = start_short(),
    wait_published(),
    %% 模拟崩溃：先解链再硬杀（无 terminate 清理、无遗留状态）
    Pid = whereis(?PUB),
    unlink(Pid),
    exit(Pid, kill),
    eventually(fun() -> whereis(?PUB) =:= undefined end, 2000, killed),
    timer:sleep(?SHORT_TICK_MS * 2),
    textguard_export_tests:insert_word(99000903, <<"post-crash-word">>, <<"pub">>, <<"low">>),
    %% 重启：无持久状态，靠启动即对账 + 周期 tick 收敛到最新包
    {ok, _} = start_short(),
    wait_published(),
    eventually(
        fun() ->
            mock_active_digest() =:= fresh_export_digest()
        end,
        3000,
        crash_converge
    ),
    ?assertEqual(false, mock_get(dangling_seen, false)).

t_cas_retry_once() ->
    mock_set(put_conflicts_left, 1),
    {ok, _} = start_short(),
    wait_published(),
    Attempts = mock_attempts(),
    ?assertEqual(2, length(Attempts)),
    %% 初始无活跃包：第一次 PUT 无 If-Match，被 409（远端前进到 rev-1）；
    %% 重试的全新导出 GET 到 rev-1 → If-Match=rev-1 成功
    [First, Second] = Attempts,
    ?assertEqual(none, maps:get(if_match, First)),
    ?assertEqual(<<"rev-1">>, maps:get(if_match, Second)),
    ?assertEqual(maps:get(body, First), maps:get(body, Second)),
    {ok, Status} = ?PUB:status(),
    ?assertMatch(#{state := published}, Status),
    ?assertEqual(fresh_export_digest(), mock_active_digest()).

t_cas_exhausted() ->
    mock_set(put_error, cas_conflict_forever),
    ?PUB:stop(),
    {ok, _} = ?PUB:start_link(#{
        transport => ?MODULE,
        reconcile_interval_ms => ?LONG_TICK_MS,
        cas_max_retries => 1,
        cas_retry_backoff_ms => 20,
        publish_timeout_ms => 2000
    }),
    eventually(
        fun() ->
            case ?PUB:status() of
                {ok, #{state := failed}} -> true;
                _ -> false
            end
        end,
        3000,
        cas_failed
    ),
    {ok, Status} = ?PUB:status(),
    ?assertMatch({cas_conflict, exhausted}, maps:get(last_error, Status)),
    %% 有界重试：恰好 1 + 1 次 PUT 尝试，无无限循环；周期极长无干扰
    timer:sleep(150),
    ?assertEqual(2, length(mock_attempts())),
    %% failed 可恢复：解除注入后手动对账收敛
    mock_set(put_error, undefined),
    {ok, _} = ?PUB:reconcile(),
    {ok, #{state := published}} = ?PUB:status(),
    ?assertEqual(fresh_export_digest(), mock_active_digest()).

t_state_unknown_recovery() ->
    {ok, _} = start_short(),
    wait_published(),
    %% 注入 GET 网络故障 + 脏通知：状态未知且变更仍挂起（saved/pending
    %% 与 published 的区分），绝不盲目回滚
    mock_set(get_error, connection_failed),
    textguard_export_tests:insert_word(99000904, <<"unknown-word">>, <<"pub">>, <<"low">>),
    ok = ?PUB:mark_dirty(),
    eventually(
        fun() ->
            case ?PUB:status() of
                {ok, #{state := state_unknown}} -> true;
                _ -> false
            end
        end,
        2000,
        unknown_visible
    ),
    {ok, Status} = ?PUB:status(),
    ?assertMatch(#{state := state_unknown, pending := true}, Status),
    ?assertMatch({get_active, connection_failed}, maps:get(last_error, Status)),
    %% 故障解除：周期 tick GET 对账恢复并发布最新包
    mock_set(get_error, undefined),
    eventually(
        fun() ->
            case ?PUB:status() of
                {ok, #{state := published}} -> true;
                _ -> false
            end
        end,
        3000,
        unknown_recovered
    ),
    ?assertEqual(fresh_export_digest(), mock_active_digest()).

%% -------------------------------------------------------------------
%% 真实 HTTP 适配器发布面（ewh_http_fixture 真 socket）
%% -------------------------------------------------------------------

t_http_adapter_publish() ->
    {ok, _} = application:ensure_all_started(gun),
    Self = self(),
    %% 远端由本测试进程驱动：GET/PUT 到达后经消息握手决定响应。
    Remote = fun
        (#{method := <<"GET">>, path := <<"/internal/v1/rulesets/active">>}) ->
            Self ! {http_get, self()},
            receive
                {remote, not_found} ->
                    {reply, 404, [], <<>>};
                {remote, #{revision := R, generation := G, digest := D}} ->
                    {reply, 200, [],
                        jsone:encode(#{
                            <<"revision">> => R, <<"generation">> => G, <<"digest">> => D
                        })}
            after 3000 -> {reply, 500, [], <<>>}
            end;
        (#{method := <<"PUT">>, path := <<"/internal/v1/rulesets/active">>} = Req) ->
            Self ! {http_put, self(), Req},
            receive
                {put_reply, ok} ->
                    {reply, 200, [], <<"{\"revision\":\"srv-1\",\"generation\":7}">>};
                {put_reply, conflict} ->
                    {reply, 409, [],
                        <<"{\"error\":{\"code\":\"CAS_CONFLICT\",\"message\":\"stale\"}}">>}
            after 3000 -> {reply, 500, [], <<>>}
            end;
        (_) ->
            {reply, 404, [], <<>>}
    end,
    H = ewh_http_fixture:start(Remote),
    try
        Port = integer_to_binary(ewh_http_fixture:port(H)),
        BaseUrl = <<"http://127.0.0.1:", Port/binary>>,
        ok = application:set_env(imboy, moderation_detector, [
            {http_base_url, BaseUrl},
            {publish_token, <<"publish-token-placeholder">>}
        ]),
        clean_gun_cache(),
        %% GET：无活跃包 → not_found
        ?assertEqual(
            {error, not_found},
            run_adapter(
                fun() ->
                    moderation_detector_http:get_active_ruleset(#{deadline_ms => 2000})
                end,
                fun remote_not_found/0
            )
        ),
        %% GET：200 严格解析 revision/generation/digest
        ?assertEqual(
            {ok, #{revision => <<"srv-0">>, generation => 6, digest => <<"d0">>}},
            run_adapter(
                fun() ->
                    moderation_detector_http:get_active_ruleset(#{deadline_ms => 2000})
                end,
                fun remote_active/0
            )
        ),
        %% PUT：精确字节 + If-Match + Bearer publish_token；200 严格解析。
        %% 请求线细节取自 fixture 的请求账本（确定性，避免消息时序竞争）
        {ok, E} = textguard_export:export_package(),
        Pkg = maps:get(package, E),
        PutResult = run_adapter(
            fun() ->
                moderation_detector_http:publish_package(Pkg, #{
                    if_match => <<"rev-old">>, ctx => #{deadline_ms => 2000}
                })
            end,
            fun remote_put_ok/0
        ),
        ?assertEqual({ok, #{revision => <<"srv-1">>, generation => 7}}, PutResult),
        ReqPut = lists:last([
            R
         || R <- ewh_http_fixture:requests(H),
            maps:get(method, R) =:= <<"PUT">>,
            maps:get(path, R) =:= <<"/internal/v1/rulesets/active">>
        ]),
        ?assertEqual(Pkg, maps:get(body, ReqPut)),
        %% 请求头断言走 raw：ewh_http_fixture 的 headers 解析有既有缺陷
        %% （split_once 返回 list 而生成器模式是 {K,V} 元组，恒空），不在
        %% 本任务修共享夹具；raw 线格式断言等价且更直接
        Raw = maps:get(raw, ReqPut),
        case binary:match(Raw, <<"if-match: rev-old\r\n">>) of
            nomatch -> erlang:error({if_match_header_missing, Raw});
            _Found -> ok
        end,
        case binary:match(Raw, <<"authorization: Bearer publish-token-placeholder\r\n">>) of
            nomatch -> erlang:error({authorization_header_missing, Raw});
            _Found2 -> ok
        end,
        case binary:match(Raw, <<"content-type: application/json\r\n">>) of
            nomatch -> erlang:error({content_type_header_missing, Raw});
            _Found3 -> ok
        end,
        %% PUT 409（信封 CAS_CONFLICT）→ cas_conflict
        ?assertEqual(
            {error, cas_conflict},
            run_adapter(
                fun() ->
                    moderation_detector_http:publish_package(Pkg, #{ctx => #{deadline_ms => 2000}})
                end,
                fun remote_conflict/0
            )
        ),
        %% 缺 publish_token → fail-closed not_ready（不出网）
        ok = application:set_env(imboy, moderation_detector, [{http_base_url, BaseUrl}]),
        clean_gun_cache(),
        ?assertEqual(
            {error, not_ready},
            moderation_detector_http:publish_package(Pkg, #{ctx => #{deadline_ms => 500}})
        )
    after
        ewh_http_fixture:stop(H),
        clean_gun_cache(),
        application:unset_env(imboy, moderation_detector)
    end,
    ok.

%% 在独立进程中跑适配器调用（同步阻塞），先喂远端响应再收结果。
-spec run_adapter(fun(), fun(() -> ok)) -> term().
run_adapter(Call, Feed) ->
    Self = self(),
    {Pid, Mon} = spawn_monitor(fun() -> Self ! {adapter_result, Call()} end),
    ok = Feed(),
    Result =
        receive
            {adapter_result, R} -> R
        after 5000 -> timeout
        end,
    receive
        {'DOWN', Mon, process, Pid, normal} -> ok
    after 5000 -> ok
    end,
    Result.

%% 喂远端的钩子（与 fixture handler 进程握手决定响应）
remote_not_found() ->
    receive
        {http_get, Fx} -> Fx ! {remote, not_found}
    after 2000 -> error(no_get)
    end,
    ok.

remote_active() ->
    receive
        {http_get, Fx} ->
            Fx ! {remote, #{revision => <<"srv-0">>, generation => 6, digest => <<"d0">>}}
    after 2000 -> error(no_get_active)
    end,
    ok.

remote_put_ok() ->
    receive
        {http_put, Fx, _Req} -> Fx ! {put_reply, ok}
    after 2000 -> error(no_put)
    end,
    ok.

remote_conflict() ->
    receive
        {http_put, Fx, _Req} -> Fx ! {put_reply, conflict}
    after 2000 -> error(no_put_conflict)
    end,
    ok.

%% 进程字典 gun 连接缓存清理（T15 测试同款惯例）
-spec clean_gun_cache() -> ok.
clean_gun_cache() ->
    [
        erase(K)
     || {K, _} <- get(),
        is_tuple(K) andalso element(1, K) =:= gun_conn
    ],
    ok.
