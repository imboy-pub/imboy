-module(textguard_publisher_tests).

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
                undefined -> {ok, publication_state(0, <<>>, <<"uninitialized">>)};
                Active -> {ok, Active}
            end;
        Reason ->
            {error, Reason}
    end.

publish_package(Body, Opts) ->
    mock_append(put_attempts, #{
        body => Body,
        if_match => maps:get(if_match, Opts, none),
        at => os:system_time(millisecond)
    }),
    case no_dangling(Body) of
        ok ->
            {ok, Active} = get_active_ruleset(#{}),
            case maps:get(if_match, Opts) =:= maps:get(state_etag, Active) of
                true -> do_publish(Body);
                false -> {error, cas_conflict}
            end;
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
    mock_set(generation, Gen),
    State = publication_state(Gen, sha_hex(Body), <<"active">>),
    mock_set(active, State),
    {ok, State#{status => <<"committed">>}}.

advance_revision() ->
    Gen = mock_get(generation, 0) + 1,
    mock_set(generation, Gen),
    mock_set(active, publication_state(Gen, sha_hex(integer_to_binary(Gen)), <<"active">>)),
    case mock_get(mutate_on_conflict, false) of
        true ->
            textguard_export_tests:insert_word(
                99000999, <<"fresh-on-conflict">>, <<"pub">>, <<"high">>
            );
        false ->
            ok
    end.

publication_state(Gen, Revision, Status) ->
    Generation = integer_to_binary(Gen),
    Instance = <<"persistent-publisher-fixture">>,
    ETag = sha_hex(<<Instance/binary, "\n", Generation/binary, "\n", Revision/binary>>),
    Count =
        case Gen of
            0 -> 0;
            _ -> 2
        end,
    #{
        instance_id => Instance,
        ready => Gen > 0,
        generation => Generation,
        ruleset_revision => Revision,
        state_etag => ETag,
        status => Status,
        scene_counts => #{channel_message => Count, moment_post => Count, profile_field => Count}
    }.

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
    isolated_pg_fixture:test(fun publisher_fixture/0).

publisher_fixture() ->
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
                "CAS: 412 retries once from fresh export with new If-Match",
                fun t_cas_retry_once/0
            ),
            with_case(
                "CAS: exhausted retries leave failed state visible (bounded puts)",
                fun t_cas_exhausted/0
            ),
            with_case("busy does not consume CAS retries", fun t_publish_busy/0),
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

-spec pg_only_setup() -> map().
pg_only_setup() ->
    textguard_export_tests:owned_pg_setup().

-spec pg_only_cleanup(map()) -> ok.
pg_only_cleanup(State) ->
    textguard_export_tests:owned_pg_teardown(State).

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
        #{ruleset_revision := D} -> D
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
        #{ruleset_revision := _, generation := <<"1">>, state_etag := _, digest := _},
        maps:get(last_published, Status)
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
    mock_set(mutate_on_conflict, true),
    {ok, _} = start_short(),
    wait_published(),
    Attempts = mock_attempts(),
    ?assertEqual(2, length(Attempts)),
    [First, Second] = Attempts,
    ?assertEqual(
        maps:get(state_etag, publication_state(0, <<>>, <<"uninitialized">>)),
        maps:get(if_match, First)
    ),
    ?assertEqual(
        maps:get(state_etag, publication_state(1, sha_hex(<<"1">>), <<"active">>)),
        maps:get(if_match, Second)
    ),
    ?assertNotEqual(maps:get(body, First), maps:get(body, Second)),
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

t_publish_busy() ->
    {ok, _} = start_long(),
    wait_published(),
    Before = length(mock_attempts()),
    mock_set(put_error, publish_busy),
    textguard_export_tests:insert_word(99000998, <<"pending-busy">>, <<"pub">>, <<"low">>),
    {ok, Status} = ?PUB:reconcile(),
    ?assertMatch(
        #{state := failed, pending := true, last_error := {publish, publish_busy}}, Status
    ),
    ?assertEqual(Before + 1, length(mock_attempts())),
    mock_set(put_error, undefined),
    {ok, #{state := published, pending := false}} = ?PUB:reconcile().

%% -------------------------------------------------------------------
%% 真实 HTTP 适配器发布面（ewh_http_fixture 真 socket）
%% -------------------------------------------------------------------

t_http_adapter_publish() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, Export} = textguard_export:export_package(),
    Package = maps:get(package, Export),
    Initial = publication_state(0, <<>>, <<"uninitialized">>),
    Published = publication_state(1, sha_hex(Package), <<"committed">>),
    mock_set(http_reply, {200, jsone:encode(Initial)}),
    H = ewh_http_fixture:start(fun(_) ->
        {Code, Body} = mock_get(http_reply, undefined),
        {reply, Code, [], Body}
    end),
    try
        BaseUrl = <<"http://127.0.0.1:", (integer_to_binary(ewh_http_fixture:port(H)))/binary>>,
        application:set_env(
            imboy,
            moderation_detector,
            [{http_base_url, BaseUrl}, {publish_token, <<"publish-token-placeholder">>}]
        ),
        Ctx = #{deadline_ms => 2000, request_id => <<"publish-001">>},
        ?assertEqual({ok, Initial}, moderation_detector_http:get_active_ruleset(Ctx)),
        ?assertEqual(
            {error, protocol_error}, moderation_detector_http:publish_package(Package, #{})
        ),
        ?assertEqual(1, ewh_http_fixture:request_count(H)),
        Opts = #{if_match => maps:get(state_etag, Initial), ctx => Ctx, confirm_empty => true},
        mock_set(http_reply, {200, jsone:encode(Published)}),
        ?assertEqual({ok, Published}, moderation_detector_http:publish_package(Package, Opts)),
        Request = lists:last(ewh_http_fixture:requests(H)),
        ?assertEqual(Package, maps:get(body, Request)),
        Raw = maps:get(raw, Request),
        lists:foreach(
            fun(Header) -> ?assertNotEqual(nomatch, binary:match(Raw, Header)) end,
            [
                <<"if-match: ", (maps:get(state_etag, Initial))/binary, "\r\n">>,
                <<"authorization: Bearer publish-token-placeholder\r\n">>,
                <<"x-textguard-confirm-empty: true\r\n">>,
                <<"x-textguard-request-id: publish-001\r\n">>
            ]
        ),
        Already = Published#{status => <<"already_active">>},
        mock_set(http_reply, {200, jsone:encode(Already)}),
        ?assertEqual({ok, Already}, moderation_detector_http:publish_package(Package, Opts)),
        lists:foreach(
            fun({Status, Code, Reason}) ->
                mock_set(
                    http_reply,
                    {Status,
                        jsone:encode(#{
                            error => #{
                                code => Code,
                                request_id => <<"publish-001">>,
                                retryable => true
                            }
                        })}
                ),
                ?assertEqual(
                    {error, Reason}, moderation_detector_http:publish_package(Package, Opts)
                )
            end,
            [
                {412, <<"CAS_CONFLICT">>, cas_conflict},
                {409, <<"PUBLISH_BUSY">>, publish_busy},
                {409, <<"RETIRED_SNAPSHOT_BUSY">>, retired_snapshot_busy},
                {409, <<"CAS_CONFLICT">>, protocol_error},
                {500, <<"PUBLISH_STATE_UNKNOWN">>, publish_state_unknown}
            ]
        ),
        application:set_env(imboy, moderation_detector, [{http_base_url, BaseUrl}]),
        ?assertEqual({error, not_ready}, moderation_detector_http:publish_package(Package, Opts))
    after
        ewh_http_fixture:stop(H),
        clean_gun_cache(),
        application:unset_env(imboy, moderation_detector)
    end.

%% 进程字典 gun 连接缓存清理（T15 测试同款惯例）
-spec clean_gun_cache() -> ok.
clean_gun_cache() ->
    [
        erase(K)
     || {K, _} <- get(),
        is_tuple(K) andalso element(1, K) =:= gun_conn
    ],
    ok.
