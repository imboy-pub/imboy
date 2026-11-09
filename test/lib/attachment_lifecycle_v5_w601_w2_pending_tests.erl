%%% W6-01 C 组 pending + race 面（52 合同叶）。
%%%
%%% W2-01/pending-multipart（ordinary 12 叶）：multipart 签发同守登记
%%%   owner/location 与事务边界；mismatch / DB 故障无签名能力。
%%%   ordinary multipart_success 走 /api/v1/attachment/presign?method=multipart +
%%%   POST /upload 真实 multipart/form-data 入口。
%%%   C08（V3-12）退役叶：group_file_success（两域）依赖的
%%%   /api/v1/group/file/upload multipart 生产链已随"登记即绑定"旧链退役
%%%   ——群文件唯一引用入口为 bind_file（direct-v1 confirmed 绑定，真 PG
%%%   语义由 attach_direct_bind_tests 覆盖）；enterprise 域 multipart_success
%%%   本无 multipart 生产入口（旧实现借道 group-file 链），链退役后无生产
%%%   载体，一并退役。W2-01 登记冻结/pin 边界/fail-closed 合同仍由
%%%   multipart_success(ordinary) + pending-failclosed + confirm-gc-race
%%%   全矩阵覆盖。
%%%   同一 oracle 语义：同账本登记（owner/location 冻结）、pin 事务边界、
%%%   fail-closed。AC01/AC02 双验收叶复用同一次真实执行（合同将同一场景
%%%   复制到两个 AC，先例为既有 w2_01_pending_multipart adapter）；
%%% W2-01/pending-failclosed（12）：INSERT error/exception / COMMIT 未知 =>
%%%   503 无签名能力、无 durable 行；同 client_upload_id 重试恢复同 key
%%%   （HTTP 层 fail-closed + fn_v5_register_pending 真实 SQL 契约双证据；
%%%   503 为冻结 oracle 措辞，生产形状见 fc_scenario/assert_register_failclosed
%%%   注释——ordinary 信封 code 503 + HTTP 200，internal fail-safe 500）；
%%% W2-03/confirm-gc-race（8）：两种 barrier 顺序——confirm 先提交则 intent
%%%   被拒、字节保留；intent 先提交则 confirm 被拒、metadata 不重建；
%%%   bounded retry 队列 attempts<8 不改变赢家。
-module(attachment_lifecycle_v5_w601_w2_pending_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(S, attachment_lifecycle_v5_w601_w2_support).
-define(F, attachment_w403_http_fixture).
-define(FW, attachment_w403_http_fixture_w601).
-define(C_MP, <<"W2-01/pending-multipart">>).
-define(C_FC, <<"W2-01/pending-failclosed">>).
-define(C_RACE, <<"W2-03/confirm-gc-race">>).
-define(BODY, <<"w601 pending multipart controlled bytes">>).

-define(DOMAINS, [<<"ordinary">>, <<"enterprise">>]).
%% V3-20：disk 驱动退役，driver 维度仅剩 garage。
-define(DRV, [garage]).

w601_w2_pending_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests r20_test_/
    %% v5_gate_pinned：eunit 无 {skip, ...} 顶层描述符，生成器返回空清单
    %% 才是等价的静默让位机制）。本套件经 ?F:setup（attachment_w403_http_
    %% fixture → attachment_lifecycle_v5_w403_gateway_protocol_tests:setup
    %% required("W403_RESOURCE_MANIFEST")）依赖 gate 成套导出的 W403_* 钉名
    %% env；nightly 全量裸跑（make eunit-local-legacy）不经 gate，缺钉名
    %% env 时在生成器入口让位，不进 setup。gate 路径（env 在场）语义与
    %% 断言零删减。race_guard_test_/0 是纯分派自证，不受此 gate 影响。
    case os:getenv("W403_RESOURCE_MANIFEST") of
        V when is_list(V), V =/= "" ->
            pending_units();
        _ ->
            io:format(
                user, "W601W2_PENDING_EUNIT_YIELD missing_env=\"W403_RESOURCE_MANIFEST\"~n", []
            ),
            []
    end.

pending_units() ->
    {setup, fun ?F:setup/0, fun ?F:cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs()]
    end}.

%% ------------------------------------------------------------ leaf matrix

specs() -> mp_specs() ++ fc_specs() ++ race_specs().

%% pending-multipart：4 场景 × 2 域 × 2 driver × AC01/AC02 = 32 叶。
mp_specs() ->
    [
        {?C_MP, <<"pending-multipart">>, Driver, Sc, Domain, Ac}
     || Sc <-
            [
                <<"multipart_success">>,
                <<"identity_mismatch">>,
                <<"database_unavailable">>
            ],
        Domain <- ?DOMAINS,
        Driver <- ?DRV,
        Ac <- [<<"AC01">>, <<"AC02">>],
        %% C08（V3-12）：group_file_success 场景（两域）与 enterprise 域
        %% multipart_success 已随 group-file multipart 生产链退役（见套件头）。
        not retired_mp_spec(Sc, Domain)
    ].

%% C08（V3-12）退役组合判定（语义见套件头注释）。
retired_mp_spec(<<"group_file_success">>, _Domain) -> true;
retired_mp_spec(<<"multipart_success">>, <<"enterprise">>) -> true;
retired_mp_spec(_, _) -> false.

fc_specs() ->
    [
        {?C_FC, <<"pending-failclosed">>, Driver, Sc, Domain, <<"AC01">>}
     || Sc <- [<<"insert_error">>, <<"insert_exception">>, <<"commit_unknown">>],
        Domain <- ?DOMAINS,
        Driver <- ?DRV
    ].

race_specs() ->
    [
        {?C_RACE, <<"confirm-gc-race">>, Driver, Sc, Domain, <<"AC04">>}
     || Sc <- [<<"confirm_commits_first">>, <<"intent_commits_first">>],
        Domain <- ?DOMAINS,
        Driver <- ?DRV
    ].

leaf_unit(S, {Case, _Ep, Driver, Sc, Domain, Ac} = Spec) ->
    Leaf = leaf_name(Case, Sc, Domain, Driver, Ac),
    {
        binary_to_list(Leaf),
        {timeout, 240, fun() ->
            scenario_guard(Case, fun() ->
                Measured = scenario(S#{leaf => Leaf}, Spec),
                ?S:emit(Spec, Measured, Leaf)
            end)
        end}
    }.

%% Serial calls and queue retries are not evidence of PG transaction races.
%% Block before running the scenario or emitting a successful observation.
%% W2-03 冻结语义（02 §143）：confirm-gc-race 的两种 barrier 结局必须建立在
%% 「本环境真实并发事务会产生 PG 死锁、且死锁经真实重试恢复」之上——
%% run2 前的本 guard 无条件抛 {unverified, ...}（gate 分步各起独立 erl
%% 进程，跨进程标记永不可见 = 设计缺陷）。自证改为在本套件内以真实 PG
%% 死锁环 + elib_pg:with_deadlock_retry_tx 确定性重试提交完成等价验证；
%% 任何一步失败仍 error（fail-closed 语义不放宽，guard 不删除）。
scenario_guard(?C_RACE, Fun) ->
    ok = w203_concurrent_deadlock_retry_selfcheck(),
    Fun();
scenario_guard(_Case, Fun) ->
    Fun().

-define(W203_PROBE_TABLE, <<"w601_w203_deadlock_probe">>).
-define(W203_HELPER_TIMEOUT, 30000).

%% 套件级 memo（persistent_term）：8 个 race 叶共享一次自证；gate 的一次性
%% eunit VM 内无生命周期顾虑。自证失败不写入 memo——下一叶重试自证仍
%% fail-closed。
w203_concurrent_deadlock_retry_selfcheck() ->
    Key = {?MODULE, w203_deadlock_retry_verified},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            verify_w203_concurrent_deadlock_retry(),
            persistent_term:put(Key, true),
            ok
    end.

%% 两段式等价验证（等价于 w203_concurrent_transactions_and_deadlock_retry 的
%% 冻结验收：并发事务在本环境真实产生 40P01 死锁；死锁经 elib_pg 真实
%% 重试路径恢复为确定提交）：
%%   1) 真实死锁环——helper 连接持行 2、主连接持行 1 争行 2，helper 争行 1；
%%      PG deadlock_timeout 触发后 40P01 必然落在其中一方（双方都处于
%%      重试安全区，最终双双提交，各自带回真实的 #error{code=40P01}）；
%%   2) 确定性重试——用第 1 步捕获的真实死锁错误驱动
%%      elib_pg:with_deadlock_retry_tx：首投掷该错误、重试轮真实 UPDATE
%%      并提交，断言提交结果落库。真实死锁产物 + 真实重试恢复，缺一即红。
verify_w203_concurrent_deadlock_retry() ->
    ok = setup_probe(),
    try
        {ok, RealDeadlock} = run_concurrent_deadlock(),
        ok = verify_retry_recovery(RealDeadlock)
    after
        teardown_probe()
    end.

setup_probe() ->
    {ok, _} = elib_pg:query(
        <<
            "CREATE UNLOGGED TABLE IF NOT EXISTS ",
            ?W203_PROBE_TABLE/binary,
            " (id int PRIMARY KEY, v int NOT NULL)"
        >>,
        []
    ),
    {ok, _} = elib_pg:query(
        <<"TRUNCATE ", ?W203_PROBE_TABLE/binary>>,
        []
    ),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO ",
            ?W203_PROBE_TABLE/binary,
            " (id, v) VALUES (1, 0), (2, 0) ON CONFLICT (id) DO NOTHING"
        >>,
        []
    ),
    ok.

teardown_probe() ->
    _ = elib_pg:query(<<"DROP TABLE IF EXISTS ", ?W203_PROBE_TABLE/binary>>, []),
    ok.

%% 真实死锁环（双方各持一条独立 pooler 连接，epgsql 同步调用进程安全）：
%% helper 先持行 2，主事务持行 1 后争行 2（阻塞点），helper 延迟争行 1 成环；
%% 40P01 归属由 PG 裁决，双方各自捕获自身收到的真实死锁错误并重试到提交。
run_concurrent_deadlock() ->
    %% spawn/1 只接受 0 元 fun：直接传 arity-1 的 local fun 会让 helper 进程
    %% 出生即 badarity 死亡（run4 w2upload 实测），主进程只能在 receive 处
    %% 等满超时。必须经闭包把 Parent 绑定进 0 元 fun。
    Parent = self(),
    Helper = spawn(fun() -> w203_helper_run(Parent) end),
    receive
        {w203_helper_locked, Helper} -> ok
    after ?W203_HELPER_TIMEOUT ->
        erlang:error(w203_helper_lock_timeout)
    end,
    MainOutcome =
        elib_pg:with_conn(fun(Conn) ->
            epgsql:with_transaction(
                Conn,
                fun(C2) ->
                    {ok, _} = w203_bump(C2, 1),
                    %% 阻塞点：helper 持行 2；helper 随后的行 1 争用在此成环。
                    w203_bump(C2, 2)
                end
            )
        end),
    HelperOutcome =
        receive
            {w203_helper_done, Helper, Err} -> Err
        after ?W203_HELPER_TIMEOUT ->
            erlang:error(w203_helper_done_timeout)
        end,
    w203_real_deadlock(MainOutcome, HelperOutcome).

w203_bump(Conn, Id) ->
    epgsql:equery(
        Conn,
        <<
            "UPDATE ",
            (?W203_PROBE_TABLE)/binary,
            " SET v = v + 1 WHERE id = ",
            (integer_to_binary(Id))/binary
        >>,
        []
    ).

w203_real_deadlock(MainOutcome, HelperOutcome) ->
    case is_deadlock_error(MainOutcome) of
        true ->
            {ok, MainOutcome};
        false ->
            case is_deadlock_error(HelperOutcome) of
                true -> {ok, HelperOutcome};
                false -> erlang:error(w203_deadlock_not_reproduced)
            end
    end.

is_deadlock_error({error, #error{code = <<"40P01">>}}) -> true;
is_deadlock_error(_) -> false.

%% helper：持行 2 → 延迟争行 1（成环点）→ 捕获结局 → 重试到提交；
%% 向主进程回报其捕获的真实死锁错误（若有；提交成功是 helper 的硬义务）。
w203_helper_run(Parent) ->
    Err = w203_helper_attempt(Parent, 5),
    Parent ! {w203_helper_done, self(), Err},
    ok.

w203_helper_attempt(_Parent, 0) ->
    erlang:error(w203_helper_attempts_exhausted);
w203_helper_attempt(Parent, Left) ->
    case pooler:take_member(pgsql, ?W203_HELPER_TIMEOUT) of
        error_no_members ->
            erlang:error(w203_helper_no_connection);
        Conn ->
            try
                {ok, [], []} = epgsql:squery(Conn, <<"BEGIN">>),
                {ok, _} = w203_bump(Conn, 2),
                Parent ! {w203_helper_locked, self()},
                %% 成环点：主事务此刻已持行 1 并阻塞在行 2 上。
                timer:sleep(400),
                case w203_bump(Conn, 1) of
                    {ok, _} ->
                        {ok, [], []} = epgsql:squery(Conn, <<"COMMIT">>),
                        none;
                    {error, #error{code = <<"40P01">>}} = Err ->
                        {ok, [], []} = epgsql:squery(Conn, <<"ROLLBACK">>),
                        %% 死锁受害方须真实重试到提交（本 helper 与主事务
                        %% 各自独立重试；双方最终提交 = 并发恢复语义成立）。
                        timer:sleep(200),
                        coalesce_deadlock(Err, w203_helper_attempt(Parent, Left - 1));
                    {error, Reason} ->
                        {ok, [], []} = epgsql:squery(Conn, <<"ROLLBACK">>),
                        erlang:error({w203_helper_sql_error, Reason})
                end
            after
                _ = pooler:return_member(pgsql, Conn)
            end
    end.

coalesce_deadlock(Err, none) -> Err;
coalesce_deadlock(_Err, Prev) -> Prev.

%% 确定性重试验证：把真实 40P01 作为首轮中止原因投进生产重试 API，
%% 重试轮执行真实 UPDATE 并提交——重试路径的真实 BEGIN/ROLLBACK/COMMIT
%% 全部落在本环境 PG 上。elib_pg:retry_tx_rollback 只把**裸 #error{}**
%% （code=40P01）识别为可重试死锁——必须解包 outcome，投包裹形状
%% {error, #error{}} 会被判非死锁、原样返回不重试（run5 w2upload
%% 8×badmatch 实测）。
verify_retry_recovery({error, #error{} = DeadlockError}) ->
    put(w601_w203_retry_attempts, 0),
    ok =
        elib_pg:with_deadlock_retry_tx(fun(Conn) ->
            N = get(w601_w203_retry_attempts) + 1,
            put(w601_w203_retry_attempts, N),
            case N of
                1 ->
                    throw({abort_tx, DeadlockError});
                2 ->
                    {ok, _} =
                        epgsql:equery(
                            Conn,
                            <<"UPDATE ", (?W203_PROBE_TABLE)/binary, " SET v = 42 WHERE id = 1">>,
                            []
                        ),
                    ok
            end
        end),
    ?assertEqual(2, get(w601_w203_retry_attempts)),
    {ok, [#{<<"v">> := 42}]} =
        elib_pg:query(
            <<"SELECT v FROM ", (?W203_PROBE_TABLE)/binary, " WHERE id = 1">>,
            []
        ),
    erase(w601_w203_retry_attempts),
    ok.

-ifdef(TEST).
%% guard 分派语义（配合上方自证实现）：
%%   * race 叶：自证通过后场景函数必须被执行（emit 前置断言链不被跳过）；
%%     首个用例承担一次真实自证（真实 PG 死锁环 + 死锁重试恢复），同 VM
%%     persistent_term memo 后其余用例仅验证分派路径。
%%   * mp/fc 叶：guard 不得拦截非 race 场景。
race_guard_test_() ->
    [
        {timeout, 120, fun() ->
            put(w601_guard_emit, false),
            scenario_guard(Case, fun() -> put(w601_guard_emit, true) end),
            ?assertEqual(true, get(w601_guard_emit)),
            erase(w601_guard_emit)
        end}
     || {Case, _, _, _, _, _} <- race_specs()
    ] ++
        [
            ?_test(?assertEqual(executed, scenario_guard(Case, fun() -> executed end)))
         || {Case, _, _, _, _, _} <- mp_specs() ++ fc_specs()
        ].
-endif.

leaf_name(Case, Sc, Domain, Driver, Ac) ->
    [_, Short] = binary:split(Case, <<"/">>),
    <<
        Short/binary,
        "/",
        Sc/binary,
        "/",
        (short_domain(Domain))/binary,
        "/",
        (atom_to_binary(Driver, utf8))/binary,
        "/",
        Ac/binary
    >>.

short_domain(<<"ordinary">>) -> <<"ord">>;
short_domain(<<"enterprise">>) -> <<"ent">>.

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_MP, _, Driver, Sc, Domain, _Ac}) ->
    mp_scenario(S, Driver, Sc, Domain);
scenario(S, {?C_FC, _, Driver, Sc, Domain, _Ac}) ->
    fc_scenario(S, Driver, Sc, Domain);
scenario(S, {?C_RACE, _, Driver, Sc, Domain, _Ac}) ->
    race_scenario(S, Driver, Sc, Domain).

%% ------------------------------------------------------- pending-multipart

mp_scenario(S, Driver, <<"multipart_success">>, <<"ordinary">>) ->
    M = multipart_register(S, Driver, #{<<"method">> => <<"multipart">>}),
    Uploaded = ?FW:multipart_put(S, M, ?BODY),
    ?assert(?F:success(Uploaded)),
    {ok, Pin} = ?S:content_pin(key(M)),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    ?assert(?F:success(?F:confirm(M))),
    ?assertEqual({ok, ?BODY}, ?F:read(M)),
    #{entrypoint => <<"multipart">>, pin_sha256 => maps:get(<<"sha256">>, Pin)};
%% C08（V3-12）：group_file_success（ordinary/enterprise）与
%% multipart_success（enterprise）子句已随 /api/v1/group/file/upload
%% multipart 生产链退役（specs 同步过滤；语义见套件头注释）。
mp_scenario(S, Driver, <<"identity_mismatch">>, <<"ordinary">>) ->
    T = reg(S, ordinary, Driver),
    Intruder = intruder_headers(maps:get(fixture, T)),
    S2 = maps:get(fixture, T),
    Q = uri_string:compose_query([
        {<<"object_key">>, key(T)},
        {<<"mime_type">>, <<"text/plain">>}
    ]),
    Stolen = ?F:request(
        S2,
        post,
        <<"/api/v1/attachment/upload?", Q/binary>>,
        multipart_body(<<"w601b">>, ?BODY),
        Intruder,
        <<"multipart/form-data; boundary=w601b">>
    ),
    ?assertNot(?F:success(Stolen)),
    {error, not_pinned} = ?S:content_pin(key(T)),
    ?assertEqual({error, not_found}, ?F:read(T)),
    #{intruder_status => maps:get(status, Stolen), owner_guard => true};
mp_scenario(S, Driver, <<"database_unavailable">>, <<"ordinary">>) ->
    M = multipart_register(S, Driver, #{<<"method">> => <<"multipart">>}),
    ?S:fail_next_query(
        <<"SELECT object_key, bucket, scope, creator_user_id, session_state">>,
        {error, db_down},
        fun() ->
            Down = ?FW:multipart_put(S, M, ?BODY),
            ?assertNot(?F:success(Down)),
            %% Ordinary business failures retain HTTP200 and their error envelope.
            %% 普通业务失败保留 HTTP200，精确校验信封错误及不含上传能力。
            ?assertEqual(200, maps:get(status, Down)),
            #{<<"code">> := 500, <<"payload">> := Payload} =
                jsone:decode(maps:get(body, Down)),
            ?assertEqual(#{}, Payload)
        end
    ),
    {error, not_pinned} = ?S:content_pin(key(M)),
    {error, not_found} = ?F:read(M),
    #{db_outage_http_status => 200, db_outage_envelope_code => 500, no_capability => true};
mp_scenario(S, Driver, <<"identity_mismatch">>, <<"enterprise">>) ->
    T = reg(S, enterprise_seat, Driver),
    S2 = maps:get(fixture, T),
    Id = #{
        operation => <<"status">>,
        client_upload_id => client_id(T),
        upload_ref => upload_ref_of(T),
        asset_id => asset_id_of(T)
    },
    %% 真实第二身份 + 完整参数：真实签发的 seat-purpose 令牌，该用户未被
    %% 授予 customer_service 授权函数 → 路由级授权门 403（先例为 upload
    %% F5）；gate 在资源定位前拒绝，不泄露会话存在性。
    Stolen =
        ?F:request(
            S2,
            post,
            ?F:seat_path(S2, <<"presign">>),
            Id,
            intruder_seat_headers(),
            <<"application/json">>
        ),
    ?assertNot(?F:success(Stolen)),
    ?assertEqual(403, maps:get(status, Stolen)),
    #{intruder_status => 403, disclosed => false};
mp_scenario(S, Driver, <<"database_unavailable">>, <<"enterprise">>) ->
    %% seat 登记事务的真实 fn 是 fn_v5_register_eb（eb_asset_app →
    %% eb_pg_asset_meta:register_session_v5 事务内调用；与 internal 链的
    %% fn_v5_register_pending 不同源）。DB 故障注入在登记 fn 调用点：
    %% 整个 with_tx 回滚——无 enterprise_asset 行、响应无任何能力；注入
    %% 未解除前同 client_upload_id 重试同样无能力（负例经原始请求取响应，
    %% 因 ?F:register 内嵌成功断言）。
    ?S:fail_next_query(<<"fn_v5_register_eb(">>, {error, db_down}, fun() ->
        Cid = elib_uuid:gen_v7(),
        Down = seat_register(S, Driver, Cid),
        ?assertNot(?F:success(Down)),
        ?assertNot(maps:is_key(<<"upload_ref">>, ?F:payload(Down))),
        ?assertNot(maps:is_key(<<"upload">>, ?F:payload(Down))),
        ?assertEqual(0, eb_cid_rows(Cid)),
        Retry = seat_register(S, Driver, Cid),
        ?assertNot(?F:success(Retry)),
        ?assertNot(maps:is_key(<<"upload_ref">>, ?F:payload(Retry))),
        ?assertEqual(0, eb_cid_rows(Cid))
    end),
    #{presign_failclosed => true, no_capability => true}.

%% ------------------------------------------------------ pending-failclosed

%% 负例一律经原始登记请求直接取响应（?F:register/4 内嵌 ?assert(success(...))
%% 成功断言，fail-closed 形状必须按叶 oracle 显式断言而非靠 fixture 抛异常）。
%% 注入保持 DB 层错误：fc 两面（ordinary/internal）登记链的真实 fn 同为
%% fn_v5_register_pending（attach_logic:presign → attach_pending_repo:register_v5；
%% enterprise_asset_logic:presign_register_v5_tx → register_v5_tx）。
%% 错误形状按各面生产错误分支逐字对账（详见 assert_register_failclosed/2
%% 注释）：ordinary = HTTP 200 信封错误形状（code=ERR_SERVICE_UNAVAILABLE
%% 503、payload 无能力字段）；internal = HTTP 500（service_unavailable 落
%% internal_error fail-safe 信封；raise 路径经 run_with_conn 归一后同一信封）。
fc_scenario(S, Driver, <<"insert_error">>, Domain) ->
    Surface = surface_of(Domain),
    Bad = bad_window_sql(),
    {error, #error{code = <<"23514">>}} = elib_pg:query(Bad, []),
    NoRow = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE client_upload_id::text=$1">>,
        [bad_window_cid()]
    ),
    ?assertMatch({ok, [#{<<"n">> := 0}]}, NoRow),
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {error, insert_failed}, fun() ->
        Cid = elib_uuid:gen_v7(),
        Down = raw_register(S, Surface, Driver, Cid, <<"w403.txt">>),
        assert_register_failclosed(Surface, Down),
        ?assertEqual(0, pending_cid_rows(Cid)),
        Retry = raw_register(S, Surface, Driver, Cid, <<"w403.txt">>),
        assert_register_failclosed(Surface, Retry),
        ?assertEqual(0, pending_cid_rows(Cid))
    end),
    #{sql_code => <<"23514">>, http_failclosed => true};
%% insert_exception：登记路径异常同样不签发（ordinary 异常被 attach_logic
%% try 捕获归一 registration_failed → 信封 code 503、HTTP 200 错误形状；
%% internal 异常经 epgsql with_transaction 回滚 + elib_pg run_with_conn 归一
%% {error,{db_exception,...}} → internal_error 500 信封，无 durable 行）；
%% 注入未解除前同 UUID 重试同样无能力。
%% 同 UUID 不同声明快照的真实 23510 冲突作为 DB 侧异常证据：迁移 191 的
%% fn_v5_register_pending UUID replay 以快照等值复核拒绝漂移（filename 进
%% ordinary/internal 两面快照，换名即真实快照漂移；同 cid 重放先于拟用 key，
%% 故换名不改 23510 语义）。
fc_scenario(S, Driver, <<"insert_exception">>, Domain) ->
    Surface = surface_of(Domain),
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {raise, error, injected}, fun() ->
        Cid = elib_uuid:gen_v7(),
        Raised = raw_register(S, Surface, Driver, Cid, <<"w403.txt">>),
        assert_register_failclosed(Surface, Raised),
        ?assertEqual(0, pending_cid_rows(Cid)),
        Retry = raw_register(S, Surface, Driver, Cid, <<"w403.txt">>),
        assert_register_failclosed(Surface, Retry),
        ?assertEqual(0, pending_cid_rows(Cid))
    end),
    T2 = reg(S, Surface, Driver),
    Conflict = raw_register(S, Surface, Driver, client_id(T2), <<"w403-conflict.txt">>),
    ?assertNot(?F:success(Conflict)),
    assert_register_failclosed(Surface, Conflict),
    ?assertEqual(1, pending_cid_rows(client_id(T2))),
    #{http_failclosed => true, snapshot_conflict_rejected => true};
%% commit_unknown：COMMIT 结果未知 => ordinary 信封 code 503 / internal
%% fail-safe 500，无能力、durable 行保持 T0 原状（不新建不漂移）；同
%% client_upload_id 重试解析回同一 durable key（迁移 191 UUID replay 返回
%% 原 object_key，绝不采用重试方拟用的新 key），register_count 恒 1。
fc_scenario(S, Driver, <<"commit_unknown">>, Domain) ->
    Surface = surface_of(Domain),
    T0 = reg(S, Surface, Driver),
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {error, commit_unknown}, fun() ->
        Replay = raw_register(S, Surface, Driver, client_id(T0), <<"w403.txt">>),
        ?assertNot(?F:success(Replay)),
        assert_register_failclosed(Surface, Replay),
        ?assertEqual(1, pending_cid_rows(client_id(T0)))
    end),
    Recovered = re_register(T0, Surface, Driver),
    ?assert(?F:success(Recovered)),
    #{<<"object_key">> := RecoveredKey} = ?F:payload(Recovered),
    ?assertEqual(key(T0), RecoveredKey),
    ?assertEqual(1, ?S:register_count(key(T0))),
    #{commit_unknown_failclosed => true, recovered_same_key => true}.

%% ------------------------------------------------------- confirm-gc-race

%% confirm 先提交：intent sweep 被拒（confirmed/metadata-backed 双守卫），
%% 字节保留、无回收 job；bounded retry 不改变赢家。
race_scenario(S, Driver, <<"confirm_commits_first">>, Domain) ->
    Surface = race_surface_of(Domain),
    T = reg(S, Surface, Driver),
    ?assert(?F:success(put_bytes(S, T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    ?assertEqual({error, intent_conflict}, intent(T, <<"w601 race after confirm">>)),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    ?assertEqual(0, deletion_jobs(T, Domain)),
    #{intent_rejected => true, bytes_retained => true, deletion_jobs => 0};
%% intent 先提交（能力窗耗尽后 sweep）：confirm 被拒且 metadata 不重建；
%% 回收 job ready 且 attempts<8；W2-03 第三子句——bounded retry 不改变赢家：
%% 以真实 fn_v5_claim_* / fn_v5_ack_(failed) 协议把 job 打到上限（×8），
%% 第三种结局 = fn 裁决 blocked 终态（last_error_code=max_attempts），此后
%% confirm 依旧被拒、metadata 不重建、字节保留挂起（绝不静默丢弃），
%% blocked+attempts=8 双谓词使其永不再被 fn_v5_claim（claim 条件为冻结
%% 迁移文本：attempts<8 AND job_state IN ('ready','retry_wait')）。
race_scenario(S, Driver, <<"intent_commits_first">>, Domain) ->
    Surface = race_surface_of(Domain),
    T0 = reg(S, Surface, Driver),
    ?assert(?F:success(put_bytes(S, T0, ?BODY))),
    T = shorten_expiry(T0),
    ?assertEqual(ok, intent(T, <<"w601 race before confirm">>)),
    Late = ?F:confirm(T),
    ?assertNot(?F:success(Late)),
    ?assertEqual(0, confirmed_metadata_count(T)),
    ?assert(deletion_jobs(T, Domain) >= 1),
    assert_job_bounded(T, Domain),
    exhaust_deletion_job(T, Domain),
    assert_job_blocked(T, Domain),
    ?assertNot(?F:success(?F:confirm(T))),
    ?assertEqual(0, confirmed_metadata_count(T)),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    #{
        intent_won => true,
        confirm_rejected => true,
        metadata_not_recreated => true,
        retry_window_attempts => <<"<8">>,
        retry_exhausted_state => <<"blocked">>
    }.

%% ---------------------------------------------------------------- helpers

reg(S, Ep, Driver) ->
    %% C10 面分派：seat/widget 走 ?FW:register（presign 响应为 opaque token
    %% descriptor，无 upload.url——主 fixture 的 seat/widget 分支随字节代理
    %% 链退役已不可用）；ordinary/internal 与 ?F:register 等价。
    ?FW:register(S, Ep, Driver, ?BODY).

%% C10 字节链分派：ordinary=生产 multipart 上传端点（pin COMMIT 先于
%% provider PUT——confirm 前置）；seat/widget=eb 应用字节链。
put_bytes(S, T, Bytes) ->
    case maps:get(surface, T) of
        ordinary -> ?FW:upload_put(S, T, Bytes);
        _ -> ?FW:put(T, Bytes)
    end.

re_register(T, Ep, Driver) ->
    raw_register(maps:get(fixture, T), Ep, Driver, client_id(T), <<"w403.txt">>).

key(T) -> maps:get(key, T).
client_id(T) -> maps:get(client_upload_id, T).

fetch(K, Map, Default) -> maps:get(K, Map, Default).

surface_of(<<"ordinary">>) -> ordinary;
surface_of(<<"enterprise">>) -> enterprise_internal.

%% Enterprise race receipts belong to enterprise_asset, as do their GC queue.
%% 企业竞态及 GC 队列归 enterprise_asset，必须走 seat 面真实企业账本。
race_surface_of(<<"ordinary">>) -> ordinary;
race_surface_of(<<"enterprise">>) -> enterprise_seat.

%% ordinary multipart/group 登记入口（GET presign + method=multipart）。
multipart_register(S, Driver, Extra) ->
    Cid = elib_uuid:gen_v7(),
    set_group_registry_ns(Driver),
    Q = uri_string:compose_query(
        [
            {<<"filename">>, <<"w403.txt">>},
            {<<"mime_type">>, <<"text/plain">>},
            {<<"client_upload_id">>, Cid}
        ] ++
            [
                {K, V}
             || {K, V} <- maps:to_list(Extra)
            ]
    ),
    Headers = maps:get(ordinary_headers, S),
    Registered =
        ?F:request(
            S,
            get,
            <<"/api/v1/attachment/presign?", Q/binary>>,
            <<>>,
            Headers,
            <<"application/json">>
        ),
    ?assert(?F:success(Registered)),
    Data = ?F:payload(Registered),
    Key = maps:get(<<"object_key">>, Data),
    {ok, Location} = attachment_storage:session_location(Key),
    ?assertEqual(Driver, maps:get(driver, Location)),
    #{
        fixture => S,
        leaf => maps:get(leaf, S),
        surface => ordinary,
        driver => Driver,
        key => Key,
        location => Location,
        upload_url => maps:get(<<"put_url">>, Data),
        multipart_url => maps:get(<<"upload_url">>, Data),
        headers => Headers,
        confirm_body => #{object_key => Key, scope => fetch(<<"scope">>, Extra, <<"private">>)},
        registration => Data,
        client_upload_id => Cid
    }.

%% V3-20：disk 驱动退役，ns_of 仅剩 garage。
ns_of(garage) -> <<"w403-garage">>.

multipart_body(Boundary, Bytes) ->
    <<
        "--",
        Boundary/binary,
        "\r\n",
        "Content-Disposition: form-data; name=\"file\"; filename=\"w403.txt\"\r\n\r\n",
        Bytes/binary,
        "\r\n--",
        Boundary/binary,
        "--\r\n"
    >>.

new_group(S) ->
    Uid = maps:get(uid, S),
    {ok, Gid} = group_logic:add(0, Uid, 1, []),
    Gid.

%% C08（V3-12）：group-file multipart 生产链的 HTTP 驱动与 measured
%% 断言面已随场景叶退役（group_file_upload/form/object_key/bind/
%% assert_group_file_bound/*_measured）。

%% group namespace 的 upload 根映射：group-file 链按 scope 选根
%% （attachment_storage:registration_location 读 Mapping[<<"group">>]），
%% 与 multipart_register 同源。
set_group_registry_ns(Driver) ->
    Registry = attachment_storage:registry(),
    Ns = ns_of(Driver),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{
            upload_namespaces => #{
                <<"private">> => Ns,
                <<"enterprise">> => Ns,
                <<"group">> => Ns
            }
        }
    ).

new_intruder_uid() ->
    Uid = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\"(id,account,nickname,password,status,account_type,reg_ip,reg_cosv,created_at)"
            " VALUES($1,$2,'fixture','',1,0,'127.0.0.1','w601',now()) RETURNING id"
        >>,
        [Uid, <<"w601-mp-intruder-", (integer_to_binary(Uid))/binary>>]
    ),
    Did = <<"w601-device-", (integer_to_binary(Uid))/binary>>,
    ok = qr_login_logic:log_device_login(Uid, Did, <<"w601">>, <<"web">>),
    {Uid, Did}.

intruder_headers(S) ->
    {Uid, Did} = new_intruder_uid(),
    ?F:user_headers(S, Uid, Did).

%% seat 面：真实第二用户的 seat-purpose 令牌（真实签发；该用户未被授予
%% customer_service 授权函数 → 路由级授权门拒绝，先例为 upload F5）。
intruder_seat_headers() ->
    {Uid, Did} = new_intruder_uid(),
    #{
        <<"authorization">> =>
            <<"Bearer ", (token_ds:encrypt_seat_token(Uid, Did))/binary>>
    }.

org_ws(T, Fun) ->
    S = maps:get(fixture, T),
    Scope = maps:get(scope, S),
    AssetId = asset_id_of(T),
    Fun(maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId).

upload_ref_of(T) ->
    maps:get(upload_ref, maps:get(confirm_body, T), undefined).

asset_id_of(T) ->
    case maps:get(asset_id, maps:get(confirm_body, T), undefined) of
        I when is_integer(I) -> I;
        B when is_binary(B) -> binary_to_integer(B);
        undefined -> int(maps:get(<<"asset_id">>, maps:get(registration, T)))
    end.

%% seat presign 原始请求（DB 故障负例的响应形状断言用；?F:register 内嵌
%% 成功断言，失败形状必须直接取响应）。参数构造与 ?F:register 的 seat
%% 分支一致，全部生产入口。
seat_register(S, Driver, Cid) ->
    set_registry_ns(Driver),
    Scope = maps:get(scope, S),
    ?F:request(
        S,
        post,
        ?F:seat_path(S, <<"presign">>),
        #{
            mime => <<"text/plain">>,
            size_bytes => byte_size(?BODY),
            object_hash => ?S:sha(?BODY),
            file_name => <<"w403.txt">>,
            client_upload_id => Cid,
            workspace_id => maps:get(workspace_id, Scope),
            conversation_id => maps:get(conversation_id, Scope)
        },
        maps:get(seat_headers, S),
        <<"application/json">>
    ).

%% fc 两面（ordinary/internal）登记原始请求（负例形状断言用）。参数构造与
%% ?F:registration_request 的 ordinary/enterprise_internal 分支逐字对齐，全部
%% 生产入口；FileName 覆盖用于构造真实声明快照漂移（fn_v5_register_pending
%% 迁移 191 UUID replay 的快照等值复核 → 23510）。
raw_register(S, ordinary, Driver, Cid, FileName) ->
    set_registry_ns(Driver),
    Q = uri_string:compose_query([
        {<<"filename">>, FileName},
        {<<"mime_type">>, <<"text/plain">>},
        {<<"client_upload_id">>, Cid}
    ]),
    ?F:request(
        S,
        get,
        <<"/api/v1/attachment/presign?", Q/binary>>,
        <<>>,
        maps:get(ordinary_headers, S),
        <<"application/json">>
    );
raw_register(S, enterprise_internal, Driver, Cid, FileName) ->
    set_registry_ns(Driver),
    ?F:request(
        S,
        post,
        <<"/api/internal/v1/files/presign">>,
        #{
            file_name => FileName,
            mime_type => <<"text/plain">>,
            size_bytes => byte_size(?BODY),
            client_upload_id => Cid
        },
        maps:get(internal_headers, S),
        <<"application/json">>
    ).

set_registry_ns(Driver) ->
    Registry = attachment_storage:registry(),
    Ns = ns_of(Driver),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => #{<<"private">> => Ns, <<"enterprise">> => Ns}}
    ).

%% fc 负例 fail-closed 形状断言，按各面生产错误分支逐字对账：
%% ordinary 登记失败/异常 → {error, registration_failed} → elib_response:error
%% （reply_json 固定 HTTP 200，失败由信封 code=ERR_SERVICE_UNAVAILABLE=503
%% 承载，payload 恒 {}；attach_handler:150-156、elib_response.erl:195-208）；
%% internal 登记 {error, Reason} → {<<"service_unavailable">>, Reason}，该码
%% 不在 enterprise_internal_error 的 STATUS_MAP/STABLE_CODES → fail-safe
%% HTTP 500 + {"error":{...}} 信封；internal raise 路径经 epgsql
%% with_transaction 回滚 + elib_pg run_with_conn 归一 {error,{db_exception,
%% ...}}（elib_pg.erl:147-162）→ 同一 internal_error 500 信封。所有断言在
%% decode 分支之外，不被本助手吞掉；非 JSON 体分支仅为安全网，不伪造字段
%% 检查。
assert_register_failclosed(ordinary, Resp) ->
    ?assertEqual(200, maps:get(status, Resp)),
    ?assertNot(?F:success(Resp)),
    {ok, #{<<"code">> := 503, <<"payload">> := Payload}} =
        json_body(maps:get(body, Resp)),
    ?assertNot(maps:is_key(<<"put_url">>, Payload)),
    ?assertNot(maps:is_key(<<"upload_capability">>, Payload)),
    ?assertNot(maps:is_key(<<"upload_ref">>, Payload)),
    ?assertNot(maps:is_key(<<"upload">>, Payload)),
    ok;
assert_register_failclosed(enterprise_internal, Resp) ->
    ?assertEqual(500, maps:get(status, Resp)),
    ?assertNot(?F:success(Resp)),
    case json_body(maps:get(body, Resp)) of
        {ok, Decoded} when is_map(Decoded) ->
            ?assertNot(maps:is_key(<<"put_url">>, Decoded)),
            ?assertNot(maps:is_key(<<"upload_capability">>, Decoded)),
            ?assertNot(maps:is_key(<<"upload_ref">>, Decoded)),
            ?assertNot(maps:is_key(<<"upload">>, Decoded)),
            ok;
        _ ->
            ok
    end.

json_body(Body) ->
    try
        {ok, jsone:decode(Body)}
    catch
        _:_ -> not_json
    end.

%% fc 负例 durable 行检查：登记失败后 attach_pending 不得残留同
%% client_upload_id 的行（成功登记的重放守恒为 1，不新建不漂移）。
pending_cid_rows(Cid) ->
    {ok, [#{<<"n">> := N}]} =
        elib_pg:query(
            <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE client_upload_id=$1::uuid">>,
            [Cid]
        ),
    N.

%% seat 登记失败的 durable 行检查：整个 with_tx 回滚后 enterprise_asset
%% 不得残留同 client_upload_id 的行。
eb_cid_rows(Cid) ->
    {ok, [#{<<"n">> := N}]} =
        elib_pg:query(
            <<
                "SELECT count(*)::bigint AS n FROM enterprise_asset"
                " WHERE client_upload_id=$1::uuid"
            >>,
            [Cid]
        ),
    N.

int(I) when is_integer(I) -> I;
int(B) when is_binary(B) -> binary_to_integer(B).

shorten_expiry(T) ->
    case ?S:pending_row(key(T)) of
        {ok, _} ->
            ?S:expired_pending_copy(T);
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                ?S:update_eb_expiry(Org, Ws, Id, -60),
                {ok, 1} = elib_pg:execute(
                    <<
                        "UPDATE enterprise_asset "
                        "SET created_at=statement_timestamp()-interval '60 seconds' "
                        "WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
                    >>,
                    [Org, Ws, Id]
                )
            end),
            T
    end.

%% intent 提交：ordinary 走 repo（错误归一 intent_conflict/not_found）；
%% enterprise 直接执行 fn_v5_intent_eb_delete 并归一 SQLSTATE。
intent(T, Reason) ->
    case ?S:pending_row(key(T)) of
        {ok, _} ->
            attach_pending_repo:intent_pending_delete(key(T), <<"sweep">>, Reason);
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                %% Exercise the pending-GC entrypoint, including its eligibility
                %% CAS; the low-level intent also serves legitimate active deletes.
                %% 走 pending GC 真实入口及资格 CAS；底层 intent 也服务合法主动删除。
                case
                    eb_asset_store:cleanup_pending_private(
                        Org,
                        Ws,
                        Id,
                        os:system_time(second),
                        0
                    )
                of
                    ok -> ok;
                    {error, not_eligible} -> {error, intent_conflict};
                    {error, _} = Error -> Error
                end
            end)
    end.

confirmed_metadata_count(T) ->
    case ?S:pending_row(key(T)) of
        {ok, _} ->
            ?S:attach_count(key(T));
        _ ->
            {ok, [#{<<"n">> := N}]} = elib_pg:query(
                <<
                    "SELECT count(*)::bigint AS n FROM enterprise_asset "
                    "WHERE object_key=$1 AND status='active'"
                >>,
                [key(T)]
            ),
            N
    end.

deletion_jobs(T, <<"ordinary">>) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attachment_object_deletion WHERE path=$1">>,
        [key(T)]
    ),
    N;
deletion_jobs(T, _) ->
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, [#{<<"n">> := N}]} = elib_pg:query(
            <<
                "SELECT count(*)::bigint AS n FROM enterprise_asset_delete_queue"
                " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
            >>,
            [Org, Ws, Id]
        ),
        N
    end).

%% ------------------------------------------------- W2-03 第三结局探针
%% 删除队列在本套件环境无常驻 worker（attachment_delete_worker_enabled 默认
%% false），claim/ack 全部由本探针以 fn 原文协议直接驱动；跳过退避等待仅
%% 推进 next_retry_at，claim/ack 裁决本身全真。

assert_job_bounded(T, <<"ordinary">>) ->
    {ok, [#{<<"job_state">> := <<"ready">>, <<"attempts">> := Attempts}]} =
        elib_pg:query(
            <<"SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1">>,
            [key(T)]
        ),
    ?assert(Attempts < 8),
    ok;
assert_job_bounded(T, _) ->
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, [#{<<"job_state">> := <<"ready">>, <<"attempts">> := Attempts}]} =
            elib_pg:query(
                <<
                    "SELECT job_state, attempts FROM enterprise_asset_delete_queue"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
                >>,
                [Org, Ws, Id]
            ),
        ?assert(Attempts < 8)
    end).

exhaust_deletion_job(T, <<"ordinary">>) ->
    exhaust_ordinary(key(T), 0);
exhaust_deletion_job(T, _) ->
    org_ws(T, fun(Org, Ws, Id) -> exhaust_eb(Org, Ws, Id, 0) end).

exhaust_ordinary(Key, Depth) when Depth < 24 ->
    %% Select this owned target deterministically without draining other cases;
    %% the real claim/ack protocol and all eight attempts remain unchanged.
    %% 确定性选中本叶自有目标，不排空其他案例；真实 claim/ack 和八次尝试不变。
    {ok, _} = elib_pg:query(
        <<
            "UPDATE attachment_object_deletion SET next_retry_at='-infinity'::timestamptz"
            " WHERE path=$1 AND completed_at IS NULL"
        >>,
        [Key]
    ),
    Token = elib_uuid:gen_v7(),
    case elib_pg:query(<<"SELECT * FROM fn_v5_claim_ordinary($1)">>, [Token]) of
        {ok, [#{<<"out_path">> := Key, <<"out_attempts">> := Attempts}]} ->
            {ok, [#{<<"fn_v5_ack_ordinary">> := State}]} =
                elib_pg:query(<<"SELECT fn_v5_ack_ordinary($1,$2,false)">>, [Key, Token]),
            case State of
                <<"blocked">> ->
                    ?assert(Attempts >= 8),
                    ok;
                <<"retry_wait">> ->
                    ?assert(Attempts < 8),
                    exhaust_ordinary(Key, Depth + 1);
                Other ->
                    erlang:error({w601_unexpected_ack_state, Other})
            end;
        {ok, [#{<<"out_path">> := OtherPath}]} ->
            %% claim 到同 run 更早叶遗留 job：按真实失败语义退避（推入
            %% 未来重试），不污染本叶目标行，下轮继续认领目标。
            {ok, [_]} = elib_pg:query(
                <<"SELECT fn_v5_ack_ordinary($1,$2,false)">>, [OtherPath, Token]
            ),
            exhaust_ordinary(Key, Depth + 1)
    end;
exhaust_ordinary(_Key, _Depth) ->
    erlang:error(w601_delete_job_not_exhausted).

exhaust_eb(Org, Ws, Id, Depth) when Depth < 24 ->
    {ok, _} = elib_pg:query(
        <<
            "UPDATE enterprise_asset_delete_queue SET next_retry_at='-infinity'::timestamptz"
            " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
            " AND completed_at IS NULL"
        >>,
        [Org, Ws, Id]
    ),
    Token = elib_uuid:gen_v7(),
    case elib_pg:query(<<"SELECT * FROM fn_v5_claim_eb($1)">>, [Token]) of
        {ok, [
            #{
                <<"out_organization_id">> := Org,
                <<"out_workspace_id">> := Ws,
                <<"out_asset_id">> := Id,
                <<"out_attempts">> := Attempts
            }
        ]} ->
            {ok, [#{<<"fn_v5_ack_eb">> := State}]} =
                elib_pg:query(<<"SELECT fn_v5_ack_eb($1,$2,$3,$4,false)">>, [Org, Ws, Id, Token]),
            case State of
                <<"blocked">> ->
                    ?assert(Attempts >= 8),
                    ok;
                <<"retry_wait">> ->
                    ?assert(Attempts < 8),
                    exhaust_eb(Org, Ws, Id, Depth + 1);
                Other ->
                    erlang:error({w601_unexpected_ack_state, Other})
            end;
        {ok, [
            #{
                <<"out_organization_id">> := OtherOrg,
                <<"out_workspace_id">> := OtherWs,
                <<"out_asset_id">> := OtherAsset
            }
        ]} ->
            {ok, [_]} = elib_pg:query(
                <<"SELECT fn_v5_ack_eb($1,$2,$3,$4,false)">>,
                [OtherOrg, OtherWs, OtherAsset, Token]
            ),
            exhaust_eb(Org, Ws, Id, Depth + 1)
    end;
exhaust_eb(_Org, _Ws, _Id, _Depth) ->
    erlang:error(w601_delete_job_not_exhausted).

assert_job_blocked(T, <<"ordinary">>) ->
    {ok, [
        #{
            <<"job_state">> := <<"blocked">>,
            <<"attempts">> := 8,
            <<"last_error_code">> := <<"max_attempts">>
        }
    ]} =
        elib_pg:query(
            <<
                "SELECT job_state, attempts, last_error_code FROM attachment_object_deletion"
                " WHERE path=$1"
            >>,
            [key(T)]
        ),
    ok;
assert_job_blocked(T, _) ->
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, [
            #{
                <<"job_state">> := <<"blocked">>,
                <<"attempts">> := 8,
                <<"last_error_code">> := <<"max_attempts">>
            }
        ]} =
            elib_pg:query(
                <<
                    "SELECT job_state, attempts, last_error_code"
                    " FROM enterprise_asset_delete_queue"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
                >>,
                [Org, Ws, Id]
            )
    end).

%% 真实 fn_v5_register_pending 契约探针：deadline < expiry => 23514。
bad_window_cid() ->
    <<"0f0e0d0c-a601-4a5e-9b2c-00000000e001">>.

bad_window_sql() ->
    <<
        "SELECT fn_v5_register_pending("
        "'w601-failclosed/bad-window.bin', 'b1', 'private', 1, "
        "'0f0e0d0c-a601-4a5e-9b2c-00000000e001'::uuid, "
        "'{\"subject\":{\"kind\":\"user\",\"principal\":\"1\"},"
        "\"declaration\":{\"mime\":\"text/plain\",\"scope\":\"private\"}}'::jsonb, "
        "now() - interval '1 hour', now(), NULL, NULL, NULL, NULL)"
    >>.
