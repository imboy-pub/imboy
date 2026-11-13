-module(imboy_real_lifecycle_tests).

%%%===================================================================
%%% @doc T02：真实启停 + 资源回收 + 已确认消息重启恢复（计划 §6 验收矩阵
%%% 「SIGTERM/drain 与发送并发」行）。
%%%
%%% 全链路真实：peer 起独立 BEAM，application:ensure_all_started(imboy)
%%% 在空库上真实跑全部 up 迁移建表（不 meck 冒充任何层），经
%%% msg_c2c_logic:c2c 注入消息并等待真实 SERVER_ACK（服务端持久化确认），
%%% 随后从 OS 层发真实 SIGTERM 停止节点，验证：
%%%
%%%   1. 非法 drain 配置（负数）启动即拒（不进入运行态）；
%%%   2. 节点退出后 OS 进程/监听端口无残留，独占 PG 不受影响；
%%%   3. 新 VM 重启后：已 SERVER_ACK 且接收方已 CLIENT_ACK 的消息不再
%%%      出现在接收方离线视图（不重复消费）；已 SERVER_ACK 未 ACK 的
%%%      消息离线行仍在（不丢失、恢复责任保留）；staging 无残留。
%%%
%%% PG 坐标经 T02_REAL_PG_* 环境变量注入（同 healthz 真实池用例约定）；
%%% 缺变量时 skip——本用例只允许连接独占测试库。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% T02：peer 跨 VM 远程入口必须显式导出。eunit_autoexport 只导出编译器
%% 视角"被引用"的函数——phase1/phase3 仅经 peer:call 远程调用，本地无引用，
%% 自动导出缺席 → peer 内调用直接 undef（实测复现）。显式 export 是合同。
-export([phase1_boot_and_inject/1, phase3_restart_and_verify/3]).

%% 端口经 lane env 注入（lane-setup 分配），缺省回落 T02 lane 分配值；
%% peer fork 继承父 VM 环境，两侧取值一致。
-define(HTTP_PORT, list_to_integer(os:getenv("EUNIT_HTTP_PORT", "19981"))).
-define(ADM_PORT, list_to_integer(os:getenv("EUNIT_HTTP_ADM_PORT", "19971"))).
%% drain 预算（阶段 1 正常启动与阶段 3 重启使用的 ms 值）
-define(DRAIN_TIMEOUT_MS, 2000).
%% SIGTERM 后的总退出上界：drain 预算 + 监督树逆序关闭/worker terminate 余量。
-define(EXIT_BUDGET_MS, 25000).

imboy_real_lifecycle_test_() ->
    case pg_opts_from_env() of
        {ok, PgOpts} ->
            {timeout, 900, fun() -> run_real_lifecycle(PgOpts) end};
        {skip, _Reason} ->
            %% eunit 没有 {skip, Reason} 顶层描述符（bad descriptor → cancel 整轮），
            %% 测试体 throw 也进不了 skip 通道（run_testfun 把 throw 收敛为
            %% error；tty 仅支持 module_not_found/no_such_function 两种原因）。
            %% 项目惯例：生成器返回空清单静默让位（enterprise_oa_sso_tests 同款）。
            %% 需要真实跑时注入 T02_REAL_PG_{HOST,PORT,USER,DB} 四件套。
            []
    end.

pg_opts_from_env() ->
    case
        {
            os:getenv("T02_REAL_PG_HOST"),
            os:getenv("T02_REAL_PG_PORT"),
            os:getenv("T02_REAL_PG_USER"),
            os:getenv("T02_REAL_PG_DB")
        }
    of
        {Host, Port, User, Db} when is_list(Host), is_list(Port), is_list(User), is_list(Db) ->
            ConnOpts = #{
                host => Host,
                port => list_to_integer(Port),
                username => User,
                database => Db,
                ssl => false,
                timeout => 4000,
                codecs => [{epgsql_codec_rfc3339_bin, []}]
            },
            {ok, #{conn => ConnOpts, db => Db}};
        _ ->
            %% eunit 的 {skip, Reason} 要求 Reason 为 string（tuple/裸原子
            %% 之外的复合形态会被判 bad descriptor）；与 healthz 真实池用例同约定。
            {skip, "no_real_pg_env: T02_REAL_PG_{HOST,PORT,USER,DB} not set"}
    end.

run_real_lifecycle(#{conn := ConnOpts} = Pg) ->
    %% ---------- 阶段 1：peer A 启动（真实迁移建表）+ 拒启校验 + 注入 ----------
    {ok, PeerA, _NodeA, AliasA} = start_lifecycle_peer(),
    try
        {BeamPid, MsgA, MsgB} = peer:call(PeerA, ?MODULE, phase1_boot_and_inject, [Pg], 700000),
        true = is_list(BeamPid),

        %% ---------- 阶段 2：真实 SIGTERM + 资源回收 ----------
        sigterm_and_verify_reclaimed(BeamPid, PeerA, ConnOpts),

        %% ---------- 阶段 3：新 VM 重启 + 恢复验证 ----------
        {ok, PeerB, _NodeB, AliasB} = start_lifecycle_peer(),
        try
            ok = peer:call(PeerB, ?MODULE, phase3_restart_and_verify, [Pg, MsgA, MsgB], 300000)
        after
            stop_lifecycle_peer(PeerB, AliasB)
        end,
        io:format(user, "[t02-lifecycle] peer B stopped, alias cleaned~n", [])
    catch
        Class:Reason:Stack ->
            erlang:raise(Class, Reason, Stack)
    after
        stop_lifecycle_peer(PeerA, AliasA)
    end.

%%--------------------------------------------------------------------
%% peer 启动（继承 code path，不带 -config：全部用 set_env 注入，
%% 与定向 eunit 的外层 VM 保持同源）
%%--------------------------------------------------------------------
%% OTP 29 实测：code:priv_dir/1 要求 ebin 的**父目录名与 app 名匹配**
%% （prod release 的 lib/imboy-<vsn>/ebin、eunit-local 的 alias/imboy/ebin
%% 均满足；真实 build/ebin 恒 {error, bad_name}，父 VM 同样命中）。
%% 而 imboy start/2 在 app started 前消费 priv_dir（product_terminology、
%% migration 兜底），peer 直接用 build 路径必炸。
%% 复刻 eunit-local-legacy 的 alias_root 机制：临时目录软链 <build> 为
%% <tmp>/imboy，其 ebin 置于 path 最前。返回值带上临时目录供 stop 后清理。
start_lifecycle_peer() ->
    Paths = [filename:absname(P) || P <- code:get_path()],
    BuildRoot = filename:dirname(filename:dirname(code:which(imboy_app))),
    AliasRoot = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "t02-peer-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(AliasRoot, "keep")),
    LinkPath = filename:join(AliasRoot, "imboy"),
    case file:make_symlink(BuildRoot, LinkPath) of
        ok ->
            ok;
        {error, eexist} ->
            %% 前一轮 after 清理未执行（测试进程异常终止）留下的同名链接：
            %% 链接内容随建随弃，删除重建即本轮语义。
            ok = file:delete(LinkPath),
            ok = file:make_symlink(BuildRoot, LinkPath)
    end,
    AliasEbin = filename:join([AliasRoot, "imboy", "ebin"]),
    PaArgs = lists:append([["-pa", P] || P <- [AliasEbin | Paths]]),
    {ok, Peer, Node} = peer:start_link(#{
        connection => standard_io,
        args => ["-no_dot_erlang" | PaArgs],
        wait_boot => 15000
    }),
    {ok, Peer, Node, AliasRoot}.

%% peer 收尾：stop + 删除 alias 临时目录（先删内部 symlink 再删目录）。
stop_lifecycle_peer(Peer, AliasRoot) ->
    catch peer:stop(Peer),
    catch file:delete(filename:join(AliasRoot, "imboy")),
    catch file:del_dir(AliasRoot),
    ok.

%%--------------------------------------------------------------------
%% 阶段 1（在 peer A 内执行）
%%--------------------------------------------------------------------
phase1_boot_and_inject(Pg) ->
    %% eunit 对 peer 内失败只回显 2 帧堆栈；undef 等错误不带上 MFA。
    %% 本地展开完整堆栈落 user 输出再原样上抛，保证失败可诊断。
    try
        phase1_boot_and_inject_impl(Pg)
    catch
        Class:Reason:Stack ->
            io:format(
                user,
                "[t02-lifecycle] phase1 crashed ~p:~0p at ~p~n",
                [Class, Reason, Stack]
            ),
            erlang:raise(Class, Reason, Stack)
    end.

phase1_boot_and_inject_impl(#{conn := ConnOpts}) ->
    %% drain 配置非法 → 启动即拒（validate_drain_config 在监听器/迁移之前，
    %% 拒启无副作用）。ensure_all_started 对 start/2 崩溃返回 {error, _}。
    ok = load_and_configure(ConnOpts, -1),
    {error, StartFail} = application:ensure_all_started(imboy),
    false = lists:keymember(imboy, 1, application:which_applications()),
    io:format(user, "[t02-lifecycle] invalid drain config rejected: ~0p~n", [StartFail]),

    %% 修正配置后正常启动：空库上真实执行全部 up 迁移。
    ok = application:set_env(imboy, drain_timeout_ms, ?DRAIN_TIMEOUT_MS),
    {ok, _Apps} = application:ensure_all_started(imboy),
    ok = await_ready(erlang:monotonic_time(millisecond) + 120000),
    ready = imboy_app:readiness(),
    io:format(user, "[t02-lifecycle] app ready (real migrations applied)~n", []),

    %% 建两个用户 + 双向好友（c2c 发送前置）。
    {FromUid, ToUid} = seed_users(),
    %% T02 诊断：seed 全走 elib_pg 池；c2c 注入前显式核对池仍在（定位
    %% 池进程消失窗口）。
    io:format(
        user,
        "[t02-lifecycle] after seed: whereis(pgsql)=~p pooler_app=~p~n",
        [whereis(pgsql), lists:keymember(pooler, 1, application:which_applications())]
    ),

    %% 注入 A（将被接收方确认）与 B（不确认），等待真实 SERVER_ACK。
    %% msg_id 每轮唯一（时间戳后缀）：重复注入固定 id 会命中 staging
    %% duplicate（只补 ACK 不重投，正确语义）导致 await 空等 —— 本测试
    %% 的前提是每轮产生全新消息行。
    RoundTag = integer_to_binary(elib_dt:millisecond()),
    MsgA = <<"t02real-confirmed-", (integer_to_binary(FromUid))/binary, $-, RoundTag/binary>>,
    MsgB = <<"t02real-pending-", (integer_to_binary(FromUid))/binary, $-, RoundTag/binary>>,
    NowMs = elib_dt:millisecond(),
    send_and_await_server_ack(MsgA, FromUid, ToUid, NowMs),
    send_and_await_server_ack(MsgB, FromUid, ToUid, NowMs + 1),
    io:format(user, "[t02-lifecycle] both messages SERVER_ACKed~n", []),

    %% 等 worker 把 staging 转正到 msg_c2c（真实异步管线）。
    ok = await_rows_in_msg_c2c([MsgA, MsgB], ToUid, 30000),

    %% 等 staging 行全部 processed（直写成功即 mark_processed；轮询 30s）。
    %% T02 实测：client_ack（legacy 删行）与 pending staging 的异步管线存在
    %% 竞态 —— ACK 删行后 staging 若仍 pending，转正/重放会把已确认消息
    %% 重新写入 msg_c2c（复活，候选产品缺陷，归属消息管线 lane）。真实
    %% 客户端的 ACK 发生在收到消息之后（管线稳定窗口），此处等待与该
    %% 时序一致，使阶段 3 的"重启不复活"断言测的是启停语义而非竞态。
    ok = await_staging_processed([MsgA, MsgB], 30000),

    %% A 做接收方确认（legacy 无设备路径：删除离线行 = 已交付）。
    ok = msg_ack_logic:client_ack(<<"c2c">>, MsgA, ToUid, <<>>),
    0 = count_msg_c2c(MsgA, ToUid),
    %% T02 取证：client_ack 与后台异步管线（async_retry 重试 3 次、worker
    %% drain 批次）的竞态窗口。静默 2.5s 后复数：若仍 0 = 阶段 1 内管线
    %% 已静默；若 >0 = ACK 后即被重插（复活发生在 SIGTERM 之前）。
    timer:sleep(2500),
    CountAfterQuiet = count_msg_c2c(MsgA, ToUid),
    io:format(
        user,
        "[t02-lifecycle] quiet-window count(MsgA)=~p (0 = no resurrection in phase1)~n",
        [CountAfterQuiet]
    ),
    1 = count_msg_c2c(MsgB, ToUid),
    io:format(user, "[t02-lifecycle] A acked (offline row cleared), B pending~n", []),

    %% 返回 {OS pid, MsgA, MsgB}：pid 供阶段 2 SIGTERM；消息 id 经**父 VM
    %% 内存**传给阶段 3 的全新 VM（跨被测节点内存，合法通道），供重启后
    %% 精确核对这两行的恢复结果。
    {os:getpid(), MsgA, MsgB}.

load_and_configure(ConnOpts, DrainTimeoutMs) ->
    ok = application:load(imboy),
    PgConf = #{
        name => pgsql,
        init_count => 2,
        max_count => 10,
        start_mfa => {epgsql, connect, [ConnOpts]}
    },
    ok = application:set_env(imboy, pg_conf, PgConf),
    Super = maps:without([codecs, timeout], ConnOpts#{timeout => 8000}),
    ok = application:set_env(imboy, super_account, Super),
    ok = application:set_env(imboy, http_port, ?HTTP_PORT),
    ok = application:set_env(imboy, http_port_adm, ?ADM_PORT),
    ok = application:set_env(imboy, auto_migrate, true),
    ok = application:set_env(imboy, start_mode, http),
    ok = application:set_env(imboy, cluster_nodes, []),
    ok = application:set_env(imboy, dsync_enabled, false),
    %% 事务路径（elib_pg:with_tx 族）的池名取 config_ds:env(sql_driver)，
    %% 无代码级默认 —— 生产/测试 VM 由 sys.config 或 eunit_runner 显式注入；
    %% peer 缺该键时 take_member(undefined) → noproc（实测 c2c staging 崩点）。
    ok = application:set_env(imboy, sql_driver, pgsql),
    %% scripts_path 不得经 code:priv_dir(imboy) 推导：OTP 29 中 app 仅
    %% load（未 started）时 priv_dir 返回 {error, bad_name}（peer 内实测），
    %% 而 scripts_path 必须在 ensure_all_started 之前就位（start/2 内
    %% maybe_migrate 消费）。imboy_app.beam 位于 <build>/ebin，priv 即其
    %% 两步父目录下的 priv/ —— 与 ebin 同源，等价于 priv_dir 语义。
    PrivRoot = filename:dirname(filename:dirname(code:which(imboy_app))),
    ok = application:set_env(
        imboy, scripts_path, filename:join(PrivRoot, filename:join("priv", "migrations"))
    ),
    ok = application:set_env(imboy, drain_timeout_ms, DrainTimeoutMs),
    ok.

await_ready(Deadline) ->
    case imboy_app:readiness() of
        ready ->
            ok;
        _ ->
            %% erlang:monotonic_time/1 非 guard BIF，deadline 比较放 case 表达式
            case deadline_left(Deadline) of
                true ->
                    timer:sleep(200),
                    await_ready(Deadline);
                false ->
                    error(real_app_not_ready)
            end
    end.

deadline_left(Deadline) ->
    Deadline - erlang:monotonic_time(millisecond) > 0.

seed_users() ->
    FromUid = 88110001,
    ToUid = 88110002,
    ok = ensure_user(FromUid, <<"t02realfrom">>),
    ok = ensure_user(ToUid, <<"t02realto">>),
    ok = ensure_friendship(FromUid, ToUid),
    {FromUid, ToUid}.

ensure_user(Uid, Account) ->
    case user_repo:may_exist(Uid) of
        true ->
            ok;
        _ ->
            UserData = #{
                <<"id">> => Uid,
                <<"account">> => Account,
                <<"nickname">> => Account,
                <<"password">> => elib_password:generate(<<"t02-real-lifecycle">>),
                <<"mobile">> => Account,
                <<"email">> => <<>>,
                <<"region">> => <<>>,
                <<"avatar">> => <<>>,
                <<"sign">> => <<>>,
                <<"gender">> => 0,
                <<"status">> => 1,
                <<"created_at">> => elib_dt:now(),
                <<"reg_ip">> => <<"127.0.0.1">>,
                <<"reg_cosv">> => <<"t02_lifecycle_test">>,
                <<"level_id">> => 1,
                <<"experience">> => 0,
                <<"ref_user_id">> => 0,
                <<"ref_parent_user_id">> => 0,
                <<"source">> => <<"t02">>
            },
            {ok, Uid} = user_repo:save(UserData),
            ok
    end.

ensure_friendship(FromUid, ToUid) ->
    FriendTb = elib_pg_sql:public_tablename(<<"user_friend">>),
    Rows = [
        {friend_row_id(FromUid, ToUid), FromUid, ToUid},
        {friend_row_id(ToUid, FromUid), ToUid, FromUid}
    ],
    lists:foreach(
        fun({Id, F, T}) ->
            {ok, _} = elib_pg:execute(
                <<"INSERT INTO ", FriendTb/binary,
                    " (id, from_user_id, to_user_id, status) "
                    "VALUES ($1, $2, $3, 1) "
                    "ON CONFLICT (from_user_id, to_user_id) DO NOTHING">>,
                [Id, F, T]
            )
        end,
        Rows
    ),
    ok.

%% 确定性行 id：user_friend.id 为 bigint 主键且无默认值，必须显式提供。
%% 由 uid 对派生（A*100000 + B 末 5 位），固定 uid 对重跑 id 不变，
%% 与 ON CONFLICT (from_user_id, to_user_id) 一起保证幂等。
friend_row_id(A, B) ->
    A * 100000 + (B rem 100000) + 7.

%% 注入消息并等待真实 SERVER_ACK（c2c 持久化 staging 成功后把
%% C2C_SERVER_ACK 发回调用进程信箱）。
send_and_await_server_ack(MsgId, FromUid, ToUid, CreatedAtMs) ->
    Data = #{
        <<"to">> => integer_to_binary(ToUid),
        <<"payload">> => #{<<"text">> => <<"t02 real lifecycle">>},
        <<"created_at">> => CreatedAtMs,
        <<"msg_type">> => <<"text">>
    },
    ok = msg_c2c_logic:c2c(MsgId, FromUid, Data),
    receive
        {reply, #{<<"type">> := <<"C2C_SERVER_ACK">>, <<"id">> := MsgId}} = Ack ->
            io:format(user, "[t02-lifecycle] SERVER_ACK ~0p~n", [Ack]),
            ok
    after 10000 ->
        error({server_ack_timeout, MsgId})
    end.

await_rows_in_msg_c2c(MsgIds, ToUid, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    await_rows(MsgIds, ToUid, Deadline).

await_rows(MsgIds, ToUid, Deadline) ->
    Found = [M || M <- MsgIds, count_msg_c2c(M, ToUid) > 0],
    case length(Found) =:= length(MsgIds) of
        true ->
            ok;
        false ->
            case deadline_left(Deadline) of
                true ->
                    timer:sleep(200),
                    await_rows(MsgIds, ToUid, Deadline);
                false ->
                    error({msg_c2c_rows_missing, MsgIds -- Found})
            end
    end.

count_msg_c2c(MsgId, ToUid) ->
    Tb = elib_pg_sql:public_tablename(<<"msg_c2c">>),
    {ok, [#{<<"count">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::int AS count FROM ", Tb/binary, " WHERE msg_id = $1 AND to_id = $2">>,
        [MsgId, ToUid]
    ),
    N.

%%--------------------------------------------------------------------
%% 阶段 2（父 eunit VM）：SIGTERM + 资源回收
%%--------------------------------------------------------------------
sigterm_and_verify_reclaimed(BeamPid, PeerA, ConnOpts) ->
    io:format(user, "[t02-lifecycle] SIGTERM -> beam ~s~n", [BeamPid]),
    T0 = erlang:monotonic_time(millisecond),
    %% OTP 29 已移除 os:system/1（undef，实测）；os:cmd 返回 stdout（kill 无输出），
    %% kill 失败由 await_process_gone 的超时兜底暴露。
    _ = os:cmd("kill -TERM " ++ BeamPid),
    ok = await_process_gone(BeamPid, 60000),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    %% drain 预算内退出：SIGTERM 经 erl_signal_server 走 init:stop() 优雅停机
    %% （suspend listener → drain → stop_listener → 监督树逆序关闭）。本用例
    %% SIGTERM 时无在途连接，drain 应立即返回，总耗时 = drain 预算 + sup/worker
    %% 关闭余量；超 ?EXIT_BUDGET_MS 说明停机路径卡死。
    ?assert(Elapsed < ?EXIT_BUDGET_MS),
    io:format(
        user,
        "[t02-lifecycle] beam exited in ~pms (drain budget ~pms, exit budget ~pms)~n",
        [Elapsed, ?DRAIN_TIMEOUT_MS, ?EXIT_BUDGET_MS]
    ),
    %% peer 的 controlling 进程收尾（节点可能已死）
    catch peer:stop(PeerA),

    %% 端口无监听残留
    [] = listeners_on_port(?HTTP_PORT),
    io:format(user, "[t02-lifecycle] beam gone, port ~p released~n", [?HTTP_PORT]),

    %% 独占 PG 不受影响（探针独立路径之外再走一次真实查询）
    Conn = connect_for_check(ConnOpts),
    {ok, _, [{1}]} = epgsql:equery(Conn, <<"SELECT 1">>, []),
    epgsql:close(Conn),
    io:format(user, "[t02-lifecycle] scratch PG still alive~n", []),
    ok.

await_process_gone(Pid, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    await_gone(Pid, Deadline).

await_gone(Pid, Deadline) ->
    %% OTP 29 无 os:system/1：经 os:cmd 的 shell echo $? 取 ps 退出码，
    %% "0" = 进程仍在（SIGTERM 优雅停机未完成），其余 = 已退出。
    Output = os:cmd("ps -p " ++ Pid ++ " > /dev/null 2>&1; echo $?"),
    case string:trim(Output) of
        "0" ->
            case deadline_left(Deadline) of
                true ->
                    timer:sleep(200),
                    await_gone(Pid, Deadline);
                false ->
                    error({beam_did_not_exit, Pid})
            end;
        _ ->
            ok
    end.

listeners_on_port(Port) ->
    Cmd = "lsof -nP -iTCP:" ++ integer_to_list(Port) ++ " -sTCP:LISTEN 2>/dev/null || true",
    Output = os:cmd(Cmd),
    [L || L <- string:tokens(Output, "\n"), string:len(string:trim(L)) > 0].

connect_for_check(ConnOpts) ->
    %% epgsql:connect/1 map 形式：host/port/username/database/ssl/timeout/codecs
    %% 全部是合法 connect_option 键（connect/3 元数实为 (Host, Username, Opts)）。
    {ok, C} = epgsql:connect(ConnOpts),
    C.

%%--------------------------------------------------------------------
%% 阶段 3（在 peer B 内执行）：重启 + 恢复验证
%%--------------------------------------------------------------------
phase3_restart_and_verify(#{conn := ConnOpts}, MsgA, MsgB) ->
    ok = load_and_configure(ConnOpts, ?DRAIN_TIMEOUT_MS),
    {ok, _} = application:ensure_all_started(imboy),
    ok = await_ready(erlang:monotonic_time(millisecond) + 120000),
    ready = imboy_app:readiness(),
    io:format(user, "[t02-lifecycle] restarted VM ready (migrations idempotent)~n", []),

    %% 稳定窗口：worker / 定时任务全速跑一小段时间，任何"重启重放"类
    %% 缺陷都会在这个窗口把已确认消息重新写回或重复投递。
    timer:sleep(2000),

    ToUid = 88110002,

    %% 不重复消费：A（已确认）不在接收方离线视图
    0 = count_msg_c2c(MsgA, ToUid),
    %% 不丢失：B（已 SERVER_ACK 未确认）的离线行仍在且唯一
    1 = count_msg_c2c(MsgB, ToUid),
    %% staging 无 **pending** 残留：processed 行是持久化底账（周期清理负责，
    %% 实测保留），pending 行才会被 worker 转正令已确认消息复活 —— 检查
    %% 口径为"无未处理残留"而非"行数为零"。
    0 = count_staging_pending(MsgA),
    0 = count_staging_pending(MsgB),
    io:format(user, "[t02-lifecycle] recovery verified: no dup, no loss~n", []),
    ok.

count_staging_pending(MsgId) ->
    Tb = elib_pg_sql:public_tablename(<<"msg_store_staging">>),
    {ok, [#{<<"count">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::int AS count FROM ", Tb/binary,
            " WHERE msg_id = $1 AND processed_at IS NULL">>,
        [MsgId]
    ),
    N.

%% 等待 staging 行全部离开 pending 态（processed_at 非空）：直写成功路径
%% 会立即 mark_processed，worker 转正路径在 drain 批次内打标。超时即
%% 管线未收敛（故障，不掩盖）。
await_staging_processed(MsgIds, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    await_staging_done(MsgIds, Deadline).

await_staging_done(MsgIds, Deadline) ->
    Pending =
        [M || M <- MsgIds, not staging_processed(M)],
    case Pending of
        [] ->
            ok;
        _ ->
            case deadline_left(Deadline) of
                true ->
                    timer:sleep(200),
                    await_staging_done(MsgIds, Deadline);
                false ->
                    error({staging_not_processed, Pending})
            end
    end.

staging_processed(MsgId) ->
    Tb = elib_pg_sql:public_tablename(<<"msg_store_staging">>),
    {ok, [#{<<"count">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::int AS count FROM ", Tb/binary,
            " WHERE msg_id = $1 AND processed_at IS NOT NULL">>,
        [MsgId]
    ),
    N > 0.
