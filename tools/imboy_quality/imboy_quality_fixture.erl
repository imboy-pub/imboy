%% IMBOY_ERLANG_QUALITY P1-07：原生 PG fixture 隔离（纯 OTP，epgsql 直连）。
%% 任务私有模板/克隆库。归属 marker 用库内表 imboy_quality_meta.imboy_quality_ownership
%% （专用 schema：产品的 live-schema 覆盖测试枚举 public 全表，marker 不得污染 public——
%% opt-run6 data_disposition coverage_failed 教训）
%% （PG18 的 database COMMENT 不入 pg_description，表 marker 且随 TEMPLATE
%% 克隆天然复制）。DB01 克隆等价、DB03 活动连接、DB06 未拥有库拒删、
%% DB07 环境缺失 fail-closed、DB02 模板 digest 键。
-module(imboy_quality_fixture).
-export([
    connect/1,
    connect_db/3,
    ensure_template/2,
    clone/3,
    drop_owned/2,
    active_connections/2,
    marker_ok/2,
    with_template_pool/4,
    preflight_extensions/3,
    pg_args_from_config/1,
    ensure_marked_database/3,
    system_identifier/1,
    clone_live/4,
    drop_owned/3,
    comment_database/3
]).

-define(MARKER_SCHEMA, "imboy_quality_meta").
-define(MARKER_TABLE, "imboy_quality_ownership").
-define(MARKER_FQN, ?MARKER_SCHEMA ++ "." ++ ?MARKER_TABLE).
-define(OWNED_PREFIX, "imboy_quality_").

%% ---- 实时库克隆（组间 DB 隔离：每 worker 私有克隆库；P1-07 池式使用的正式接线） ----
%% 两个 worker 共用 imboy_test_v1 会产生交叉干扰（opt-run5：throttle limiter 迁移态、
%% 资产 webhook 等 S 内新失败）。PG TEMPLATE 语义要求源库此刻无其他会话；克隆后
%% 写归属 marker 表，供 drop_owned/3 回收（仅 OWNED_PREFIX 前缀）。
clone_live(Conn, Args, Source, Target) ->
    true = is_owned_name(Target),
    q(Conn, ["CREATE DATABASE \"", Target, "\" TEMPLATE \"", Source, "\""]),
    connect_db(Args, Target, fun(C) -> ensure_marker(C) end),
    ok.

%% 归属 COMMENT（独占库合同如 group_schedule_remind 校验
%% shobj_description LIKE 'imboy-delivery-v3.2 merge schedule%exclusive%'）。
comment_database(Conn, Name, Comment) ->
    true = is_owned_name(Name),
    q(Conn, ["COMMENT ON DATABASE \"", Name, "\" IS '", Comment, "'"]),
    ok.

ensure_marker(C) ->
    q(C, ["CREATE SCHEMA IF NOT EXISTS ", ?MARKER_SCHEMA]),
    q(C, [
        "CREATE TABLE IF NOT EXISTS ",
        ?MARKER_FQN,
        " (owner text NOT NULL, created_at timestamptz DEFAULT now())"
    ]),
    q(C, ["INSERT INTO ", ?MARKER_FQN, " (owner) VALUES ('IMBOY_ERLANG_QUALITY')"]),
    ok.

%% 显式连接参数版本（不依赖 conn_args_of/1 的保守默认）。worker VM 刚退出时
%% 其 PG 后端可能尚未断开：有界等待（最多 ~10s）后再 DROP；超时即报错，不强杀。
drop_owned(Conn, Args, Db) ->
    case is_owned_name(Db) andalso marker_ok(Args, Db) of
        true ->
            %% 归属已证明（前缀 + marker）：先有界等待 worker/独占/独立 VM 的后端自然
            %% 断开（opt-run6：独占车道最后一个 VM 退出后后端滞留 >10s），超时则只
            %% 终止本私有库自身的会话（不触碰任何共享库），再 DROP。
            case wait_no_connections(Conn, Db, 120) of
                ok ->
                    ok;
                {still_in_use, _} ->
                    q(Conn, [
                        "SELECT pg_terminate_backend(pid) FROM pg_stat_activity ",
                        "WHERE datname = '",
                        Db,
                        "' AND pid <> pg_backend_pid()"
                    ]),
                    ok =
                        case wait_no_connections(Conn, Db, 20) of
                            ok -> ok;
                            {still_in_use, N} -> error({database_still_in_use, Db, N})
                        end
            end,
            q(Conn, ["ALTER DATABASE \"", Db, "\" IS_TEMPLATE false"]),
            q(Conn, ["DROP DATABASE \"", Db, "\""]),
            ok;
        false ->
            error({refusing_unowned_database, Db})
    end.

wait_no_connections(Conn, Db, Left) ->
    case active_connections(Conn, Db) of
        [] ->
            ok;
        Pids when Left =< 0 ->
            {still_in_use, length(Pids)};
        _ ->
            timer:sleep(500),
            wait_no_connections(Conn, Db, Left - 1)
    end.

%% ---- 连接参数来源：项目 sys config 的 imboy.pg_conf.start_mfa ----
%% 与 eunit 轨道同一真实 PG（host/port/username/password），不另造凭据。
pg_args_from_config(ConfigPath) ->
    {ok, [Terms]} = file:consult(ConfigPath),
    {imboy, Env} = lists:keyfind(imboy, 1, Terms),
    #{start_mfa := {epgsql, connect, [Args]}} = proplists:get_value(pg_conf, Env),
    maps:with([host, port, username, password, database], Args).

%% ---- 带 database COMMENT 归属标记的任务私有库（幂等） ----
%% legacy talk_record_owned_eunit_fixture 的 verify_server 以
%% shobj_description(pg_database)（pg_shdescription，COMMENT ON DATABASE 可写）
%% 读 marker；本模块的库内表 marker 面向模板克隆，两者并存不冲突。
%% 仅允许 OWNED_PREFIX 前缀的库名（与 drop_owned 同一归属纪律）。
ensure_marked_database(Conn, Name, Marker) ->
    true = lists:prefix(?OWNED_PREFIX, Name),
    case q(Conn, ["SELECT 1 FROM pg_database WHERE datname = '", Name, "'"]) of
        {ok, _, []} -> q(Conn, ["CREATE DATABASE \"", Name, "\""]);
        {ok, _, [_ | _]} -> ok
    end,
    q(Conn, ["COMMENT ON DATABASE \"", Name, "\" IS '", Marker, "'"]),
    {ok, _, [{Got}]} = q(Conn, [
        "SELECT shobj_description(d.oid, 'pg_database') FROM pg_database d ",
        "WHERE d.datname = '",
        Name,
        "'"
    ]),
    Got = list_to_binary(Marker),
    ok.

system_identifier(Conn) ->
    {ok, _, [{Id}]} = q(Conn, "SELECT system_identifier::text FROM pg_control_system()"),
    binary_to_list(Id).

%% DB07：扩展 preflight——必需扩展任一缺失即 {error, ...}，不返回空集假绿。
%% Required :: [binary()]，与产品合同一致（pg_jieba/postgis/timescaledb/pgcrypto）。
preflight_extensions(Args, Db, Required) ->
    connect_db(Args, Db, fun(C) ->
        {ok, _, Rows} = q(C, "SELECT extname FROM pg_extension"),
        Present = [E || {E} <- Rows],
        Missing = [R || R <- Required, not lists:member(R, Present)],
        case Missing of
            [] -> {ok, Present};
            _ -> {error, {missing_extensions, Missing, Present}}
        end
    end).

connect(Args) ->
    epgsql:connect(
        maps:get(host, Args, "127.0.0.1"),
        maps:get(username, Args, "imboy_user"),
        maps:get(password, Args, ""),
        #{
            port => maps:get(port, Args, 4323),
            database => maps:get(database, Args, "postgres"),
            ssl => false,
            timeout => 5000
        }
    ).

%% 连接到指定库执行 Fun 后关闭（跨库元数据需要按库连接）。
connect_db(Args, Db, Fun) ->
    {ok, C} = connect(Args#{database := Db}),
    try
        Fun(C)
    after
        try
            epgsql:close(C)
        catch
            _:_ -> ok
        end
    end.

q(C, Sql) ->
    SqlStr = iolist_to_binary(Sql),
    case epgsql:squery(C, SqlStr) of
        {ok, _} = Ok -> Ok;
        {ok, _, _} = Ok -> Ok;
        {ok, _, _, _} = Ok -> Ok;
        {error, E} -> error({sql_failed, SqlStr, E})
    end.

%% ---- 模板 ----

ensure_template(Conn, Name) ->
    true = is_owned_name(Name),
    ensure(Conn, Name),
    Args = conn_args_of(Conn),
    connect_db(Args, Name, fun(C) -> ensure_marker(C) end),
    q(Conn, ["ALTER DATABASE \"", Name, "\" IS_TEMPLATE true"]),
    #{name => Name, digest => template_digest()}.

%% 从现有连接恢复连接参数（epgsql 无自省；调用方通常已有 Args——
%% 这里保守重建：host/port 由调用方 ensure_template 的 Args 参数传入更佳）。
conn_args_of(_Conn) ->
    #{
        host => "127.0.0.1",
        port => 4323,
        username => "imboy_user",
        password => "abc54321",
        database => "postgres"
    }.

ensure(Conn, Name) ->
    case db_exists(Conn, Name) of
        true -> ok;
        false -> q(Conn, ["CREATE DATABASE \"", Name, "\""])
    end.

db_exists(Conn, Name) ->
    {ok, _, Rows} = q(
        Conn,
        "SELECT 1 FROM pg_database WHERE datname='" ++ Name ++ "'"
    ),
    Rows =/= [].

template_digest() ->
    crypto:hash(sha256, <<"imboy_quality_template_v1">>).

%% ---- 克隆（DB01/DB03） ----

clone(Conn, Template, Target) ->
    true = is_owned_name(Target),
    %% DB03：克隆前模板无活动连接
    [] = active_connections(Conn, Template),
    q(Conn, ["CREATE DATABASE \"", Target, "\" TEMPLATE \"", Template, "\""]),
    clone_equivalence(Conn, Template, Target).

clone_equivalence(Conn, A, B) ->
    Args = conn_args_of(Conn),
    Ca = connect_db(Args, A, fun table_set/1),
    Cb = connect_db(Args, B, fun table_set/1),
    case Ca =:= Cb of
        true -> {ok, Cb};
        false -> error({clone_not_equivalent, A, B, Ca, Cb})
    end.

table_set(C) ->
    {ok, _, Rows} = q(
        C,
        "SELECT tablename FROM pg_tables WHERE schemaname='public' ORDER BY 1"
    ),
    [R || {R} <- Rows].

active_connections(Conn, Db) ->
    {ok, _, Rows} = q(
        Conn,
        "SELECT pid FROM pg_stat_activity WHERE datname='" ++ Db ++
            "' AND pid <> pg_backend_pid()"
    ),
    Rows.

%% ---- 归属与删除（DB06：未拥有库拒删） ----

is_owned_name(N) when is_list(N); is_binary(N) ->
    S = unicode:characters_to_list(N),
    lists:prefix("imboy_quality_", S);
is_owned_name(_) ->
    false.

marker_ok(Args, Db) ->
    connect_db(Args, Db, fun(C) ->
        {ok, _, Rows} = q(C, [
            "SELECT owner FROM ",
            ?MARKER_FQN,
            " WHERE owner='IMBOY_ERLANG_QUALITY' LIMIT 1"
        ]),
        Rows =/= []
    end).

drop_owned(Conn, Db) ->
    case is_owned_name(Db) andalso marker_ok(conn_args_of(Conn), Db) of
        true ->
            [] = active_connections(Conn, Db),
            %% 模板库必须先解除 IS_TEMPLATE 才允许 DROP（PG 语义）
            q(Conn, ["ALTER DATABASE \"", Db, "\" IS_TEMPLATE false"]),
            q(Conn, ["DROP DATABASE \"", Db, "\""]),
            ok;
        false ->
            error({refusing_unowned_database, Db})
    end.

%% ---- 池式使用（组间隔离；每 worker 独立克隆库） ----

with_template_pool(Conn, Template, Args, Fun) ->
    Target =
        "imboy_quality_clone_" ++
            integer_to_list(erlang:unique_integer([positive]) band 16#FFFFFF),
    clone(Conn, Template, Target),
    try
        Fun(Args#{database := Target})
    after
        try
            drop_owned(Conn, Target)
        catch
            _:_ -> ok
        end
    end.
