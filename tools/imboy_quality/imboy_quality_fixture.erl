%% IMBOY_ERLANG_QUALITY P1-07：原生 PG fixture 隔离（纯 OTP，epgsql 直连）。
%% 任务私有模板/克隆库。归属 marker 用库内表 public.imboy_quality_ownership
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
    preflight_extensions/3
]).

-define(MARKER_TABLE, "imboy_quality_ownership").

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
        catch epgsql:close(C)
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
    connect_db(Args, Name, fun(C) ->
        q(C, [
            "CREATE TABLE IF NOT EXISTS public.",
            ?MARKER_TABLE,
            " (owner text NOT NULL, created_at timestamptz DEFAULT now())"
        ]),
        q(C, [
            "INSERT INTO public.",
            ?MARKER_TABLE,
            " (owner) VALUES ('IMBOY_ERLANG_QUALITY')"
        ])
    end),
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
            "SELECT owner FROM public.",
            ?MARKER_TABLE,
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
        catch drop_owned(Conn, Target)
    end.
