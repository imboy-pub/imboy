#!/usr/bin/env escript
%%! -noshell
%% ============================================================
%% H3 演练库迁移工具 / Drill-DB migration driver (up/down/version)
%% ------------------------------------------------------------
%% 配合 scripts/sanitized_snapshot.sh 使用（手册 §五：快照恢复到演练库
%% → 迁移 → 冒烟 → 回滚 → 复核）。对演练库执行 erlang_migrate 的
%% up/down（strict 模式，与 imboy_migrate:migrate/0 同口径）。
%%
%% 用法:
%%   PGHOST=... PGPORT=... PGUSER=... PGPASSWORD=... PGDATABASE=imboy_drill \
%%     scripts/drill_migrate.escript version|up|down
%%   down = 回滚一步（如 00000081 → 80）；up = 应用全部未应用迁移。
%% 依赖 imboy 仓 deps/{epgsql,erlang_migrate}/ebin（erlang.mk 原位编译，先 make compile）。
%% ============================================================
main([Action0]) ->
    %% 在 imboy 仓根目录运行（或用 IMBOY_DIR 指定仓根）
    RepoDir = env("IMBOY_DIR", "."),
    MigDir = filename:join(RepoDir, "priv/migrations"),
    code:add_pathsa([
        filename:join([RepoDir, "deps/epgsql/ebin"]),
        filename:join([RepoDir, "deps/erlang_migrate/ebin"])
    ]),
    Host = env("PGHOST", "127.0.0.1"),
    Port = list_to_integer(env("PGPORT", "4323")),
    User = env("PGUSER", "imboy_user"),
    Pass = env("PGPASSWORD", ""),
    Db = env("PGDATABASE", "imboy_drill"),
    Action = case Action0 of "version" -> version; "up" -> up; "down" -> down end,
    case epgsql:connect(#{host => Host, port => Port, username => User,
                          password => Pass, database => Db}) of
        {ok, Conn} ->
            Conf = #{conn => Conn, dir => MigDir, strict => true},
            R = case Action of
                version -> erlang_migrate:version(Conf);
                up      -> erlang_migrate:up(Conf);
                down    -> erlang_migrate:down(Conf, 1)
            end,
            io:format("~p~n", [R]),
            epgsql:close(Conn),
            case R of
                {error, _} -> halt(1);
                _ -> ok
            end;
        {error, Reason} ->
            io:format(standard_error, "connect failed: ~p~n", [Reason]),
            halt(1)
    end;
main(_) ->
    io:format(standard_error,
              "usage: drill_migrate.escript version|up|down  (PG* 环境变量定连接)~n", []),
    halt(2).

env(K, D) -> case os:getenv(K) of false -> D; V -> V end.
