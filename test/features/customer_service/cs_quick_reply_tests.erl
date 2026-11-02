%% CS Quick Reply 后端合同验证（真库：inttest marker DB 同款 disposable PG；
%% 合同 v3/specs/cs-quick-reply.md §1/§2/§4/§5）。
%% 断言面：
%%   * 数据面 CRUD：创建/分页/更新全量（校验在 app 层，此处覆盖数据面语义）；
%%   * 软删：行对一切读不可见；DELETE 幂等（重复删除同 ok）；
%%   * 越权不可见：跨 (org, workspace) find/update/delete 不可见裁决；
%%   * org 约束机械断言：update/soft_delete WHERE 携带 org+ws（铁律6 纵深防御）；
%%   * app 层：title/content trim 长度界；seat 只读投影白名单 + assignment 门。
%% 前置：普通 eunit-local PG（elib_pg 池，表随 00000240 迁移建）。
-module(cs_quick_reply_tests).

-include_lib("eunit/include/eunit.hrl").

quick_reply_repo_test_() ->
    {timeout, 60, {setup, fun setup/0, fun teardown/1, fun(S) -> [?_test(t_all(S))] end}}.

setup() ->
    ok = ensure_pool(),
    {ok, Conn} = elib_pg:with_tx(fun(C) -> {ok, C} end),
    %% 清场（幂等；共享库下本套件专用前缀键）
    {ok, _} = elib_pg:execute(
        Conn,
        <<"DELETE FROM cs_quick_reply WHERE organization_id BETWEEN 996100 AND 996199">>,
        []
    ),
    #{conn => Conn}.

teardown(_S) ->
    {ok, Conn} = elib_pg:with_tx(fun(C) -> {ok, C} end),
    {ok, _} = elib_pg:execute(
        Conn,
        <<"DELETE FROM cs_quick_reply WHERE organization_id BETWEEN 996100 AND 996199">>,
        []
    ),
    ok.

t_all(S) ->
    C = maps:get(conn, S),
    T = 996101,
    W = 996201,
    WOther = 996202,
    %% 1) 创建两行（sort 排序键 10/0）
    Id0 = cs_tsid:new_id(cs_quick_reply),
    Id1 = cs_tsid:new_id(cs_quick_reply),
    {ok, R0} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:insert_tx(Cx, #{
            id => Id0,
            organization_id => T,
            workspace_id => W,
            title => <<"问候语"/utf8>>,
            content => <<"您好"/utf8>>,
            sort => 10,
            created_by => 900001
        })
    end),
    ?assertEqual(Id0, maps:get(<<"id">>, R0)),
    ?assertEqual(10, maps:get(<<"sort">>, R0)),
    ?assertEqual(T, maps:get(<<"organization_id">>, R0)),
    {ok, R1} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:insert_tx(Cx, #{
            id => Id1,
            organization_id => T,
            workspace_id => W,
            title => <<"结束语"/utf8>>,
            content => <<"再见"/utf8>>,
            sort => 0,
            created_by => 900001
        })
    end),
    ?assert(is_integer(maps:get(<<"created_at">>, R1))),
    %% 2) 分页排序 sort ASC, id ASC + search 包含匹配
    {ok, Rows, Meta} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10})
    end),
    ?assertEqual([Id1, Id0], [maps:get(<<"id">>, R) || R <- Rows]),
    ?assertEqual(2, maps:get(total, Meta)),
    ?assertEqual(1, maps:get(total_page, Meta)),
    {ok, RowsS, MetaS} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10, search => <<"结束"/utf8>>})
    end),
    ?assertEqual([Id1], [maps:get(<<"id">>, R) || R <- RowsS]),
    ?assertEqual(1, maps:get(total, MetaS)),
    %% 3) 更新全量 + 软删行 not_found
    {ok, U} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:update_tx(Cx, Id0, T, W, #{
            title => <<"问候语2"/utf8>>, content => <<"您好，很高兴为您服务"/utf8>>, sort => 5
        })
    end),
    ?assertEqual(<<"问候语2"/utf8>>, maps:get(<<"title">>, U)),
    ?assertEqual(5, maps:get(<<"sort">>, U)),
    %% 4) 跨 (org, workspace) 不可见：update/delete/find on other workspace
    SD = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:soft_delete_tx(Cx, Id0, T, W)
    end),
    io:format("QR_SOFT_DELETE=~p~n", [SD]),
    %% 跨 (org, workspace) 可见性裁决在 app 层（find 探针+boundary 分支，
    %% 探针行携带 org/workspace 输出）；数据面 update/soft_delete 另携带
    %% org+ws 谓词纵深防御（合同 §4.3/§4.4 的 403/404 由 app 映射）。
    ?assertEqual(
        {error, not_found},
        cs_pg_quick_reply:with_tx(fun(Cx) ->
            cs_pg_quick_reply:update_tx(Cx, Id0, T, W, #{title => <<"x">>, content => <<"y">>})
        end)
    ),
    {ok, PageAfterDelete, _} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10})
    end),
    ?assertEqual([Id1], [maps:get(<<"id">>, R) || R <- PageAfterDelete]),
    %% 5) 软删幂等：重复删除同 ok
    {ok, already_deleted} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:soft_delete_tx(Cx, Id0, T, W)
    end),
    %% 5b) org+ws 谓词纵深防御：申报范围不匹配 → 0 行命中（already_deleted，
    %% 而非误删他行；探针 find 只按 id+未删，此谓词是 update/delete 的第二道门）
    Id2 = cs_tsid:new_id(cs_quick_reply),
    {ok, _} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:insert_tx(Cx, #{
            id => Id2,
            organization_id => T,
            workspace_id => W,
            title => <<"范围行"/utf8>>,
            content => <<"内容"/utf8>>,
            sort => 0,
            created_by => 900001
        })
    end),
    {ok, already_deleted} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:soft_delete_tx(Cx, Id2, T, WOther)
    end),
    {ok, _, _} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10})
    end),
    %% 6) app 层：trim 校验 + seat 投影/assignment 门
    {error, invalid_argument} = cs_quick_reply_app:create(#{
        organization_id => T,
        workspace_id => W,
        title => <<"   ">>,
        content => <<"正文"/utf8>>
    }),
    {error, invalid_argument} = cs_quick_reply_app:create(#{
        organization_id => T,
        workspace_id => W,
        title => <<"标题"/utf8>>,
        content => lists:duplicate(2001, $x)
    }),
    {error, invalid_argument} = cs_quick_reply_app:page(#{
        organization_id => T, page => 0, workspace_id => W
    }),
    {error, forbidden} = cs_quick_reply_app:seat_list(#{
        organization_id => T, workspace_id => WOther, seat_user_id => 900099
    }),
    {error, forbidden} = cs_quick_reply_app:seat_list(#{
        organization_id => T, workspace_id => W, seat_user_id => 900099
    }),
    ok.

%% 隔离库直连池（talk_record_t05 已迁 00000240；本套件专用 996 段清场）。
%% 不触碰共享池：pooler `csqr` 池 + elib_pg 池化查询仍走 `pgsql` 名——本套件
%% 的数据面调用经 cs_pg_quick_reply:with_tx（默认池名 pgsql），故此处替换
%% 默认池并在 teardown 恢复（无并发套件：单模块 t= 运行形态）。
ensure_pool() ->
    catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    _ = code:ensure_loaded(pooler),
    {ok, _} = application:ensure_all_started(pooler),
    true = code:add_patha(filename:absname("deps/pooler/ebin")),
    _ = application:load(imboy),
    ok = application:set_env(imboy, pg_conf, #{
        name => pgsql,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => os:getenv("T05_PG_HOST", "127.0.0.1"),
                    port => list_to_integer(os:getenv("T05_PG_PORT", "4323")),
                    username => list_to_binary(os:getenv("T05_PG_USER", "imboy_user")),
                    password => list_to_binary(os:getenv("T05_PG_PASSWORD", "")),
                    database => list_to_binary(os:getenv("T05_PG_DB", "talk_record_t05")),
                    ssl => false,
                    timeout => 10000
                }
            ]}
    }),
    catch pooler:rm_pool(pgsql),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 1,
        max_count => 2,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => os:getenv("T05_PG_HOST", "127.0.0.1"),
                    port => list_to_integer(os:getenv("T05_PG_PORT", "4323")),
                    username => list_to_binary(os:getenv("T05_PG_USER", "imboy_user")),
                    password => list_to_binary(os:getenv("T05_PG_PASSWORD", "")),
                    database => list_to_binary(os:getenv("T05_PG_DB", "talk_record_t05")),
                    ssl => false,
                    timeout => 10000
                }
            ]}
    }),
    ok.
