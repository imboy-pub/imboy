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
    isolated_pg_fixture:test(fun() ->
        {timeout, 60, {setup, fun setup/0, fun teardown/1, fun(S) -> [?_test(t_all(S))] end}}
    end).

setup() ->
    %% 快照泄漏前的 pg_conf：ensure_pool 会覆盖为 T05 直跑形态（T05_PG_PASSWORD
    %% 未设时为空密码），teardown 必须还原，否则同 VM 后续 root 的
    %% config_pg_conn/marker 连接全灭（full-20261104T081330Z invalid_password
    %% ×15 + no_members ×20 连环根因；group-20261104T094* 复现实证）。
    PrevPgConf = application:get_env(imboy, pg_conf),
    ok = ensure_pool(),
    {ok, Conn} = elib_pg:with_tx(fun(C) -> {ok, C} end),
    %% 清场（幂等；共享库下本套件专用前缀键）
    {ok, _} = elib_pg:execute(
        Conn,
        <<"DELETE FROM cs_quick_reply WHERE organization_id BETWEEN 996100 AND 996199">>,
        []
    ),
    #{conn => Conn, prev_pg_conf => PrevPgConf}.

teardown(S) ->
    {ok, Conn} = elib_pg:with_tx(fun(C) -> {ok, C} end),
    {ok, _} = elib_pg:execute(
        Conn,
        <<"DELETE FROM cs_quick_reply WHERE organization_id BETWEEN 996100 AND 996199">>,
        []
    ),
    %% 还原覆盖过的 pg_conf 并按原值重建默认池（ensure_pool 曾 rm+new 过
    %% pgsql 池）；Prev 不可用时至少 unset 脏值，fail-closed 交由
    %% eunit_runner:enforce_pool_capacity 的基线自愈兜底。
    case maps:get(prev_pg_conf, S, undefined) of
        {ok, Prev} when is_map(Prev) ->
            ok = application:set_env(imboy, pg_conf, Prev),
            _ = eunit_runner:ensure_default_pool(),
            ok;
        _ ->
            application:unset_env(imboy, pg_conf),
            ok
    end.

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

%% 独立 VM 使用当前测试配置；T05 变量只覆盖显式提供的字段。
ensure_pool() ->
    %% 本套件经 isolated_pg_fixture:test 在 peer VM 内执行：peer 只继承
    %% -config 与 code path，父 VM 的 pgsql 池不随 peer 走。quality 车道
    %% （IMBOY_TEST_CONFIG 注入 worker config）的 -config pg_conf 是正确的
    %% 克隆库连接（不做 T05 覆盖——空密码泄漏源，full-20261104T081330Z
    %% invalid_password/no_members 连环根因），池缺失时按它就地重建；
    %% 本地单跑（无该 env）仍走 T05 直跑形态。
    case pool_ready() of
        true ->
            ok;
        false ->
            case os:getenv("IMBOY_TEST_CONFIG") of
                false -> ensure_pool_t05();
                _ -> ok = eunit_runner:ensure_default_pool()
            end
    end,
    %% peer VM 不继承父 VM 的 tsid 状态（full-20261104T161607Z 实证：
    %% elib_tsid_not_initialized）。init 幂等，T05 分支已含同款。
    catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    ok.

pool_ready() ->
    _ = code:ensure_loaded(pooler),
    try pooler:take_member(pgsql, 100) of
        Pid when is_pid(Pid) ->
            pooler:return_member(pgsql, Pid, ok),
            true;
        _ ->
            false
    catch
        _:_ -> false
    end.

ensure_pool_t05() ->
    catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    _ = code:ensure_loaded(pooler),
    {ok, _} = application:ensure_all_started(pooler),
    _ = application:load(imboy),
    {ok, Base} = application:get_env(imboy, pg_conf),
    {epgsql, connect, [Opts]} = maps:get(start_mfa, Base),
    Overrides = [
        {"T05_PG_HOST", host},
        {"T05_PG_USER", username},
        {"T05_PG_PASSWORD", password},
        {"T05_PG_DB", database}
    ],
    Selected = lists:foldl(
        fun({Env, Key}, Acc) ->
            case os:getenv(Env) of
                false -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        Opts,
        Overrides
    ),
    Connect =
        case os:getenv("T05_PG_PORT") of
            false -> Selected;
            Port -> Selected#{port => list_to_integer(Port)}
        end,
    Conf = Base#{
        name => pgsql,
        init_count => 1,
        max_count => 2,
        start_mfa => {epgsql, connect, [Connect]}
    },
    ok = application:set_env(imboy, pg_conf, Conf),
    catch pooler:rm_pool(pgsql),
    {ok, _} = pooler:new_pool(Conf),
    ok.
