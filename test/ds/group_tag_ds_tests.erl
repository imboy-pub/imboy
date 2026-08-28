-module(group_tag_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

add_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {group_tag_repo, [
                {'exists_tx', 3, fun(_Conn, 1, <<"tag-a">>) ->
                    false
                end},
                {'add', 2, fun(_Conn, Data) ->
                    ?assertEqual(1, maps:get(group_id, Data)),
                    ?assertEqual(<<"tag-a">>, maps:get(tag_name, Data)),
                    {ok, 7}
                end}
            ]},
            %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
            {workspace_resolver, [{'resolve_workspace', 1, fun(_) -> personal end}]},
            {elib_pg, [{'with_tx', 1, fun(TxFun) -> TxFun(fake_conn) end}]}
        ],
        fun() ->
            ?assertEqual({ok, 7}, group_tag_ds:add(1, 100, <<"tag-a">>))
        end
    ).

add_existing_tag_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {group_tag_repo, [
                {'exists_tx', 3, fun(_Conn, _GroupId, _TagName) ->
                    true
                end}
            ]},
            %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
            {workspace_resolver, [{'resolve_workspace', 1, fun(_) -> personal end}]},
            {elib_pg, [{'with_tx', 1, fun(TxFun) -> TxFun(fake_conn) end}]}
        ],
        fun() ->
            ?assertEqual({error, <<"标签已存在"/utf8>>}, group_tag_ds:add(1, 100, <<"tag-a">>))
        end
    ).

add_long_tag_name_returns_error_test() ->
    LongTag = list_to_binary(lists:duplicate(51, $x)),
    ?assertEqual({error, <<"标签名过长，最多50个字符"/utf8>>}, group_tag_ds:add(1, 100, LongTag)).

remove_success_test_() ->
    ?WITH_MECKS(
        [
            {group_tag_repo, [
                {'delete_tx', 3, fun(_Conn, 1, <<"tag-a">>) ->
                    {ok, 1}
                end}
            ]},
            %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
            {workspace_resolver, [{'resolve_workspace', 1, fun(_) -> personal end}]},
            {elib_pg, [{'with_tx', 1, fun(TxFun) -> TxFun(fake_conn) end}]}
        ],
        fun() ->
            ?assertEqual(ok, group_tag_ds:remove(1, 100, <<"tag-a">>))
        end
    ).

list_success_test_() ->
    ?WITH_MECK(
        group_tag_repo,
        [
            {'list_by_group', 2, fun(1, <<"id, tag_name, created_by, created_at">>) ->
                {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"tag-a">>}]}
            end}
        ],
        fun() ->
            ?assertEqual(
                {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"tag-a">>}]}, group_tag_ds:list(1)
            )
        end
    ).

search_empty_tag_name_returns_error_test() ->
    ?assertEqual({error, <<"标签名不能为空"/utf8>>}, group_tag_ds:search(<<>>)).

hot_tags_invalid_limit_returns_empty_list_test() ->
    ?assertEqual({ok, []}, group_tag_ds:hot_tags(0)).

count_repo_error_returns_zero_test_() ->
    ?WITH_MECK(
        group_tag_repo,
        [
            {'count_by_group', 1, fun(_GroupId) ->
                {error, db_error}
            end}
        ],
        fun() ->
            ?assertEqual({ok, 0}, group_tag_ds:count(1))
        end
    ).

%% ===================================================================
%% H-2：DS 写事务错误形态透传（原 ec_cnv:to_binary 对元组 function_clause 崩 500）
%% ===================================================================

%% with_tx mock 须模拟 epgsql:with_transaction 的 abort_tx 归一语义
tx_abort_mock() ->
    {'with_tx', 1, fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end}.

remove_archived_error_tuple_no_crash_test_() ->
    ?WITH_MECKS(
        [
            {workspace_resolver, [
                {'resolve_workspace', 1, fun({group, 1}) -> {ok, 800001} end}
            ]},
            {elib_pg, [
                tx_abort_mock(),
                {'query', 3, fun(
                    fake_conn,
                    <<"SELECT status FROM workspace WHERE id = $1 FOR UPDATE">>,
                    [800001]
                ) ->
                    {ok, [#{<<"status">> => <<"archived">>}]}
                end}
            ]},
            {group_tag_repo, [
                {'delete_tx', 3, fun(_Conn, _Gid, _Tag) ->
                    erlang:error(unexpected_delete_after_archived)
                end}
            ]}
        ],
        fun() ->
            %% 归档守卫 980 元组必须原样透传（不崩、不压成乱码 binary）
            ?assertMatch({error, {980, _}}, group_tag_ds:remove(1, 100, <<"tag-a">>))
        end
    ).

add_repo_db_error_tuple_no_crash_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [{'now', 0, fun() -> <<"2026-03-16T00:00:00Z">> end}]},
            {workspace_resolver, [{'resolve_workspace', 1, fun(_) -> personal end}]},
            {elib_pg, [tx_abort_mock()]},
            {group_tag_repo, [
                {'exists_tx', 3, fun(_Conn, _Gid, _Tag) -> false end},
                {'add', 2, fun(_Conn, _Data) ->
                    {error, {pgsql_error, #{code => <<"23505">>}}}
                end}
            ]}
        ],
        fun() ->
            %% repo DB 错误元组必须原样透传（归一交 logic 层）
            ?assertMatch(
                {error, {pgsql_error, _}}, group_tag_ds:add(1, 100, <<"tag-a">>)
            )
        end
    ).
