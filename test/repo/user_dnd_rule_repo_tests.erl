-module(user_dnd_rule_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_dnd_rule_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户免打扰(DND)规则数据访问层
%%% 覆盖：表名、查询(命中/未命中)、upsert、删除
%%%===================================================================

%% ===================================================================
%% tablename/0
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(
        elib_pg_sql,
        [
            {'public_tablename', 1, fun(_Table) -> <<"public.user_dnd_rule">> end}
        ],
        fun() ->
            ?assertEqual(<<"public.user_dnd_rule">>, user_dnd_rule_repo:tablename())
        end
    ).

%% ===================================================================
%% find_by_uid/1
%% ===================================================================

find_by_uid_found_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.user_dnd_rule">> end}]},
            {elib_pg, [
                {'one', 2, fun(Sql, Params) ->
                    ?assert(binary:match(Sql, <<"FROM public.user_dnd_rule">>) =/= nomatch),
                    ?assert(binary:match(Sql, <<"WHERE user_id = $1">>) =/= nomatch),
                    ?assertEqual([1], Params),
                    {ok, #{
                        <<"user_id">> => 1,
                        <<"start_min">> => 1320,
                        <<"end_min">> => 480,
                        <<"status">> => 1
                    }}
                end}
            ]}
        ],
        fun() ->
            Rule = user_dnd_rule_repo:find_by_uid(1),
            ?assertEqual(1, maps:get(<<"user_id">>, Rule)),
            ?assertEqual(1320, maps:get(<<"start_min">>, Rule))
        end
    ).

find_by_uid_not_found_returns_empty_map_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.user_dnd_rule">> end}]},
            {elib_pg, [{'one', 2, fun(_Sql, _Params) -> {ok, []} end}]}
        ],
        fun() ->
            ?assertEqual([], user_dnd_rule_repo:find_by_uid(999))
        end
    ).

%% binary Uid 应被 ec_cnv:to_integer 归一为 integer
find_by_uid_accepts_binary_uid_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.user_dnd_rule">> end}]},
            {elib_pg, [
                {'one', 2, fun(_Sql, Params) ->
                    % 已转 integer
                    ?assertEqual([1], Params),
                    {ok, #{<<"user_id">> => 1}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(#{<<"user_id">> => 1}, user_dnd_rule_repo:find_by_uid(<<"1">>))
        end
    ).

%% ===================================================================
%% upsert/1
%% ===================================================================

upsert_executes_insert_on_conflict_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.user_dnd_rule">> end},
                {'insert', 2, fun(_Tb, Data) ->
                    %% updated_at 应由 repo 注入
                    ?assert(maps:is_key(<<"updated_at">>, Data)),
                    {<<"INSERT INTO public.user_dnd_rule (...) VALUES (...)">>, [1, 1320, 480, 1]}
                end}
            ]},
            {elib_dt, [{'now', 0, fun() -> <<"2026-06-02 12:00:00+08">> end}]},
            {elib_pg, [
                {'execute', 2, fun(Sql, _Params) ->
                    Bin = iolist_to_binary(Sql),
                    ?assert(binary:match(Bin, <<"ON CONFLICT (user_id) DO UPDATE">>) =/= nomatch),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"user_id">> => 1,
                <<"start_min">> => 1320,
                <<"end_min">> => 480,
                <<"status">> => 1
            },
            ?assertEqual(ok, user_dnd_rule_repo:upsert(Data))
        end
    ).

%% ===================================================================
%% delete_by_uid/1
%% ===================================================================

delete_by_uid_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.user_dnd_rule">> end}]},
            {elib_pg, [
                {'execute', 2, fun(Sql, Params) ->
                    ?assert(binary:match(Sql, <<"DELETE FROM public.user_dnd_rule">>) =/= nomatch),
                    ?assertEqual([1], Params),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, user_dnd_rule_repo:delete_by_uid(1))
        end
    ).
