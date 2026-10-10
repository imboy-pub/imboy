-module(group_member_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

list_member_all_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end}
            ]},
            {user_repo, [
                {'tablename', 0, fun() ->
                    <<"user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [1, 50000]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"gm.status = 1">>)),
                    {ok, [#{<<"nickname">> => <<"user-1">>, <<"user_id">> => 100}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, [#{<<"nickname">> => <<"user-1">>, <<"user_id">> => 100}]},
                group_member_ds:list_member(1, [])
            )
        end
    ).

list_member_filtered_uses_offset_placeholders_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end}
            ]},
            {user_repo, [
                {'tablename', 0, fun() ->
                    <<"user">>
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [1, 100, 200]) ->
                    SqlBin = iolist_to_binary(Sql),
                    case
                        binary:match(
                            SqlBin,
                            <<"gm.group_id = $1 AND gm.status = 1 AND gm.user_id IN ($2,$3)">>
                        )
                    of
                        nomatch -> erlang:error({unexpected_sql, SqlBin});
                        {_, _} -> ok
                    end,
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, []}, group_member_ds:list_member(1, [100, 200]))
        end
    ).

join_group_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end},
                {'upsert_active', 5, fun(_Conn, 1, 100, 1, <<"invite">>) ->
                    {ok, true}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {elib_str, [
                {'trunc', 2, fun(Value, 100) ->
                    Value
                end}
            ]},
            {elib_pg, [
                %% T5：join_group 前置 workspace 子集校验的 scope 查询——
                %% 返回空行（个人群/无 workspace 归属）→ 校验放行，
                %% 保持本测试"普通群入群成功"的原有语义。
                %% （meck 同名 expect 后注册会覆盖前者，多分支须合并为
                %%  单个多子句 fun）
                {'query', 3, fun
                    %% discussion 访问锁（791b8817）：无 discussion 绑定语义 → 放行
                    (_Conn, <<"LOCK TABLE public.channel", _/binary>>, []) ->
                        {ok, []};
                    %% join 成员校验（allowed 谓词）→ 放行
                    (_Conn, <<"SELECT ((NOT EXISTS (SELECT 1 FROM public.channel", _/binary>>, _) ->
                        {ok, [#{<<"allowed">> => true}]};
                    (_Conn, <<"SELECT id FROM public.channel", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT discussion_group_id FROM public.channel", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT id FROM public.\"group\"", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"INSERT INTO public.msg_store_seq", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT workspace_id FROM \"group\"", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, _Sql, [1]) ->
                        {ok, [#{<<"member_count">> => 100}]}
                end},
                {'update', 5, fun(_Conn, <<"group">>, Data, <<"id = $1">>, [1]) ->
                    ?assertEqual(100, maps:get(member_count, Data)),
                    {ok, 1}
                end},
                %% E2EE-2026-012：join 事务内 open 世代（msg_store_seq 无自增锁）
                {'execute', 3, fun(_Conn, <<"WITH lock_row AS ", _/binary>>, _) ->
                    {ok, 1}
                end}
            ]},
            {group_repo, [
                {'tablename', 0, fun() ->
                    <<"group">>
                end}
            ]},
            {group_ds, [
                {'join', 2, fun(100, 1) ->
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 100}, group_member_ds:join_group(self(), <<"invite">>, 100, 1, #{role => 1})
            )
        end
    ).

join_group_already_member_returns_zero_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end},
                {'upsert_active', 5, fun(_Conn, 1, 100, 1, <<"invite">>) ->
                    {ok, false}
                end}
            ]},
            %% T5：前置 workspace 子集校验的 scope 查询（个人群 → 空行放行）
            {elib_pg, [
                {'query', 3, fun
                    %% discussion 访问锁（791b8817）：无 discussion 绑定语义 → 放行
                    (_Conn, <<"LOCK TABLE public.channel", _/binary>>, []) ->
                        {ok, []};
                    %% join 成员校验（allowed 谓词）→ 放行
                    (_Conn, <<"SELECT ((NOT EXISTS (SELECT 1 FROM public.channel", _/binary>>, _) ->
                        {ok, [#{<<"allowed">> => true}]};
                    (_Conn, <<"SELECT id FROM public.channel", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT discussion_group_id FROM public.channel", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT id FROM public.\"group\"", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"INSERT INTO public.msg_store_seq", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT workspace_id FROM \"group\"", _/binary>>, _) ->
                        {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 0}, group_member_ds:join_group(self(), <<"invite">>, 100, 1, #{}))
        end
    ).

leave_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end},
                {'find', 3, fun(1, 100, <<"*">>) ->
                    #{<<"id">> => 1, <<"group_id">> => 1, <<"user_id">> => 100, <<"role">> => 2}
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {elib_pg, [
                %% E2EE-2026-012：leave 事务内 close 世代（首子句）；
                %% 原成员 DELETE 为第二子句
                {'execute', 3, fun
                    (_Conn, <<"WITH lock_row AS ", _/binary>>, _) ->
                        {ok, 1};
                    (_Conn, _Sql, [1]) ->
                        {ok, 1}
                end},
                {'query', 3, fun
                    %% discussion 访问锁（791b8817）：无 discussion 绑定语义 → 放行
                    (_Conn, <<"LOCK TABLE public.channel", _/binary>>, []) ->
                        {ok, []};
                    %% join 成员校验（allowed 谓词）→ 放行
                    (_Conn, <<"SELECT ((NOT EXISTS (SELECT 1 FROM public.channel", _/binary>>, _) ->
                        {ok, [#{<<"allowed">> => true}]};
                    (_Conn, <<"SELECT id FROM public.channel", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT discussion_group_id FROM public.channel", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"SELECT id FROM public.\"group\"", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, <<"INSERT INTO public.msg_store_seq", _/binary>>, _) ->
                        {ok, []};
                    (_Conn, _Sql, [1]) ->
                        {ok, [#{<<"member_count">> => 200}]}
                end},
                {'update', 5, fun(_Conn, <<"group">>, Data, <<"id = $1">>, [1]) ->
                    ?assertEqual(200, maps:get(member_count, Data)),
                    {ok, 1}
                end}
            ]},
            {group_repo, [
                {'tablename', 0, fun() ->
                    <<"group">>
                end}
            ]},
            {jsone_encode, [
                {'encode', 2, fun(_Data, [native_utf8]) ->
                    {ok, <<"{}">>}
                end}
            ]},
            {group_log_repo, [
                {'add', 2, fun(_Conn, LogData) ->
                    ?assertEqual(200, maps:get(type, LogData)),
                    {ok, 1, #{}}
                end}
            ]}
        ],
        fun() ->
            {ok, 200, GM} = group_member_ds:leave(self(), 100, 1, 100),
            ?assertEqual(100, maps:get(<<"user_id">>, GM))
        end
    ).

%% Q05/F07：成员行不存在时返回 {ok, absent}，供 logic 层区分"真实删除"
%% 与"重复请求"，避免重复 publish group_member_leave 通知。
leave_absent_member_returns_absent_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end},
                {'find', 3, fun(_Gid, _Uid, <<"*">>) ->
                    #{}
                end}
            ]},
            {elib_pg, [
                %% discussion 访问锁（791b8817）：无 discussion 绑定 → 放行
                {'query', 3, fun
                    (_Conn, <<"LOCK TABLE public.channel", _/binary>>, []) -> {ok, []};
                    (_Conn, <<"SELECT id FROM public.channel", _/binary>>, _) -> {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, absent}, group_member_ds:leave(self(), 100, 1, 100))
        end
    ).

alias_success_test_() ->
    ?WITH_MECKS(
        [
            {group_member_repo, [
                {'tablename', 0, fun() ->
                    <<"group_member">>
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    <<"2026-03-16T00:00:00Z">>
                end}
            ]},
            {elib_pg, [
                {'update', 4, fun(
                    <<"group_member">>, Data, <<"group_id = $1 AND user_id = $2">>, [1, 100]
                ) ->
                    ?assertEqual(<<"alias">>, maps:get(alias, Data)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_member_ds:alias(100, 1, <<"alias">>, <<"desc">>))
        end
    ).

get_member_info_not_found_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'find', 3, fun(1, 100, <<"*">>) ->
                #{}
            end}
        ],
        fun() ->
            ?assertEqual({error, not_found}, group_member_ds:get_member_info(1, 100, <<"*">>))
        end
    ).

check_admin_true_test_() ->
    ?WITH_MECK(
        group_member_repo,
        [
            {'find', 3, fun(1, 100, <<"role">>) ->
                #{<<"role">> => 3}
            end}
        ],
        fun() ->
            ?assertEqual(true, group_member_ds:check_admin(100, 1))
        end
    ).
