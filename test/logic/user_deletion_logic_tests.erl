-module(user_deletion_logic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try Fun()
        after meck:unload(Modules)
        end
    end)()
).

%% ===================================================================
%% export_user_data/1 Tests
%% ===================================================================

export_user_data_ok_test() ->
    ?WITH_MECKS([elib_pg, user_repo, friend_repo, group_repo, group_member_repo, user_setting_repo, elib_dt], fun() ->
        meck:expect(user_repo, tablename, fun() -> <<"public.user">> end),
        meck:expect(friend_repo, tablename, fun() -> <<"public.friend">> end),
        meck:expect(group_repo, tablename, fun() -> <<"public.group">> end),
        meck:expect(group_member_repo, tablename, fun() -> <<"public.group_member">> end),
        meck:expect(user_setting_repo, tablename, fun() -> <<"public.user_setting">> end),
        meck:expect(elib_dt, now, fun() -> <<"2026-01-01T00:00:00Z">> end),
        meck:expect(elib_pg, query, fun
            (<<"SELECT id, account", _/binary>>, [1]) ->
                {ok, [#{<<"id">> => 1, <<"nickname">> => <<"Alice">>}]};
            (<<"SELECT to_user_id", _/binary>>, [1]) ->
                {ok, [#{<<"to_user_id">> => 2}]};
            (<<"SELECT g.id", _/binary>>, [1]) ->
                {ok, [#{<<"id">> => 100, <<"title">> => <<"Group1">>}]};
            (<<"SELECT * FROM", _/binary>>, [1]) ->
                {ok, [#{<<"language">> => <<"zh">>}]}
        end),
        {ok, Data} = user_deletion_logic:export_user_data(1),
        ?assert(is_map(Data)),
        ?assert(maps:is_key(<<"user_info">>, Data)),
        ?assert(maps:is_key(<<"friends">>, Data)),
        ?assert(maps:is_key(<<"groups">>, Data)),
        ?assert(maps:is_key(<<"settings">>, Data)),
        ?assert(maps:is_key(<<"exported_at">>, Data))
    end).

export_user_data_empty_user_test() ->
    ?WITH_MECKS([elib_pg, user_repo, friend_repo, group_repo, group_member_repo, user_setting_repo, elib_dt], fun() ->
        meck:expect(user_repo, tablename, fun() -> <<"public.user">> end),
        meck:expect(friend_repo, tablename, fun() -> <<"public.friend">> end),
        meck:expect(group_repo, tablename, fun() -> <<"public.group">> end),
        meck:expect(group_member_repo, tablename, fun() -> <<"public.group_member">> end),
        meck:expect(user_setting_repo, tablename, fun() -> <<"public.user_setting">> end),
        meck:expect(elib_dt, now, fun() -> <<"2026-01-01T00:00:00Z">> end),
        meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
        {ok, Data} = user_deletion_logic:export_user_data(999),
        ?assertEqual(#{}, maps:get(<<"user_info">>, Data)),
        ?assertEqual([], maps:get(<<"friends">>, Data))
    end).

%% ===================================================================
%% GenServer Tests
%% ===================================================================

cleanup_now_no_expired_test() ->
    Mods = [elib_pg, elib_log, user_repo, user_ds],
    ?WITH_MECKS(Mods, fun() ->
        meck:expect(user_repo, tablename, fun() -> <<"public.user">> end),
        meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),

        application:set_env(imboy, user_deletion_enabled, true),
        application:set_env(imboy, user_deletion_interval, 86400000),
        application:set_env(imboy, user_deletion_retention_days, 60),
        application:set_env(imboy, user_deletion_batch_size, 10),

        case whereis(user_deletion_logic) of
            undefined ->
                {ok, Pid} = user_deletion_logic:start_link(),
                try
                    {ok, Count} = user_deletion_logic:cleanup_now(),
                    ?assertEqual(0, Count)
                after
                    gen_server:stop(Pid)
                end;
            _Pid ->
                {ok, Count} = user_deletion_logic:cleanup_now(),
                ?assert(is_integer(Count))
        end
    end).

cleanup_now_with_expired_test() ->
    Mods = [elib_pg, elib_log, user_repo, user_ds],
    ?WITH_MECKS(Mods, fun() ->
        meck:expect(user_repo, tablename, fun() -> <<"public.user">> end),
        meck:expect(elib_pg, query, fun(_, _) ->
            {ok, [#{<<"id">> => 101}, #{<<"id">> => 102}]}
        end),
        meck:expect(user_ds, delete_all_related_data, fun(_Uid) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),

        application:set_env(imboy, user_deletion_enabled, true),
        application:set_env(imboy, user_deletion_interval, 86400000),
        application:set_env(imboy, user_deletion_retention_days, 60),
        application:set_env(imboy, user_deletion_batch_size, 10),

        case whereis(user_deletion_logic) of
            undefined ->
                {ok, Pid} = user_deletion_logic:start_link(),
                try
                    {ok, Count} = user_deletion_logic:cleanup_now(),
                    ?assertEqual(2, Count),
                    %% 验证 delete_all_related_data 被调用了两次
                    ?assertEqual(2, meck:num_calls(user_ds, delete_all_related_data, '_'))
                after
                    gen_server:stop(Pid)
                end;
            _Pid ->
                {ok, Count} = user_deletion_logic:cleanup_now(),
                ?assert(is_integer(Count))
        end
    end).

get_status_test() ->
    Mods = [elib_pg, elib_log, user_repo],
    ?WITH_MECKS(Mods, fun() ->
        meck:expect(user_repo, tablename, fun() -> <<"public.user">> end),
        meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),

        application:set_env(imboy, user_deletion_enabled, true),
        application:set_env(imboy, user_deletion_interval, 86400000),
        application:set_env(imboy, user_deletion_retention_days, 60),
        application:set_env(imboy, user_deletion_batch_size, 10),

        case whereis(user_deletion_logic) of
            undefined ->
                {ok, Pid} = user_deletion_logic:start_link(),
                try
                    Status = user_deletion_logic:get_status(),
                    ?assert(is_map(Status)),
                    ?assertEqual(60, maps:get(retention_days, Status)),
                    ?assertEqual(10, maps:get(batch_size, Status)),
                    ?assertEqual(0, maps:get(total_deleted, Status))
                after
                    gen_server:stop(Pid)
                end;
            _Pid ->
                Status = user_deletion_logic:get_status(),
                ?assert(is_map(Status))
        end
    end).

disabled_mode_test() ->
    Mods = [elib_log],
    ?WITH_MECKS(Mods, fun() ->
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),

        application:set_env(imboy, user_deletion_enabled, false),

        case whereis(user_deletion_logic) of
            undefined ->
                {ok, Pid} = user_deletion_logic:start_link(),
                try
                    Status = user_deletion_logic:get_status(),
                    ?assertEqual(0, maps:get(interval, Status))
                after
                    gen_server:stop(Pid)
                end;
            _ -> ok
        end
    end).
