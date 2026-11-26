-module(web_self_profile_tests).
-include_lib("eunit/include/eunit.hrl").

self_profile_projection_test_() ->
    {setup,
        fun() ->
            lists:foreach(fun(M) -> meck:new(M, [no_link]) end, [
                user_logic, elib_response, elib_pg, elib_pg_sql, fts_user_repo
            ]),
            meck:expect(elib_pg_sql, public_tablename, fun(_) -> <<"public.user_setting">> end),
            meck:expect(elib_pg, query, fun(Sql, [123]) ->
                ?assertEqual(nomatch, binary:match(Sql, <<"json_each_text">>)),
                ?assertEqual(nomatch, binary:match(Sql, <<"FOR UPDATE">>)),
                {ok, [
                    #{
                        <<"user_id">> => 123,
                        <<"setting">> => jsone:encode(#{
                            <<"enable_visibility_read">> => false,
                            <<"visibility_read_fraction">> => 0.83,
                            <<"visibility_read_delay_ms">> => 1350,
                            <<"font_size">> => <<"huge">>,
                            <<"manual_pending">> => #{<<"nested">> => true}
                        })
                    }
                ]}
            end),
            meck:expect(user_logic, find_web_user_setting, fun(Uid) ->
                user_setting_ds:find_web_by_uid(Uid)
            end),
            meck:expect(fts_user_repo, allow_search, fun(123) -> false end),
            meck:expect(elib_response, success, fun(R, P) -> R#{payload => P} end),
            meck:expect(elib_response, error_with_status, fun(R, S, _, _) -> R#{status => S} end)
        end,
        fun(_) ->
            lists:foreach(fun meck:unload/1, [
                fts_user_repo, elib_pg_sql, elib_pg, elib_response, user_logic
            ])
        end,
        fun(_) ->
            ?_test(begin
                %% user_logic 被 no_link mock（非 passthrough）：投影重构新增的
                %% profile_region_selection/2 必须显式登记，委托真实现
                %% （user_region_logic），否则 web_me 在 meck undef 上 503。
                meck:expect(user_logic, profile_region_selection, fun(Region, Raw) ->
                    user_region_logic:profile_region_selection(Region, Raw)
                end),
                meck:expect(user_logic, find_by_id, fun(123, Columns) ->
                    ?assertEqual(
                        <<"id,account,email,nickname,avatar,gender,region,sign,region_selection">>,
                        Columns
                    ),
                    #{
                        <<"id">> => 123,
                        <<"account">> => <<"synthetic">>,
                        <<"email">> => <<>>,
                        <<"nickname">> => <<"name">>,
                        <<"avatar">> => <<>>,
                        <<"gender">> => 3,
                        <<"region">> => <<"region">>,
                        <<"sign">> => <<"sign">>,
                        %% region_selection 列随 profile 投影为
                        %% regionSelection/regionSelectionStatus；null=未选择，
                        %% 非空 region 下的 absent 语义是 unresolved。
                        <<"region_selection">> => null,
                        <<"password">> => <<"must-never-return">>,
                        <<"mobile">> => <<"must-never-return">>
                    }
                end),
                State = #{surface => web_human, jwt_purpose => web_human, current_uid => 123},
                %% Client-provided IDs never select the owner; no request parsing is needed.
                Reply = user_handler:handle_action(web_me, #{query_id => 999}, State),
                Profile = maps:get(payload, Reply),
                ?assertEqual(<<"123">>, maps:get(<<"uid">>, Profile)),
                ?assertNot(maps:is_key(<<"password">>, Profile)),
                ?assertNot(maps:is_key(<<"mobile">>, Profile)),
                ?assertEqual(11, map_size(Profile)),
                ?assertEqual(null, maps:get(<<"regionSelection">>, Profile)),
                ?assertEqual(<<"unresolved">>, maps:get(<<"regionSelectionStatus">>, Profile)),
                Settings = maps:get(<<"setting">>, Profile),
                ?assertEqual(5, map_size(Settings)),
                ?assertEqual(false, maps:get(<<"allow_search">>, Settings)),
                ?assertEqual(false, maps:get(<<"enable_visibility_read">>, Settings)),
                ?assertNot(maps:is_key(<<"manual_pending">>, Settings)),
                {ok, FullSetting} = passport_logic:find_web_user_setting(123),
                ?assertEqual(#{<<"nested">> => true}, maps:get(<<"manual_pending">>, FullSetting)),
                lists:foreach(
                    fun(Result) ->
                        meck:expect(elib_pg, query, fun(_, _) -> Result end),
                        ?assertMatch(
                            #{status := 503}, user_handler:handle_action(web_me, #{}, State)
                        )
                    end,
                    [
                        {error, invalid_json},
                        {error, db_down},
                        {ok, [bad_row]},
                        {ok, [#{<<"user_id">> => 123, <<"setting">> => <<"{bad">>}]},
                        {ok, [#{<<"user_id">> => 123, <<"setting">> => <<"[]">>}]},
                        {ok, [#{<<"user_id">> => 999, <<"setting">> => <<"{}">>}]}
                    ]
                ),
                meck:expect(elib_pg, query, fun(_, _) -> erlang:error(db_down) end),
                ?assertMatch(#{status := 503}, user_handler:handle_action(web_me, #{}, State)),
                meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
                Empty = user_handler:handle_action(web_me, #{}, State),
                ?assertEqual(
                    #{<<"allow_search">> => false},
                    maps:get(<<"setting">>, maps:get(payload, Empty))
                ),
                lists:foreach(
                    fun(Raw) ->
                        meck:expect(elib_pg, query, fun(_, _) ->
                            {ok, [#{<<"user_id">> => 123, <<"setting">> => Raw}]}
                        end),
                        Default = user_handler:handle_action(web_me, #{}, State),
                        ?assertEqual(
                            #{<<"allow_search">> => false},
                            maps:get(<<"setting">>, maps:get(payload, Default))
                        )
                    end,
                    [<<>>, null, <<"{}">>]
                ),
                lists:foreach(
                    fun(Raw) ->
                        meck:expect(elib_pg, query, fun(_, _) ->
                            {ok, [#{<<"user_id">> => 123, <<"setting">> => Raw}]}
                        end),
                        ?assertMatch(
                            #{status := 503}, user_handler:handle_action(web_me, #{}, State)
                        )
                    end,
                    [<<"\"\"">>, <<"[]">>, <<"garbage">>]
                ),
                ?assertMatch(#{status := 401}, user_handler:handle_action(web_me, #{}, #{})),
                ?assertMatch(
                    #{status := 401},
                    user_handler:handle_action(web_me, #{}, State#{jwt_purpose := human})
                ),
                meck:expect(user_logic, find_by_id, fun(_, _) -> #{} end),
                ?assertMatch(#{status := 404}, user_handler:handle_action(web_me, #{}, State)),
                meck:expect(user_logic, find_by_id, fun(_, _) -> erlang:error(db_down) end),
                ?assertMatch(#{status := 503}, user_handler:handle_action(web_me, #{}, State))
            end)
        end}.
