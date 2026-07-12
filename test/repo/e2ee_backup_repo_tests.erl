-module(e2ee_backup_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc E2EE 加密密钥备份 Repo 层测试：参数化 SQL 与行映射
%%%===================================================================

save_params() ->
    #{
        <<"uid">> => 9999,
        <<"backup_version">> => 1,
        <<"algo">> => <<"pbkdf2-sha256/aes-256-gcm">>,
        <<"kdf_salt">> => <<"c2FsdA==">>,
        <<"kdf_iterations">> => 310000,
        <<"encrypted_payload">> => <<"Y2lwaGVy">>,
        <<"payload_hash">> => <<"deadbeef">>
    }.

save_inserts_with_tsid_and_ordered_params_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(e2ee_key_backup) -> 111222333 end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(Sql, Params) ->
                    ?assertMatch({_, _}, binary:match(Sql, <<"INSERT INTO e2ee_key_backups">>)),
                    %% 参数顺序必须与 SQL 占位符一致
                    ?assertEqual(
                        [
                            111222333,
                            9999,
                            1,
                            <<"pbkdf2-sha256/aes-256-gcm">>,
                            <<"c2FsdA==">>,
                            310000,
                            <<"Y2lwaGVy">>,
                            <<"deadbeef">>
                        ],
                        Params
                    ),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 111222333}, e2ee_backup_repo:save(save_params()))
        end
    ).

save_propagates_db_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(e2ee_key_backup) -> 1 end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_, _) ->
                    {error, {pgsql_error, #{code => <<"23505">>}}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, {pgsql_error, _}}, e2ee_backup_repo:save(save_params()))
        end
    ).

latest_returns_top_version_row_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [9999]) ->
                    ?assertMatch(
                        {_, _}, binary:match(Sql, <<"ORDER BY backup_version DESC LIMIT 1">>)
                    ),
                    {ok, [#{<<"backup_version">> => 7, <<"encrypted_payload">> => <<"ct">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{<<"backup_version">> => 7, <<"encrypted_payload">> => <<"ct">>}},
                e2ee_backup_repo:latest(9999)
            )
        end
    ).

latest_empty_is_not_found_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_, [9999]) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, e2ee_backup_repo:latest(9999))
        end
    ).

delete_by_uid_returns_count_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(Sql, [9999]) ->
                    ?assertMatch({_, _}, binary:match(Sql, <<"DELETE FROM e2ee_key_backups">>)),
                    {ok, 2}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 2}, e2ee_backup_repo:delete_by_uid(9999))
        end
    ).
