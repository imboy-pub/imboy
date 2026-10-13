-module(moment_atomic_binding_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

binding_failure_aborts_post_test_() ->
    binding_case({error, write_failed}, {error, write_failed}, 0).

missing_or_foreign_media_aborts_post_test_() ->
    binding_case({ok, 1}, {error, invalid_media_attachment}, 0).

all_media_bind_on_same_connection_before_timeline_test_() ->
    binding_case({ok, 2}, {ok, 555}, 1).

binding_case(BindResult, Expected, Writes) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'after_commit', 3, fun(owned_conn, {moment_acl, 555}, _Action) -> ok end},
                {'with_tx', 1, fun(F) ->
                    try
                        F(owned_conn)
                    catch
                        throw:{abort_tx, R} -> {error, R}
                    end
                end},
                {'execute', 3, fun(owned_conn, Sql, [_Id, _Now, Keys, 1001]) ->
                    ?assertEqual([<<"cover">>, <<"image">>], Keys),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"creator_user_id = $4">>)),
                    BindResult
                end}
            ]},
            {elib_pg_sql, [{'public_tablename', 1, fun(_) -> <<"public.attachment">> end}]},
            {moment_post_repo, [{'add', 2, fun(owned_conn, _) -> {ok, 555} end}]},
            {moment_post_acl_repo, [
                {'replace_for_post', 4, fun(owned_conn, 555, [], []) -> ok end}
            ]},
            {moment_timeline_repo, [
                {'upsert_batch', 5, fun(owned_conn, [1001], 555, 1001, _) -> {ok, 1} end}
            ]},
            {friend_ds, [{'list_by_uid', 1, fun(1001) -> [] end}]},
            {user_denylist_repo, [{'in_denylist', 2, fun(_, _) -> 0 end}]}
        ],
        fun() ->
            Media = [
                #{<<"url">> => <<"image">>, <<"cover_url">> => <<"cover">>},
                #{<<"url">> => <<"image">>}
            ],
            ?assertEqual(
                Expected, moment_ds:create_post(1001, #{content => <<"post">>, media => Media})
            ),
            ?assertEqual(Writes, meck:num_calls(moment_post_acl_repo, replace_for_post, '_')),
            ?assertEqual(Writes, meck:num_calls(moment_timeline_repo, upsert_batch, '_'))
        end
    ).
