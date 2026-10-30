-module(attachment_deadlock_scope_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

scope_deadlock_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(workspace_lock_only_retryable_variant_preserves_deadlock()) end,
        fun(_) -> ?_test(workspace_non_deadlock_remains_503()) end,
        fun(_) -> ?_test(resolver_deadlock_uses_transaction_connection()) end,
        fun(_) -> ?_test(upload_scope_lock_error_propagates()) end,
        fun(_) -> ?_test(upload_scope_query_error_propagates()) end,
        fun(_) -> ?_test(upload_scope_other_error_remains_false()) end
    ]}.

setup() ->
    meck:new([elib_pg, workspace_resolver], [no_link, passthrough]),
    meck:expect(workspace_resolver, resolve_workspace, fun(_) -> {ok, 11} end),
    meck:expect(workspace_resolver, resolve_workspace_tx, fun(_, _) -> {ok, 11} end),
    ok.
cleanup(_) ->
    meck:unload(elib_pg),
    meck:unload(workspace_resolver).

error_record(Code) ->
    #error{
        severity = error,
        code = Code,
        codename = unknown,
        message = <<"synthetic">>,
        extra = []
    }.
workspace_lock_only_retryable_variant_preserves_deadlock() ->
    Err = error_record(<<"40P01">>),
    meck:expect(elib_pg, query, fun(_, _, _) -> {error, Err} end),
    ?assertEqual({error, Err}, workspace_guard:ensure_writable_tx_retryable(fake, {group, 1})),
    ?assertMatch({error, {503, _}}, workspace_guard:ensure_writable_tx(fake, {group, 1})).
workspace_non_deadlock_remains_503() ->
    Err = error_record(<<"23505">>),
    meck:expect(elib_pg, query, fun(_, _, _) -> {error, Err} end),
    ?assertMatch(
        {error, {503, _}},
        workspace_guard:ensure_writable_tx_retryable(fake, {group, 1})
    ).
resolver_deadlock_uses_transaction_connection() ->
    Err = error_record(<<"40P01">>),
    meck:expect(
        workspace_resolver,
        resolve_workspace_tx,
        fun(fake, {group, 1}) -> error({resolver_db_error, Err}) end
    ),
    ?assertEqual({error, Err}, workspace_guard:ensure_writable_tx_retryable(fake, {group, 1})),
    ?assert(meck:called(workspace_resolver, resolve_workspace_tx, [fake, {group, 1}])).
upload_scope_lock_error_propagates() ->
    Err = error_record(<<"40P01">>),
    meck:expect(elib_pg, query, fun(_, _, _) -> {error, Err} end),
    ?assertEqual(
        {error, Err}, attachment_repo:authorize_upload_scope_tx_retryable(fake, group, 1, 2)
    ),
    ?assertEqual(false, attachment_repo:authorize_upload_scope_tx(fake, group, 1, 2)).
upload_scope_query_error_propagates() ->
    Err = error_record(<<"40P01">>),
    meck:expect(elib_pg, query, fun(_, Sql, _) ->
        case binary:match(iolist_to_binary(Sql), <<"FOR SHARE">>) of
            nomatch -> {error, Err};
            _ -> {ok, []}
        end
    end),
    ?assertEqual(
        {error, Err}, attachment_repo:authorize_upload_scope_tx_retryable(fake, group, 1, 2)
    ),
    ?assertEqual(false, attachment_repo:authorize_upload_scope_tx(fake, group, 1, 2)).
upload_scope_other_error_remains_false() ->
    Err = error_record(<<"23505">>),
    meck:expect(elib_pg, query, fun(_, _, _) -> {error, Err} end),
    ?assertEqual(false, attachment_repo:authorize_upload_scope_tx_retryable(fake, group, 1, 2)),
    ?assertEqual(false, attachment_repo:authorize_upload_scope_tx(fake, group, 1, 2)).

scope_ds_legacy_contract_test_() ->
    {setup,
        fun() ->
            meck:new([workspace_guard, attachment_repo], [no_link, passthrough]),
            meck:expect(attachment_repo, lock_upload_organization_tx, fun(_, _, _) -> ok end),
            meck:expect(workspace_guard, ensure_writable_tx, fun(_, _) ->
                {error, {503, <<"unavailable">>}}
            end),
            meck:expect(workspace_guard, ensure_writable_tx_retryable, fun(_, _) ->
                {error, error_record(<<"40P01">>)}
            end),
            ok
        end,
        fun(_) ->
            meck:unload(workspace_guard),
            meck:unload(attachment_repo)
        end,
        fun(_) ->
            ?_test(begin
                ?assertEqual(
                    {error, {503, <<"unavailable">>}},
                    attachment_ds:ensure_upload_scope_tx(fake, {group, 1}, 2)
                ),
                ?assertEqual(
                    {error, error_record(<<"40P01">>)},
                    attachment_ds:ensure_upload_scope_tx_retryable(fake, {group, 1}, 2)
                )
            end)
        end}.

resolver_same_connection_test_() ->
    {foreach, fun() -> meck:new(elib_pg, [no_link, passthrough]) end,
        fun(_) -> meck:unload(elib_pg) end,
        [
            fun(_) ->
                ?_test(begin
                    meck:expect(elib_pg, query, fun(fake_conn, Sql, [9]) ->
                        ?assertEqual(Expected, Sql),
                        {ok, [#{<<"scope">> => <<"workspace">>, <<"workspace_id">> => 11}]}
                    end),
                    ?assertEqual(
                        {ok, 11}, workspace_resolver:resolve_workspace_tx(fake_conn, {Kind, 9})
                    )
                end)
            end
         || {Kind, Expected} <- [
                {group, <<"SELECT scope, workspace_id FROM \"group\" WHERE id = $1">>},
                {channel, <<"SELECT scope, workspace_id FROM channel WHERE id = $1">>}
            ]
        ] ++
            [
                fun(_) ->
                    ?_test(begin
                        Err = error_record(<<"40P01">>),
                        meck:expect(elib_pg, query, fun(fake_conn, _, _) -> {error, Err} end),
                        ?assertError(
                            {resolver_db_error, Err},
                            workspace_resolver:resolve_workspace_tx(fake_conn, {group, 9})
                        ),
                        ?assertEqual(
                            {error, Err},
                            workspace_guard:ensure_writable_tx_retryable(fake_conn, {group, 9})
                        )
                    end)
                end,
                fun(_) ->
                    ?_test(begin
                        meck:expect(elib_pg, query, fun(fake_conn, _, _) -> {ok, []} end),
                        ?assertEqual(
                            {error, not_found},
                            workspace_resolver:resolve_workspace_tx(fake_conn, {channel, 9})
                        )
                    end)
                end
            ]}.
