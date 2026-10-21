%% No PostgreSQL: SQL-aware projection mock plus the real Disk delete boundary.
-module(attachment_gc_physical_key_tests).
-include_lib("eunit/include/eunit.hrl").

physical_key_override_delete_test_() ->
    {timeout, 30, fun() -> with_disk(<<"physical/winner.aac">>) end}.
legacy_null_physical_key_delete_test_() ->
    {timeout, 30, fun() -> with_disk(null) end}.

with_disk(RawPhysicalKey) ->
    undefined = whereis(elib_pg_meck),
    Parent =
        case os:getenv("V5_PHYSICAL_KEY_TEST_ROOT") of
            false -> canonical_tmp();
            Value -> Value
        end,
    true = is_list(Parent) andalso filename:pathtype(Parent) =:= absolute,
    Root = filename:join(
        Parent,
        "owned-" ++
            binary_to_list(
                binary:encode_hex(crypto:strong_rand_bytes(12), lowercase)
            )
    ),
    ok = file:make_dir(Root),
    ok = file:change_mode(Root, 8#700),
    Previous = application:get_env(imboy, attachment_storage_registry),
    Tab = ets:new(physical_key_test, [public]),
    try
        run_disk(Root, Tab, RawPhysicalKey)
    after
        try
            meck:unload(elib_pg)
        catch
            _:_ -> ok
        end,
        restore_registry(Previous),
        ets:delete(Tab),
        ok = file:del_dir_r(Root)
    end.

run_disk(Root, Tab, RawPhysicalKey) ->
    Ns = <<"physical-key-unit">>,
    Container = <<"capture">>,
    Logical = <<"logical/reference.aac">>,
    Physical =
        case RawPhysicalKey of
            null -> Logical;
            _ -> RawPhysicalKey
        end,
    LogicalFile = filename:join([Root, binary_to_list(Container), binary_to_list(Logical)]),
    PhysicalFile = filename:join([Root, binary_to_list(Container), binary_to_list(Physical)]),
    LogicalBytes = <<"different logical object: preserve me">>,
    PhysicalBytes = <<"authorized physical object: delete me">>,
    write_file(LogicalFile, LogicalBytes),
    case RawPhysicalKey of
        null -> ok;
        _ -> write_file(PhysicalFile, PhysicalBytes)
    end,
    ok = application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            physical_key_unit => #{
                driver => <<"disk">>,
                namespace => Ns,
                container => Container,
                endpoint => <<"file://", (list_to_binary(Root))/binary>>
            }
        }
    }),
    {ok, Rec} = attachment_storage:resolve(Ns),
    Row = #{
        <<"out_path">> => Logical,
        <<"out_bucket">> => Container,
        <<"out_storage_driver">> => <<"disk">>,
        <<"out_storage_namespace">> => Ns,
        <<"out_storage_container">> => Container,
        <<"out_storage_locator_sha256">> => attachment_storage:locator_digest(Ns, Rec),
        <<"out_storage_physical_key">> => RawPhysicalKey,
        <<"out_attempts">> => 1
    },
    ets:insert(Tab, {claims, 0}),
    ok = meck:new(elib_pg, [non_strict, no_link]),
    ok = meck:expect(elib_pg, query, fun(conn, Sql0, Params) ->
        Sql = iolist_to_binary(Sql0),
        case binary:match(Sql, <<"FROM fn_v5_claim_ordinary($1)">>) of
            nomatch ->
                [Logical, _Token, AckOk] = Params,
                ?assertEqual(<<"SELECT fn_v5_ack_ordinary($1, $2, $3)">>, Sql),
                ets:insert(Tab, {ack, Logical, AckOk}),
                {ok, [#{<<"fn_v5_ack_ordinary">> => <<"done">>}]};
            _ ->
                [_Token] = Params,
                case ets:update_counter(Tab, claims, 1) of
                    1 -> {ok, [sql_projection(Sql, Row)]};
                    _ -> {ok, []}
                end
        end
    end),
    {ok, 1} = attachment_delete_worker:run_batch(
        conn,
        fun(_Ns, _Container, LogicalKey, Identity) ->
            ets:insert(Tab, {delete_snapshot, LogicalKey, maps:get(key, Identity)}),
            attachment_storage:delete_snapshot(Identity)
        end
    ),
    ?assertEqual([{ack, Logical, true}], ets:lookup(Tab, ack)),
    ?assertEqual({error, enoent}, file:read_file(PhysicalFile)),
    ?assertEqual([{delete_snapshot, Logical, Physical}], ets:lookup(Tab, delete_snapshot)),
    case RawPhysicalKey of
        null -> ok;
        _ -> ?assertEqual({ok, LogicalBytes}, file:read_file(LogicalFile))
    end,
    ?assert(meck:validate(elib_pg)).

%% Mimic the driver's selected-column map, not an always-complete fixture row.
sql_projection(Sql, Row) ->
    {match, [Columns]} = re:run(
        Sql,
        <<"^SELECT (.+) FROM fn_v5_claim_ordinary">>,
        [{capture, [1], binary}, dotall]
    ),
    Keys = [
        list_to_binary(string:trim(binary_to_list(K)))
     || K <- binary:split(Columns, <<",">>, [global])
    ],
    true = lists:all(fun(K) -> maps:is_key(K, Row) end, Keys),
    maps:with(Keys, Row).

write_file(Path, Bytes) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bytes, [exclusive]),
    ok = file:change_mode(Path, 8#600).
restore_registry(undefined) -> application:unset_env(imboy, attachment_storage_registry);
restore_registry({ok, Value}) -> application:set_env(imboy, attachment_storage_registry, Value).

canonical_tmp() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.
