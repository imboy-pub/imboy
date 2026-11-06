%% No PostgreSQL：SQL 投影 mock + meck 拦截的 Garage 删除边界。
%% V3-20 disk 退役：根与行迁移 garage，真实落盘删除改为 meck delete_snapshot。
-module(attachment_gc_physical_key_tests).
-include_lib("eunit/include/eunit.hrl").

physical_key_override_delete_test_() ->
    {timeout, 30, fun() -> with_garage(<<"physical/winner.aac">>) end}.
legacy_null_physical_key_delete_test_() ->
    {timeout, 30, fun() -> with_garage(null) end}.

with_garage(RawPhysicalKey) ->
    undefined = whereis(elib_pg_meck),
    Previous = {
        application:get_env(imboy, attachment_storage_registry),
        application:get_env(imboy, garage)
    },
    Tab = ets:new(physical_key_test, [public]),
    try
        run_garage(Tab, RawPhysicalKey)
    after
        try
            meck:unload(elib_pg)
        catch
            _:_ -> ok
        end,
        try
            %% V3-20 disk 退役：garage 删除面同样在收尾卸载，避免污染后续用例。
            meck:unload(attachment_storage_garage)
        catch
            _:_ -> ok
        end,
        restore_registry(element(1, Previous)),
        restore_garage(element(2, Previous)),
        ets:delete(Tab)
    end.

run_garage(Tab, RawPhysicalKey) ->
    Ns = <<"physical-key-unit">>,
    Container = <<"capture">>,
    Logical = <<"logical/reference.aac">>,
    Physical =
        case RawPhysicalKey of
            null -> Logical;
            _ -> RawPhysicalKey
        end,
    %% V3-20 disk 退役：root 与 out_storage_driver 迁移 garage（endpoint 指向
    %% 不可达测试地址，删除由下方 meck 拦截，不真发网络）。
    Endpoint = <<"http://garage.invalid:3900">>,
    ok = application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            physical_key_unit => #{
                driver => <<"garage">>,
                namespace => Ns,
                container => Container,
                endpoint => Endpoint
            }
        }
    }),
    %% garage 分支的 frozen_location 校验要求应用 env endpoint 与 root 相等。
    ok = application:set_env(imboy, garage, #{endpoint => Endpoint}),
    {ok, Rec} = attachment_storage:resolve(Ns),
    Row = #{
        <<"out_path">> => Logical,
        <<"out_bucket">> => Container,
        <<"out_storage_driver">> => <<"garage">>,
        <<"out_storage_namespace">> => Ns,
        <<"out_storage_container">> => Container,
        <<"out_storage_locator_sha256">> => attachment_storage:locator_digest(Ns, Rec),
        <<"out_storage_physical_key">> => RawPhysicalKey,
        <<"out_attempts">> => 1
    },
    ets:insert(Tab, {claims, 0}),
    ok = meck:new(elib_pg, [non_strict, no_link]),
    %% V3-20 disk 退役：真实落盘删除改为 meck garage delete_snapshot，
    %% 记录收到的冻结身份供物理键断言。
    ok = meck:new(attachment_storage_garage, [non_strict, no_link]),
    ok = meck:expect(attachment_storage_garage, delete_snapshot, fun(I) ->
        ets:insert(Tab, {garage_identity, I}),
        ok
    end),
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
    ?assertEqual([{delete_snapshot, Logical, Physical}], ets:lookup(Tab, delete_snapshot)),
    %% 物理键≠逻辑键时删物理键：garage 收到的冻结身份 key 必须等于物理键
    %% （磁盘 enoent/保留断言随 disk 退役一并移除）。
    [{garage_identity, Got}] = ets:lookup(Tab, garage_identity),
    ?assertEqual(Physical, maps:get(key, Got)),
    ?assert(meck:validate(elib_pg)),
    ?assert(meck:validate(attachment_storage_garage)).

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

restore_registry(undefined) -> application:unset_env(imboy, attachment_storage_registry);
restore_registry({ok, Value}) -> application:set_env(imboy, attachment_storage_registry, Value).
%% V3-20 disk 退役：garage 应用 env 也纳入保存/恢复（frozen_location 校验依赖）。
restore_garage(undefined) -> application:unset_env(imboy, garage);
restore_garage({ok, Value}) -> application:set_env(imboy, garage, Value).
