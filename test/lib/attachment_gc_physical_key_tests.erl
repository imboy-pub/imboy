%% No PostgreSQL: SQL-aware projection mock plus an injected delete boundary.
%% V3-20：disk 驱动退役，物理删除边界由测试桩注入（ack/SQL 投影/physical key
%% 选择逻辑保持原断言形态），真实 provider 删除链由 garage 车道套件验收。
-module(attachment_gc_physical_key_tests).
-include_lib("eunit/include/eunit.hrl").

physical_key_override_delete_test_() ->
    {timeout, 30, fun() -> with_fixture(<<"physical/winner.aac">>) end}.
legacy_null_physical_key_delete_test_() ->
    {timeout, 30, fun() -> with_fixture(null) end}.

with_fixture(RawPhysicalKey) ->
    undefined = whereis(elib_pg_meck),
    Previous = application:get_env(imboy, attachment_storage_registry),
    PreviousGarage = application:get_env(imboy, garage),
    Tab = ets:new(physical_key_test, [public]),
    try
        run_case(Tab, RawPhysicalKey)
    after
        try
            meck:unload(elib_pg)
        catch
            _:_ -> ok
        end,
        restore_registry(Previous),
        restore_garage(PreviousGarage),
        ets:delete(Tab)
    end.

run_case(Tab, RawPhysicalKey) ->
    Ns = <<"physical-key-unit">>,
    Container = <<"capture">>,
    Logical = <<"logical/reference.aac">>,
    Physical =
        case RawPhysicalKey of
            null -> Logical;
            _ -> RawPhysicalKey
        end,
    %% garage env 与 registry 根同 endpoint/container：worker 的 frozen_location
    %% garage 分支校验签名一致性，缺此 env 会 location_unverified → AckOk=false
    %% → 与桩的 done 裁决冲突（ack_rejected）。
    GarageConfig = #{
        endpoint => <<"https://physical-key-unit.invalid">>,
        bucket => Container,
        access_key => <<"physical-key-access">>,
        secret_key => <<"physical-key-secret">>
    },
    ok = application:set_env(imboy, garage, GarageConfig),
    ok = application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            physical_key_unit => #{
                driver => <<"garage">>,
                namespace => Ns,
                container => Container,
                endpoint => <<"https://physical-key-unit.invalid">>
            }
        }
    }),
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
            %% 注入桩：模拟物理删除成功（V3-20 后不直连已退役 disk 驱动）。
            ok
        end
    ),
    ?assertEqual([{ack, Logical, true}], ets:lookup(Tab, ack)),
    ?assertEqual([{delete_snapshot, Logical, Physical}], ets:lookup(Tab, delete_snapshot)),
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

restore_registry(undefined) -> application:unset_env(imboy, attachment_storage_registry);
restore_registry({ok, Value}) -> application:set_env(imboy, attachment_storage_registry, Value).

restore_garage(undefined) -> ok = application:unset_env(imboy, garage);
restore_garage({ok, V}) -> ok = application:set_env(imboy, garage, V).
