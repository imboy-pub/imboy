-module(attachment_lifecycle_v5_w601_w2_resume_surface_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, attachment_lifecycle_v5_w601_w2_support).
-define(F, attachment_w403_http_fixture).
-define(C, attachment_lifecycle_v5_w601_w2_confirm_tests).
-define(BODY, <<"w601 confirmed immutable content">>).
-define(TAB, w601_resume_domain_test).

resume_domain_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(resume_surface(Ep))
         || Ep <-
                [ordinary, enterprise_internal, enterprise_seat, cs_widget]
        ] ++
            [
                ?_test(provider_bypass_is_rejected()),
                ?_test(replay_changed_id_is_rejected()),
                ?_test(replay_added_reference_is_rejected()),
                ?_test(wrong_response_field_is_rejected()),
                ?_test(missing_object_message_binding_is_rejected())
            ]
    end}.

setup() ->
    ets:new(?TAB, [named_table, public, set]),
    meck:new([?S, ?F, elib_pg], [passthrough, no_link]),
    meck:expect(?F, register, fun(S, Ep, Driver, _) ->
        ets:insert(?TAB, {surface, Ep}),
        #{
            fixture => S#{scope => #{org_id => 11, workspace_id => 22}},
            surface => Ep,
            driver => Driver,
            key => <<"key">>,
            registration => #{<<"asset_id">> => 33}
        }
    end),
    meck:expect(?F, put, fun(_, _) ->
        case ets:update_counter(?TAB, put, 1, {put, 0}) of
            1 ->
                case ets:lookup(?TAB, bypass) of
                    [] ->
                        try
                            attachment_storage_disk:put_snapshot(#{key => <<"key">>}, a, b, c)
                        catch
                            error:pin_committed_then_crash -> #{status => 500}
                        end;
                    _ ->
                        #{status => 500}
                end;
            2 ->
                #{status => 200}
        end
    end),
    meck:expect(?F, payload, fun(Resp) -> maps:get(payload, Resp) end),
    meck:expect(?F, success, fun(#{status := Status} = Resp) ->
        Status =:= 200 andalso not maps:is_key(body, Resp)
    end),
    meck:expect(?F, confirm, fun(#{surface := Ep}) ->
        case ets:update_counter(?TAB, confirms, 1, {confirms, 0}) of
            1 ->
                missing_response(Ep);
            N when N >= 2 ->
                Id =
                    case {N, ets:lookup(?TAB, changed_id)} of
                        {3, [_]} -> 34;
                        _ -> 33
                    end,
                Field =
                    case {Ep, ets:lookup(?TAB, wrong_field)} of
                        {enterprise_seat, [_]} -> <<"file_id">>;
                        {ordinary, _} -> <<"attachment_id">>;
                        {enterprise_internal, _} -> <<"file_id">>;
                        {_, _} -> <<"asset_id">>
                    end,
                #{status => 200, payload => #{Field => Id}}
        end
    end),
    meck:expect(?F, read, fun(_) ->
        case ets:update_counter(?TAB, read, 1, {read, 0}) of
            1 -> {error, not_found};
            2 -> {ok, ?BODY}
        end
    end),
    meck:expect(?S, content_pin_any, fun(Ep, _) ->
        ets:insert(?TAB, {pin_surface, Ep}),
        {ok, #{<<"sha256">> => ?S:sha(?BODY)}}
    end),
    meck:expect(?S, attach_count, fun(_) ->
        case {ets:lookup(?TAB, confirms), ets:lookup(?TAB, surface)} of
            {[{confirms, N}], [{surface, Ep}]} when
                N >= 2,
                (Ep =:= ordinary orelse Ep =:= enterprise_internal)
            ->
                1;
            _ ->
                0
        end
    end),
    meck:expect(?S, pending_row, fun(_) ->
        State =
            case ets:lookup(?TAB, confirms) of
                [{confirms, N}] when N >= 2 -> <<"confirmed">>;
                _ -> <<"registered">>
            end,
        {ok, identity(#{<<"session_state">> => State})}
    end),
    meck:expect(?S, eb_row, fun(11, 22, 33) ->
        State =
            case ets:lookup(?TAB, confirms) of
                [{confirms, N}] when N >= 2 -> <<"active">>;
                _ -> <<"pending_confirm">>
            end,
        {ok, identity(#{<<"status">> => State})}
    end),
    meck:expect(elib_pg, query, fun(Sql, Params) ->
        ets:insert(?TAB, {query, Sql, Params}),
        case
            {Params, binary:match(Sql, <<"referer_time">>),
                binary:match(Sql, <<"conversation_id">>)}
        of
            {[<<"key">>], {_, _}, _} ->
                Ref =
                    case {ets:lookup(?TAB, confirms), ets:lookup(?TAB, added_ref)} of
                        {[{confirms, 3}], [_]} -> 2;
                        _ -> 1
                    end,
                {ok, [#{<<"id">> => 33, <<"referer_time">> => Ref}]};
            {[11, 22, <<"key">>], _, {_, _}} ->
                {ok, [#{<<"id">> => 33, <<"conversation_id">> => null, <<"message_id">> => null}]};
            {[11, 22, 33], _, _} ->
                Message =
                    case {ets:lookup(?TAB, bad_binding), ets:lookup(?TAB, confirms)} of
                        {[_], [{confirms, N}]} when N >= 1 -> 77;
                        _ -> null
                    end,
                {ok, [#{<<"message_id">> => Message}]};
            {[<<"key">>], _, _} ->
                {ok, [#{<<"file_hash256">> => ?S:sha(?BODY)}]};
            {[11, 22, 33, <<"key">>], _, _} ->
                {ok, [#{<<"object_hash">> => ?S:sha(?BODY)}]}
        end
    end),
    ok.

cleanup(_) ->
    meck:unload([?S, ?F, elib_pg]),
    ets:delete(?TAB),
    ok.

identity(Row) ->
    Row#{
        <<"object_key">> => <<"key">>,
        <<"storage_driver">> => <<"disk">>,
        <<"storage_namespace">> => <<"run">>,
        <<"storage_container">> => <<"bucket">>,
        <<"storage_locator_sha256">> => <<"locator">>
    }.

missing_response(ordinary) ->
    #{
        status => 200,
        body => jsone:encode(#{
            <<"code">> => 400,
            <<"msg">> => <<"对象不存在或未完成上传"/utf8>>
        })
    };
missing_response(enterprise_internal) ->
    #{
        status => 400,
        body => jsone:encode(#{<<"error">> => #{<<"code">> => <<"invalid_request">>}})
    };
missing_response(_) ->
    #{
        status => 500,
        body => jsone:encode(#{
            <<"code">> => 500,
            <<"msg">> => <<"object_unreadable.object_store.not_found">>
        })
    }.

provider_bypass_is_rejected() ->
    ets:delete_all_objects(?TAB),
    ets:insert(?TAB, {bypass, true}),
    ?assertError(
        expected_provider_fault_not_observed,
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            ordinary,
            disk,
            <<"crash_after_pin_before_put_resume">>
        )
    ).

replay_changed_id_is_rejected() ->
    ets:delete_all_objects(?TAB),
    ets:insert(?TAB, {changed_id, true}),
    ?assertError(
        {assertEqual, _},
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            ordinary,
            disk,
            <<"crash_after_pin_before_put_resume">>
        )
    ).

replay_added_reference_is_rejected() ->
    ets:delete_all_objects(?TAB),
    ets:insert(?TAB, {added_ref, true}),
    ?assertException(
        error,
        {badmatch, _},
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            ordinary,
            disk,
            <<"crash_after_pin_before_put_resume">>
        )
    ).

wrong_response_field_is_rejected() ->
    ets:delete_all_objects(?TAB),
    ets:insert(?TAB, {wrong_field, true}),
    ?assertError(
        {badkey, <<"asset_id">>},
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            enterprise_seat,
            disk,
            <<"crash_after_pin_before_put_resume">>
        )
    ).

missing_object_message_binding_is_rejected() ->
    ets:delete_all_objects(?TAB),
    ets:insert(?TAB, {bad_binding, true}),
    ?assertException(
        error,
        {badmatch, _},
        ?C:imm_scenario(
            #{leaf => <<"test">>},
            enterprise_seat,
            disk,
            <<"crash_after_pin_before_put_resume">>
        )
    ),
    ?assertEqual([{confirms, 1}], ets:lookup(?TAB, confirms)).

resume_surface(Ep) ->
    ets:delete_all_objects(?TAB),
    Result = ?C:imm_scenario(
        #{leaf => <<"test">>},
        Ep,
        disk,
        <<"crash_after_pin_before_put_resume">>
    ),
    ?assertEqual(true, maps:get(resumed_same_receipt, Result)),
    ?assertEqual(?S:sha(?BODY), maps:get(stored_hash, Result)),
    ?assertEqual([{pin_surface, Ep}], ets:lookup(?TAB, pin_surface)),
    [{query, Sql, Params}] = ets:lookup(?TAB, query),
    case Ep of
        ordinary ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM attachment">>)),
            ?assertEqual([<<"key">>], Params);
        enterprise_internal ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM attachment">>)),
            ?assertEqual([<<"key">>], Params);
        _ ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM enterprise_asset">>)),
            ?assertEqual([11, 22, 33, <<"key">>], Params)
    end.
