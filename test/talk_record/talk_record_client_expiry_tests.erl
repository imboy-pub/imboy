-module(talk_record_client_expiry_tests).
-include_lib("eunit/include/eunit.hrl").

expired_dispatch_is_unknown_test() ->
    check_expiry(<<"dispatching">>, <<"provider_outcome_unknown">>).
expired_confirmed_is_safe_expired_test() -> check_expiry(<<"confirmed">>, <<"expired">>).
expired_unstarted_is_safe_expired_test() -> check_expiry(<<"not_dispatched">>, <<"expired">>).

check_expiry(PartState, Expected) ->
    meck:new(elib_pg, [passthrough, no_link]),
    TestPid = self(),
    try
        meck:expect(elib_pg, query, fun(_, Sql, _) ->
            %% 全量负载下后台 worker 偶发打入 elib_pg 属环境噪声：
            %% 直通返回，仅对被测进程断言。
            case self() =:= TestPid of
                true ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"talk_record_client_run_part">>));
                false ->
                    ok
            end,
            {ok, [#{<<"provider_state">> => PartState}]}
        end),
        meck:expect(elib_pg, execute, fun(_, Sql, Args) ->
            case self() =:= TestPid of
                true ->
                    case binary:match(Sql, <<"UPDATE talk_record_client_run SET">>) of
                        nomatch -> ok;
                        _ -> ?assertEqual(Expected, lists:nth(2, Args))
                    end,
                    ?assertEqual(nomatch, binary:match(Sql, <<"state='queued'">>));
                false ->
                    ok
            end,
            {ok, 1}
        end),
        ?assertEqual(
            {expired, Expected},
            talk_record_client_repo:expire_tx(
                conn,
                #{<<"id">> => 8, <<"state">> => <<"active">>, <<"lease_valid">> => false}
            )
        ),
        %% validate 的被测路径等价（按调用进程过滤历史）：本进程对 elib_pg
        %% 的调用仅允许 query/3 与 execute/3，且 execute 恰 3 次。
        Own = meck:history(elib_pg, TestPid),
        ?assertEqual([execute, query], lists:usort([F || {_, {_M, F, _A}, _R} <- Own])),
        ?assertEqual(3, length([1 || {_, {_, execute, _}, _} <- Own]))
    after
        meck:unload(elib_pg)
    end.

current_or_terminal_never_reaped_test() ->
    ?assertEqual(
        current,
        talk_record_client_repo:expire_tx(
            no_db,
            #{<<"id">> => 8, <<"state">> => <<"active">>, <<"lease_valid">> => true}
        )
    ),
    ?assertEqual(
        current,
        talk_record_client_repo:expire_tx(
            no_db,
            #{
                <<"id">> => 8,
                <<"state">> => <<"provider_outcome_unknown">>,
                <<"lease_valid">> => false
            }
        )
    ).

publish_chunk_speakers_stay_scoped_test() ->
    R = #{
        <<"speakers">> => [#{<<"label">> => <<"speaker1">>}],
        <<"segments">> => [
            #{
                <<"speaker_label">> => <<"speaker1">>,
                <<"start_ms">> => 0,
                <<"end_ms">> => 100,
                <<"text">> => <<"hi">>
            }
        ]
    },
    A = #{<<"seq">> => 0, <<"offset_ms">> => 0, <<"result">> => R},
    B = #{<<"seq">> => 1, <<"offset_ms">> => 1000, <<"result">> => R},
    #{speakers := Speakers, segments := Segments} = talk_record_client_publish:transcript(
        [A, B], <<"asr-1.0">>
    ),
    ?assertEqual([<<"0:speaker1">>, <<"1:speaker1">>], [maps:get(<<"label">>, S) || S <- Speakers]),
    ?assertEqual([0, 1000], [maps:get(<<"start_ms">>, S) || S <- Segments]).
