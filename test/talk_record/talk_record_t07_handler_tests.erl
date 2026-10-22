%%% @doc talk_record T07 handler action 级测试（真实 DB 55433 + meck 传输边缘）。
%%%
%%% 覆盖（与 T05 handler 测试同法）：采集五端点 + recover/abandon + marks 的
%%% HTTP 语义（201/202/200/428/409 码位、Idempotent-Replayed 头、ETag 头）。
-module(talk_record_t07_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REC1, <<"00000000-0000-0000-0000-000000050001">>).
-define(DS1, <<"60000000-0000-0000-0000-000000000001">>).
-define(DS2, <<"60000000-0000-0000-0000-000000000002">>).
-define(EV(S), ev_uuid(S)).

setup() ->
    State = talk_record_t07_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t07_support:seed(State),
            HandlerState = talk_record_setup_fixture:mocks(
                [elib_response, elib_param, cowboy_req], fun setup_mocks/0
            ),
            State#{handler_mocks => HandlerState}
        end,
        fun talk_record_t07_support:teardown_all/1
    ).

setup_mocks() ->
    ok = meck:expect(elib_response, success, fun(Req, Data) ->
        Req#{response_status => 200, response_body => Data}
    end),
    ok = meck:expect(elib_response, error_with_status, fun(Req, Status, Msg, Code) ->
        Req#{
            response_status => Status,
            response_body => #{<<"msg">> => Msg, <<"code">> => Code}
        }
    end),
    ok = meck:expect(cowboy_req, reply, fun(Status, Headers, Body, Req) ->
        Req#{
            response_status => Status,
            response_headers => maps:merge(maps:get(resp_headers, Req, #{}), Headers),
            response_body => Body
        }
    end),
    ok = meck:expect(cowboy_req, reply, fun(Status, Req) ->
        Req#{response_status => Status}
    end),
    ok.

cleanup(State) ->
    try
        talk_record_setup_fixture:unload(maps:get(handler_mocks, State))
    after
        talk_record_t07_support:teardown_all(State)
    end.

mock_body(Body) ->
    ok = meck:expect(elib_param, post, fun(_Req) -> Body end).

req(Opts) ->
    cowboy_req_h:new(Opts).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% captures 创建（428 / 422 / 201 信封）
%%%===================================================================

capture_create_preconditions_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07h-c-0">>, ?REC1),
        IdBin = integer_to_binary(Rid),
        Body = #{<<"device_session_id">> => ?DS1, <<"device_id">> => <<"t07h-dev">>},

        %% 缺 Idempotency-Key → 422（5461）
        mock_body(Body),
        R1 = talk_record_handler:handle_action(
            captures_collection,
            req(#{method => <<"POST">>, bindings => #{id => IdBin}}),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R1)),
        ?assertEqual(5461, maps:get(<<"code">>, maps:get(response_body, R1))),

        %% 缺 If-Match → 428（5525）
        mock_body(Body),
        R2 = talk_record_handler:handle_action(
            captures_collection,
            req(#{
                method => <<"POST">>,
                bindings => #{id => IdBin},
                headers => #{<<"idempotency-key">> => <<"t07h-k1">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(428, maps:get(response_status, R2)),
        ?assertEqual(5525, maps:get(<<"code">>, maps:get(response_body, R2))),

        %% 正常创建 → 201 信封 + capture_id/revision
        mock_body(Body),
        R3 = talk_record_handler:handle_action(
            captures_collection,
            req(#{
                method => <<"POST">>,
                bindings => #{id => IdBin},
                headers => #{
                    <<"idempotency-key">> => <<"t07h-k1">>,
                    <<"if-match">> => <<"\"1\"">>
                }
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(201, maps:get(response_status, R3)),
        Env = jsone:decode(maps:get(response_body, R3)),
        ?assertEqual(0, maps:get(<<"code">>, Env)),
        Payload = maps:get(<<"payload">>, Env),
        ?assert(is_integer(maps:get(<<"capture_id">>, Payload))),
        ?assertEqual(1, maps:get(<<"revision">>, Payload)),
        ?assertEqual(0, maps:get(<<"lease_epoch">>, Payload)),

        %% 重放 → 201 + Idempotent-Replayed 头
        mock_body(Body),
        R4 = talk_record_handler:handle_action(
            captures_collection,
            req(#{
                method => <<"POST">>,
                bindings => #{id => IdBin},
                headers => #{
                    <<"idempotency-key">> => <<"t07h-k1">>,
                    <<"if-match">> => <<"\"1\"">>
                }
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(201, maps:get(response_status, R4)),
        ?assertEqual(
            <<"true">>,
            maps:get(<<"idempotent-replayed">>, maps:get(resp_headers, R4))
        )
    end).

%%%===================================================================
%%% events / audio-parts / confirm（200 / 409 码位 / 202）
%%%===================================================================

event_and_part_handler_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07h-e-0">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07h-e-1">>),
        Bind = #{id => integer_to_binary(Rid), cid => integer_to_binary(Cid)},
        S = #{current_uid => U1},

        %% events → 200 + 新 revision
        mock_body(#{
            <<"client_event_id">> => ?EV(1),
            <<"device_session_id">> => ?DS1,
            <<"seq">> => 1,
            <<"event">> => <<"start">>,
            <<"position_ms">> => 0
        }),
        R1 = talk_record_handler:handle_action(
            capture_events,
            req(#{method => <<"POST">>, bindings => Bind}),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, maps:get(response_body, R1))),
        ?assertEqual(2, maps:get(<<"revision">>, maps:get(response_body, R1))),

        %% 同 seq 异 id → 409（5532）
        mock_body(#{
            <<"client_event_id">> => ?EV(99),
            <<"device_session_id">> => ?DS1,
            <<"seq">> => 1,
            <<"event">> => <<"pause">>,
            <<"position_ms">> => 0
        }),
        R2 = talk_record_handler:handle_action(
            capture_events,
            req(#{method => <<"POST">>, bindings => Bind}),
            S
        ),
        ?assertEqual(409, maps:get(response_status, R2)),
        ?assertEqual(5532, maps:get(<<"code">>, maps:get(response_body, R2))),

        %% audio-parts → 201 + durable V5 upload_ref
        Sha = talk_record_t07_support:sha(0),
        mock_body(#{
            <<"seq">> => 0,
            <<"start_ms">> => 0,
            <<"end_ms">> => 30_000,
            <<"sha256">> => Sha,
            <<"byte_size">> => 4096,
            <<"codec">> => <<"aac">>
        }),
        R3 = talk_record_handler:handle_action(
            capture_audio_parts,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t07h-p-0">>}
            }),
            S
        ),
        ?assertEqual(201, maps:get(response_status, R3)),
        PEnv = jsone:decode(maps:get(response_body, R3)),
        PP = maps:get(<<"payload">>, PEnv),
        ?assertEqual(true, maps:get(<<"gateway">>, maps:get(<<"upload_ref">>, PP))),
        ?assertEqual(null, maps:get(<<"attachment_id">>, PP)),
        ok = talk_record_upload_test_support:upload(U1, PP),
        Pid = maps:get(<<"part_id">>, PP),

        %% confirm → 200 confirmed（首次内联校验通过）
        mock_body(#{}),
        R4 = talk_record_handler:handle_action(
            capture_audio_part_confirm,
            req(#{method => <<"POST">>, bindings => Bind#{pid => integer_to_binary(Pid)}}),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R4)),
        ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, maps:get(response_body, R4)))
    end).

confirm_facts_mismatch_409_handler_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07h-v-0">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07h-v-1">>),
        {ok, created, P} = talk_record_t07_support:register_part(
            U1,
            Rid,
            Cid,
            #{
                <<"seq">> => 0,
                <<"start_ms">> => 0,
                <<"end_ms">> => 1,
                <<"sha256">> => binary:copy(<<"b">>, 64),
                <<"byte_size">> => 1024,
                <<"codec">> => <<"aac">>
            },
            <<"t07h-v-0">>
        ),
        Bind = #{
            id => integer_to_binary(Rid),
            cid => integer_to_binary(Cid),
            pid => integer_to_binary(maps:get(<<"part_id">>, P))
        },
        %% Actual uploaded bytes differ from declared hash/size: no asset binding.
        ok = talk_record_upload_test_support:upload(U1, P),
        mock_body(#{}),
        R = talk_record_handler:handle_action(
            capture_audio_part_confirm,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(409, maps:get(response_status, R)),
        ?assertEqual(5537, maps:get(<<"code">>, maps:get(response_body, R)))
    end).

%% P5 补真实 202 断言：附件行未物化（T06 集成模式）→ 202 + status=verifying
confirm_verifying_202_handler_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07h-v2-0">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07h-v2-1">>),
        {ok, created, P} = talk_record_t07_support:register_part(
            U1,
            Rid,
            Cid,
            #{
                <<"seq">> => 0,
                <<"start_ms">> => 0,
                <<"end_ms">> => 1000,
                <<"sha256">> => binary:copy(<<"c">>, 64),
                <<"byte_size">> => 512,
                <<"codec">> => <<"aac">>
            },
            <<"t07h-v2-0">>
        ),
        Pid = maps:get(<<"part_id">>, P),
        talk_record_t07_support:sql(C, "DELETE FROM attach_pending"),
        mock_body(#{}),
        R = talk_record_handler:handle_action(
            capture_audio_part_confirm,
            req(#{
                method => <<"POST">>,
                bindings => #{
                    id => integer_to_binary(Rid),
                    cid => integer_to_binary(Cid),
                    pid => integer_to_binary(Pid)
                }
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(202, maps:get(response_status, R)),
        Env = jsone:decode(maps:get(response_body, R)),
        ?assertEqual(0, maps:get(<<"code">>, Env)),
        ?assertEqual(
            <<"verifying">>,
            maps:get(<<"status">>, maps:get(<<"payload">>, Env))
        )
    end).

recover_abandon_handler_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07h-ra-0">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07h-ra-1">>),
        Bind = #{id => integer_to_binary(Rid), cid => integer_to_binary(Cid)},
        S = #{current_uid => U1},

        %% recover 异设备 → 409（5538）
        mock_body(#{<<"device_session_id">> => ?DS2}),
        R1 = talk_record_handler:handle_action(
            capture_recover,
            req(#{method => <<"POST">>, bindings => Bind}),
            S
        ),
        ?assertEqual(409, maps:get(response_status, R1)),
        ?assertEqual(5538, maps:get(<<"code">>, maps:get(response_body, R1))),

        %% recover 原会话 → 200 recovered
        mock_body(#{<<"device_session_id">> => ?DS1}),
        R2 = talk_record_handler:handle_action(
            capture_recover,
            req(#{method => <<"POST">>, bindings => Bind}),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        ?assertEqual(true, maps:get(<<"recovered">>, maps:get(response_body, R2))),

        %% abandon 缺 confirm → 422
        mock_body(#{}),
        R3 = talk_record_handler:handle_action(
            capture_abandon,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            S
        ),
        ?assertEqual(422, maps:get(response_status, R3)),

        %% abandon 正确 → 200 + epoch=1
        mock_body(#{<<"confirm">> => true}),
        R4 = talk_record_handler:handle_action(
            capture_abandon,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"\"2\"">>}
            }),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R4)),
        ?assertEqual(1, maps:get(<<"lease_epoch">>, maps:get(response_body, R4)))
    end).

%%%===================================================================
%%% marks（201 / PATCH 200+ETag 头 / DELETE 200 / 404）
%%%===================================================================

marks_handler_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07h-m-0">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07h-m-1">>),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?EV(1),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 1,
                <<"event">> => <<"start">>,
                <<"position_ms">> => 10_000
            },
            #{}
        ),
        RecBind = #{id => integer_to_binary(Rid)},
        S = #{current_uid => U1},

        %% POST marks → 201
        mock_body(#{
            <<"client_mark_id">> => ?EV(50),
            <<"position_ms">> => 3_000,
            <<"text">> => <<"重点"/utf8>>
        }),
        R1 = talk_record_handler:handle_action(
            marks_collection,
            req(#{method => <<"POST">>, bindings => RecBind}),
            S
        ),
        ?assertEqual(201, maps:get(response_status, R1)),
        MEnv = jsone:decode(maps:get(response_body, R1)),
        MP = maps:get(<<"payload">>, MEnv),
        Mid = maps:get(<<"id">>, MP),
        ?assertEqual(Cid, maps:get(<<"capture_id">>, MP)),
        ?assertEqual(3_000, maps:get(<<"capture_offset_ms">>, MP)),

        %% GET marks → 200 清单
        R2 = talk_record_handler:handle_action(
            marks_collection,
            req(#{method => <<"GET">>, bindings => RecBind}),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        ?assertEqual(1, length(maps:get(<<"marks">>, maps:get(response_body, R2)))),

        %% PATCH → 200 + ETag 头
        mock_body(#{<<"text">> => <<"改"/utf8>>}),
        R3 = talk_record_handler:handle_action(
            marks_item,
            req(#{
                method => <<"PATCH">>,
                bindings => RecBind#{mark_id => integer_to_binary(Mid)},
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R3)),
        ?assertEqual(2, maps:get(<<"revision">>, maps:get(response_body, R3))),
        ?assertEqual(<<"\"2\"">>, maps:get(<<"etag">>, maps:get(resp_headers, R3))),

        %% DELETE → 200；重复 → 404
        R4 = talk_record_handler:handle_action(
            marks_item,
            req(#{
                method => <<"DELETE">>,
                bindings => RecBind#{mark_id => integer_to_binary(Mid)},
                headers => #{<<"if-match">> => <<"\"2\"">>}
            }),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R4)),
        ?assertEqual(true, maps:get(<<"deleted">>, maps:get(response_body, R4))),
        R5 = talk_record_handler:handle_action(
            marks_item,
            req(#{
                method => <<"DELETE">>,
                bindings => RecBind#{mark_id => integer_to_binary(Mid)},
                headers => #{<<"if-match">> => <<"\"3\"">>}
            }),
            S
        ),
        ?assertEqual(404, maps:get(response_status, R5)),

        %% 无权主体（陌生人）→ 404
        mock_body(#{<<"client_mark_id">> => ?EV(51), <<"position_ms">> => 1}),
        R6 = talk_record_handler:handle_action(
            marks_collection,
            req(#{method => <<"POST">>, bindings => RecBind}),
            #{current_uid => maps:get(u7, Ids)}
        ),
        ?assertEqual(404, maps:get(response_status, R6))
    end).

method_not_allowed_test_() ->
    with_db(fun(_State) ->
        R = talk_record_handler:handle_action(
            captures_collection,
            cowboy_req_h:new(#{method => <<"DELETE">>}),
            #{}
        ),
        ?assertEqual(405, maps:get(response_status, R))
    end).

-spec ev_uuid(integer()) -> binary().
ev_uuid(Seq) ->
    Hex = integer_to_binary(Seq, 16),
    PadLen = max(0, 12 - byte_size(Hex)),
    Pad = <<<<$0>> || _ <- lists:seq(1, PadLen)>>,
    <<"70000000-0000-0000-0000-", Pad/binary, Hex/binary>>.
