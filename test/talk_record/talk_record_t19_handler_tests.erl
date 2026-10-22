%%% @doc talk_record T19 handler action 级测试（真实 DB 55433 + meck 传输边缘）。
%%%
%%% 覆盖（与 T17 handler 测试同法）：exports/exportState/download/
%%% ownership-transfers 四端点的 HTTP 语义（202/200/403/404/410/412/422/428
%%% 码位、Idempotent-Replayed、T21 冻结形状 exportState/download url、
%% 内容流 Content-Disposition、错误信封码 5553-5561）。
-module(talk_record_t19_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    State = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t19_support:seed(State),
            HandlerState = talk_record_setup_fixture:mocks(
                [elib_response, elib_param, cowboy_req], fun setup_mocks/0
            ),
            State#{handler_mocks => HandlerState}
        end,
        fun talk_record_t19_support:teardown_all/1
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
        talk_record_t19_support:teardown_all(State)
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
%%% 夹具（编号段 9x）
%%%===================================================================

-spec fixture(map(), integer()) -> map().
fixture(State, N) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t19_support:ids(),
    U1 = maps:get(u1, Ids),
    {ok, Rid} = talk_record_t15_support:create_record(U1, N, <<"t19-handler"/utf8>>),
    _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
    S1 = talk_record_t15_support:seed_speaker(Conn, Rid, <<"S1"/utf8>>, #{}),
    _ = talk_record_t15_support:seed_segment(Conn, Rid, 1, 0, S1, 0, <<"handler 正文"/utf8>>),
    #{
        record => Rid,
        conn => Conn,
        u1 => U1,
        u2 => maps:get(u2, Ids),
        u3 => maps:get(u3, Ids),
        u7 => maps:get(u7, Ids),
        ids => Ids
    }.

%%%===================================================================
%%% POST /:id/exports（202 + Idempotent-Replayed + 门控码位）
%%%===================================================================

exports_create_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 111),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        Bind = #{id => integer_to_binary(Rid)},
        mock_body(#{<<"format">> => <<"txt">>}),
        %% owner：202 + T08 冻结形状 {export_id, state, job_id}
        R1 = talk_record_handler:handle_action(
            exports_collection,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t19h-111">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(202, maps:get(response_status, R1)),
        #{<<"payload">> := P1} = jsone:decode(maps:get(response_body, R1)),
        ?assert(is_integer(maps:get(<<"export_id">>, P1))),
        ?assertEqual(<<"queued">>, maps:get(<<"state">>, P1)),
        ?assert(is_integer(maps:get(<<"job_id">>, P1))),
        %% 同键重放：202 + Idempotent-Replayed: true
        R2 = talk_record_handler:handle_action(
            exports_collection,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t19h-111">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(202, maps:get(response_status, R2)),
        ?assertEqual(<<"true">>, maps:get(<<"idempotent-replayed">>, maps:get(resp_headers, R2))),
        #{<<"payload">> := P2} = jsone:decode(maps:get(response_body, R2)),
        ?assertEqual(maps:get(<<"export_id">>, P1), maps:get(<<"export_id">>, P2))
    end).

exports_create_error_codes_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 112),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U3 = maps:get(u3, F),
        Bind = #{id => integer_to_binary(Rid)},
        talk_record_t15_support:add_grant(maps:get(conn, F), Rid, U3, U1, <<"viewer">>),
        mock_body(#{<<"format">> => <<"txt">>}),
        %% viewer 无 allow_export → 403
        R1 = talk_record_handler:handle_action(
            exports_collection,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t19h-112a">>}
            }),
            #{current_uid => U3}
        ),
        ?assertEqual(403, maps:get(response_status, R1)),
        %% include_audio=true → 422（5557）
        mock_body(#{<<"format">> => <<"txt">>, <<"include_audio">> => true}),
        R2 = talk_record_handler:handle_action(
            exports_collection,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t19h-112b">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R2)),
        ?assertEqual(5557, maps:get(<<"code">>, maps:get(response_body, R2))),
        %% 缺幂等键 → 422
        mock_body(#{<<"format">> => <<"txt">>}),
        R3 = talk_record_handler:handle_action(
            exports_collection,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R3)),
        %% 不可见记录（stranger 用户存在但无 ACL）→ 404
        R4 = talk_record_handler:handle_action(
            exports_collection,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t19h-112c">>}
            }),
            #{current_uid => maps:get(u7, maps:get(ids, F))}
        ),
        ?assertEqual(404, maps:get(response_status, R4))
    end).

%%%===================================================================
%%% GET /:id/exports/:export_id（exportState 轮询形状）+ download
%%%===================================================================

export_state_and_download_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 113),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        mock_body(#{<<"format">> => <<"txt">>}),
        R0 = talk_record_handler:handle_action(
            exports_collection,
            req(#{
                method => <<"POST">>,
                bindings => #{id => integer_to_binary(Rid)},
                headers => #{<<"idempotency-key">> => <<"t19h-113">>}
            }),
            #{current_uid => U1}
        ),
        #{<<"payload">> := R0P} = jsone:decode(maps:get(response_body, R0)),
        ExportId = maps:get(<<"export_id">>, R0P),
        Bind = #{id => integer_to_binary(Rid), export_id => integer_to_binary(ExportId)},

        ?assertEqual(<<"queued">>, maps:get(<<"state">>, R0P)),
        ?assertMatch(
            {error, export_not_ready}, talk_record_export_logic:download(U1, Rid, ExportId)
        ),
        run_export_job(U1, Rid, ExportId, maps:get(<<"job_id">>, R0P)),
        %% exportState：worker 完成后返回 200 + T21 冻结形状
        R1 = talk_record_handler:handle_action(
            export_item,
            req(#{method => <<"GET">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        S = maps:get(response_body, R1),
        ?assertEqual(ExportId, maps:get(<<"export_id">>, S)),
        ?assertEqual(<<"ready">>, maps:get(<<"state">>, S)),
        ?assertEqual(<<"txt">>, maps:get(<<"format">>, S)),
        ?assert(is_binary(maps:get(<<"expires_at">>, S))),

        %% download 开票：200 {url, ticket, expiry, expires_at}
        R2 = talk_record_handler:handle_action(
            export_download,
            req(#{method => <<"GET">>, bindings => Bind, qs => <<>>}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        D = maps:get(response_body, R2),
        ?assert(is_binary(maps:get(<<"url">>, D))),
        Ticket = maps:get(<<"ticket">>, D),

        %% 内容流：download?ticket= → 200 文本 + Content-Disposition
        R3 = talk_record_handler:handle_action(
            export_download,
            req(#{method => <<"GET">>, bindings => Bind, qs => <<"ticket=", Ticket/binary>>}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R3)),
        ?assert(binary:match(maps:get(response_body, R3), <<"handler 正文"/utf8>>) =/= nomatch),
        %% meck reply 写 response_headers（与 set_resp_header 的 resp_headers 键区分）
        Hdrs = maps:get(response_headers, R3),
        ?assert(
            binary:match(maps:get(<<"content-disposition">>, Hdrs), <<"attachment">>) =/= nomatch
        ),
        ?assertEqual(<<"text/plain; charset=utf-8">>, maps:get(<<"content-type">>, Hdrs)),

        %% 他人票据 → 403（5561）
        R4 = talk_record_handler:handle_action(
            export_download,
            req(#{method => <<"GET">>, bindings => Bind, qs => <<"ticket=", Ticket/binary>>}),
            #{current_uid => maps:get(u2, F)}
        ),
        ?assertEqual(403, maps:get(response_status, R4)),
        ?assertEqual(5561, maps:get(<<"code">>, maps:get(response_body, R4))),

        %% 到期：expires_at 置过去 → download 410（5554）+ content 清理
        talk_record_t19_support:sql(Conn, [
            "UPDATE talk_record_export SET expires_at = now() - interval '1 hour' WHERE id=",
            integer_to_binary(ExportId)
        ]),
        R5 = talk_record_handler:handle_action(
            export_download,
            req(#{method => <<"GET">>, bindings => Bind, qs => <<>>}),
            #{current_uid => U1}
        ),
        ?assertEqual(410, maps:get(response_status, R5)),
        ?assertEqual(5554, maps:get(<<"code">>, maps:get(response_body, R5))),
        ?assertEqual(null, talk_record_t19_support:export_content(Conn, ExportId))
    end).

%%%===================================================================
%%% POST /:id/ownership-transfers（org 记录；412/428/422 + 200 形状）
%%%===================================================================

ownership_transfer_handler_test_() ->
    with_db(fun(State) ->
        Conn = maps:get(conn, State),
        Ids = talk_record_t19_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {ok, Rid} = talk_record_t15_support:create_org_record(U1, maps:get(org_a, Ids), 0, 114),
        Bind = #{id => integer_to_binary(Rid)},
        Body = #{<<"target_user_id">> => U2, <<"keep_access">> => false},
        mock_body(Body),

        %% 缺 If-Match → 428
        R1 = talk_record_handler:handle_action(
            ownership_transfers,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{<<"idempotency-key">> => <<"t19h-114a">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(428, maps:get(response_status, R1)),
        %% 旧 revision → 412
        R2 = talk_record_handler:handle_action(
            ownership_transfers,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{
                    <<"idempotency-key">> => <<"t19h-114b">>,
                    <<"if-match">> => <<"record:99">>
                }
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(412, maps:get(response_status, R2)),
        %% personal 记录 → 422（5558）
        {ok, PRid} = talk_record_t15_support:create_record(U1, 115, <<"t19-handler-p"/utf8>>),
        R3 = talk_record_handler:handle_action(
            ownership_transfers,
            req(#{
                method => <<"POST">>,
                bindings => #{id => integer_to_binary(PRid)},
                headers => #{
                    <<"idempotency-key">> => <<"t19h-114c">>,
                    <<"if-match">> => <<"record:1">>
                }
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R3)),
        ?assertEqual(5558, maps:get(<<"code">>, maps:get(response_body, R3))),

        %% 成功：200 + T21 TransferOutcome 消费键
        R4 = talk_record_handler:handle_action(
            ownership_transfers,
            req(#{
                method => <<"POST">>,
                bindings => Bind,
                headers => #{
                    <<"idempotency-key">> => <<"t19h-114d">>,
                    <<"if-match">> => <<"record:1">>
                }
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R4)),
        P = maps:get(response_body, R4),
        ?assertEqual(U2, maps:get(<<"new_owner_user_id">>, P)),
        ?assertEqual(false, maps:get(<<"keep_access">>, P)),
        ?assert(is_binary(maps:get(<<"new_owner_display_name">>, P))),
        ?assertEqual(2, maps:get(<<"acl_revision">>, P)),
        %% 原 owner 失权（detail 404）；审计行存在
        ?assertMatch({error, not_found}, talk_record_ds:detail(U1, Rid)),
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"ownership_transfer">>))
    end).

%% Claim and execute the real asynchronous job; reject unrelated queued work.
run_export_job(Uid, Rid, ExportId, JobId) ->
    {ok, Job} = elib_pg:with_tx(fun(C) ->
        talk_record_job_repo:claim_next_tx(
            C, <<"t19-handler-test">>, <<"11111111-2222-4333-8444-555555555555">>, 300
        )
    end),
    ?assertEqual(JobId, maps:get(<<"id">>, Job)),
    ?assertEqual(Rid, maps:get(<<"record_id">>, Job)),
    {ok, _} = talk_record_export_job:run(Job),
    {ok, #{<<"state">> := <<"ready">>}} = talk_record_export_logic:state(
        Uid, Rid, ExportId
    ),
    ok.
