%%% @doc talk_record T15 handler action 级测试（真实 DB 55433 + meck 传输边缘）。
%%%
%%% 覆盖（与 T05/T07 handler 测试同法）：segments/speakers/summary 端点的
%%% HTTP 语义（200/403/404/409/412/422/428 码位、ETag 头与 payload 双形态、
%%% 错误信封码 5543-5546）。
-module(talk_record_t15_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    State = talk_record_t15_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t15_support:seed(State),
            HandlerState = talk_record_setup_fixture:mocks(
                [elib_response, elib_param, cowboy_req], fun setup_mocks/0
            ),
            State#{handler_mocks => HandlerState}
        end,
        fun talk_record_t15_support:teardown_all/1
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
        talk_record_t15_support:teardown_all(State)
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
%%% 夹具（与 content 测试同构，编号独立段 3x）
%%%===================================================================

-spec fixture(map(), integer()) -> map().
fixture(State, N) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t15_support:ids(),
    U1 = maps:get(u1, Ids),
    U2 = maps:get(u2, Ids),
    U3 = maps:get(u3, Ids),
    {ok, Rid} = talk_record_t15_support:create_record(U1, N, <<"t15-handler"/utf8>>),
    talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
    talk_record_t15_support:add_grant(Conn, Rid, U3, U1, <<"editor">>),
    _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
    Sp1 = talk_record_t15_support:seed_speaker(Conn, Rid, <<"SPEAKER_00">>, #{}),
    S1 = talk_record_t15_support:seed_segment(
        Conn,
        Rid,
        1,
        0,
        Sp1,
        0,
        <<"接口层测试原文"/utf8>>
    ),
    S2 = talk_record_t15_support:seed_segment(
        Conn,
        Rid,
        1,
        1,
        Sp1,
        5000,
        <<"接口层测试第二段"/utf8>>
    ),
    _SumId = talk_record_t15_support:seed_summary(Conn, Rid, 1, #{
        overview => <<"接口层纪要"/utf8>>
    }),
    #{
        record => Rid,
        conn => Conn,
        u1 => U1,
        u2 => U2,
        u3 => U3,
        sp1 => Sp1,
        segs => [S1, S2],
        ids => Ids
    }.

%%%===================================================================
%%% GET /:id/segments（qs 解析 + q 高亮 + 404）
%%%===================================================================

segments_list_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 31),
        Rid = maps:get(record, F),
        IdBin = integer_to_binary(Rid),

        %% owner：q 命中高亮 + speaker 过滤（真 parse_qs）
        R1 = talk_record_handler:handle_action(
            segments_collection,
            req(#{
                method => <<"GET">>,
                bindings => #{id => IdBin},
                qs => <<"q=%E6%8E%A5%E5%8F%A3%E5%B1%82&limit=20">>
            }),
            #{current_uid => maps:get(u1, F)}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        Body1 = maps:get(response_body, R1),
        ?assertEqual(2, length(maps:get(<<"segments">>, Body1))),
        [First | _] = maps:get(<<"segments">>, Body1),
        %% q=「接口层」3 codepoints → 高亮 [0,3)
        ?assertEqual(
            [#{<<"start">> => 0, <<"end">> => 3}],
            maps:get(<<"highlights">>, First)
        ),

        %% viewer 可读；陌生人 404
        R2 = talk_record_handler:handle_action(
            segments_collection,
            req(#{method => <<"GET">>, bindings => #{id => IdBin}, qs => <<>>}),
            #{current_uid => maps:get(u2, F)}
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        R3 = talk_record_handler:handle_action(
            segments_collection,
            req(#{method => <<"GET">>, bindings => #{id => IdBin}, qs => <<>>}),
            #{current_uid => 700007}
        ),
        ?assertEqual(404, maps:get(response_status, R3)),

        %% 非法 speaker_id → 422
        R4 = talk_record_handler:handle_action(
            segments_collection,
            req(#{
                method => <<"GET">>,
                bindings => #{id => IdBin},
                qs => <<"speaker_id=abc">>
            }),
            #{current_uid => maps:get(u1, F)}
        ),
        ?assertEqual(422, maps:get(response_status, R4)),

        %% 405
        R5 = talk_record_handler:handle_action(
            segments_collection,
            req(#{method => <<"POST">>, bindings => #{id => IdBin}, qs => <<>>}),
            #{current_uid => maps:get(u1, F)}
        ),
        ?assertEqual(405, maps:get(response_status, R5))
    end).

%%%===================================================================
%%% PATCH /:id/segments/:segment_id（428/412/200 + ETag 双形态）
%%%===================================================================

segment_patch_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 32),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),
        Bind = #{id => integer_to_binary(Rid), segment_id => integer_to_binary(S1)},
        S = #{current_uid => maps:get(u3, F)},
        Token = <<"segment:", (integer_to_binary(S1))/binary, ":1">>,

        %% 缺 If-Match → 428（5525）
        mock_body(#{<<"text">> => <<"无版本头"/utf8>>}),
        R1 = talk_record_handler:handle_action(
            segments_item,
            req(#{method => <<"PATCH">>, bindings => Bind}),
            S
        ),
        ?assertEqual(428, maps:get(response_status, R1)),
        ?assertEqual(5525, maps:get(<<"code">>, maps:get(response_body, R1))),

        %% editor 用 T16 令牌形态 If-Match → 200 + ETag 头（引号）与 payload（令牌）
        mock_body(#{<<"text">> => <<"人工修订稿"/utf8>>}),
        R2 = talk_record_handler:handle_action(
            segments_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => Token}
            }),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        Payload = maps:get(response_body, R2),
        Seg = maps:get(<<"segment">>, Payload),
        ?assertEqual(2, maps:get(<<"revision">>, Seg)),
        ?assertEqual(
            <<"segment:", (integer_to_binary(S1))/binary, ":2">>,
            maps:get(<<"etag">>, Seg)
        ),
        ?assertEqual(
            <<"\"segment:", (integer_to_binary(S1))/binary, ":2\"">>,
            maps:get(<<"etag">>, maps:get(resp_headers, R2))
        ),

        %% 持旧令牌再改 → 412（5526）
        mock_body(#{<<"text">> => <<"并发旧稿"/utf8>>}),
        R3 = talk_record_handler:handle_action(
            segments_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"\"", Token/binary, "\"">>}
            }),
            S
        ),
        ?assertEqual(412, maps:get(response_status, R3)),
        ?assertEqual(5526, maps:get(<<"code">>, maps:get(response_body, R3))),

        %% viewer → 403
        mock_body(#{<<"text">> => <<"读者改写"/utf8>>}),
        R4 = talk_record_handler:handle_action(
            segments_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"\"2\"">>}
            }),
            #{current_uid => maps:get(u2, F)}
        ),
        ?assertEqual(403, maps:get(response_status, R4)),

        %% 跨 record segment → 404
        mock_body(#{<<"text">> => <<"越权"/utf8>>}),
        R5 = talk_record_handler:handle_action(
            segments_item,
            req(#{
                method => <<"PATCH">>,
                bindings => #{
                    id => integer_to_binary(Rid),
                    segment_id => <<"9999999">>
                },
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            S
        ),
        ?assertEqual(404, maps:get(response_status, R5))
    end).

%%%===================================================================
%%% speakers GET/PATCH/claim（403/409/422 码位）
%%%===================================================================

speakers_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 33),
        Conn = maps:get(conn, F),
        Rid = maps:get(record, F),
        Sp1 = maps:get(sp1, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        Bind = #{id => integer_to_binary(Rid), speaker_id => integer_to_binary(Sp1)},

        %% GET：200 + 清单
        R1 = talk_record_handler:handle_action(
            speakers_collection,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        ?assertEqual(1, length(maps:get(<<"speakers">>, maps:get(response_body, R1)))),

        %% viewer PATCH → 403
        mock_body(#{<<"display_name">> => <<"读者命名"/utf8>>}),
        R2 = talk_record_handler:handle_action(
            speakers_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            #{current_uid => U2}
        ),
        ?assertEqual(403, maps:get(response_status, R2)),

        %% owner 目录外标记（ORG_B 用户，org 记录目录=同 org 成员）→ 422（5545）
        Ids = maps:get(ids, F),
        {ok, OrgRid} = talk_record_t15_support:create_org_record(
            U1,
            maps:get(org_a, Ids),
            0,
            36
        ),
        SpOrg = talk_record_t15_support:seed_speaker(Conn, OrgRid, <<"SPEAKER_00">>, #{}),
        mock_body(#{<<"matched_user_id">> => maps:get(u4, Ids)}),
        R3 = talk_record_handler:handle_action(
            speakers_item,
            req(#{
                method => <<"PATCH">>,
                bindings => #{
                    id => integer_to_binary(OrgRid),
                    speaker_id => integer_to_binary(SpOrg)
                },
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R3)),
        ?assertEqual(5545, maps:get(<<"code">>, maps:get(response_body, R3))),

        %% claim：非参与者 → 403；入参与者后 owner 手工绑定他人 → claim 409（5544）
        R4 = talk_record_handler:handle_action(
            speaker_claim,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U2}
        ),
        ?assertEqual(403, maps:get(response_status, R4)),

        talk_record_t15_support:add_participant(Conn, Rid, U2, <<"在场"/utf8>>),
        mock_body(#{<<"matched_user_id">> => maps:get(u3, Ids)}),
        R5 = talk_record_handler:handle_action(
            speakers_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R5)),
        R6 = talk_record_handler:handle_action(
            speaker_claim,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U2}
        ),
        ?assertEqual(409, maps:get(response_status, R6)),
        ?assertEqual(5544, maps:get(<<"code">>, maps:get(response_body, R6)))
    end).

%%%===================================================================
%%% GET/PATCH /:id/summary（未生成 200 状态对象 + 确认 409 码位 + ETag 头）
%%%===================================================================

summary_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 34),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        Bind = #{id => integer_to_binary(Rid)},

        %% GET：draft + ETag 头（summary:1 引号形态）
        R1 = talk_record_handler:handle_action(
            summary_item,
            req(#{method => <<"GET">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        ?assertEqual(<<"draft">>, maps:get(<<"status">>, maps:get(response_body, R1))),
        ?assertEqual(
            <<"\"summary:1\"">>,
            maps:get(<<"etag">>, maps:get(resp_headers, R1))
        ),

        %% PATCH 确认（源一致）→ 200 confirmed
        mock_body(#{<<"confirm">> => true}),
        R2 = talk_record_handler:handle_action(
            summary_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"summary:1">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        ?assertEqual(
            <<"confirmed">>,
            maps:get(<<"status">>, maps:get(<<"summary">>, maps:get(response_body, R2)))
        ),

        %% 改原文（stale）后重确认 → 409（5543）
        [S1 | _] = maps:get(segs, F),
        mock_body(#{<<"text">> => <<"改稿"/utf8>>}),
        _R3 = talk_record_handler:handle_action(
            segments_item,
            req(#{
                method => <<"PATCH">>,
                bindings => #{
                    id => integer_to_binary(Rid),
                    segment_id => integer_to_binary(S1)
                },
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            #{current_uid => U1}
        ),
        mock_body(#{<<"confirm">> => true}),
        R4 = talk_record_handler:handle_action(
            summary_item,
            req(#{
                method => <<"PATCH">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"summary:2">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(409, maps:get(response_status, R4)),
        ?assertEqual(5543, maps:get(<<"code">>, maps:get(response_body, R4))),

        %% 未生成摘要的记录：GET 返回 200 状态对象（不伪装 404）
        {ok, Plain} = talk_record_t15_support:create_record(U1, 35, <<"no-summary"/utf8>>),
        R5 = talk_record_handler:handle_action(
            summary_item,
            req(#{
                method => <<"GET">>,
                bindings => #{id => integer_to_binary(Plain)}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R5)),
        ?assertEqual(
            <<"not_generated">>,
            maps:get(<<"status">>, maps:get(response_body, R5))
        )
    end).
