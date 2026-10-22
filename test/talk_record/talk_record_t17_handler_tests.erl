%%% @doc talk_record T17 handler action 级测试（真实 DB 55433 + meck 传输边缘）。
%%%
%%% 覆盖（与 T15 handler 测试同法）：grants/share-card/playback/
%%% share-candidates 端点的 HTTP 语义（200/403/404/409/412/422/428 码位、
%% acl ETag 双形态、T18 putGrants 请求体形状、T16/T08 playback 响应字段、
%%% 错误信封码 5545/5547-5549）。
-module(talk_record_t17_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    State = talk_record_t17_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t17_support:seed(State),
            HandlerState = talk_record_setup_fixture:mocks(
                [elib_response, elib_param, cowboy_req], fun setup_mocks/0
            ),
            State#{handler_mocks => HandlerState}
        end,
        fun talk_record_t17_support:teardown_all/1
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
        talk_record_t17_support:teardown_all(State)
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
%%% 夹具（编号段 6x）
%%%===================================================================

-spec fixture(map(), integer()) -> map().
fixture(State, N) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t17_support:ids(),
    U1 = maps:get(u1, Ids),
    U2 = maps:get(u2, Ids),
    U3 = maps:get(u3, Ids),
    {ok, Rid} = talk_record_t15_support:create_record(U1, N, <<"t17-handler"/utf8>>),
    _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
    talk_record_t17_support:sql(Conn, [
        "UPDATE talk_record SET duration_ms = 60000 WHERE id = ", integer_to_binary(Rid)
    ]),
    #{record => Rid, conn => Conn, u1 => U1, u2 => U2, u3 => U3, ids => Ids}.

%%%===================================================================
%%% GET/PUT /:id/grants（ETag 双形态 + T18 putGrants 形状）
%%%===================================================================

grants_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 61),
        Rid = maps:get(record, F),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        Bind = #{id => integer_to_binary(Rid)},

        %% GET：200 + ETag 头（acl:1 引号形态）与 payload 令牌本体
        R1 = talk_record_handler:handle_action(
            grants_collection,
            req(#{method => <<"GET">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        ?assertEqual(
            <<"acl:1">>,
            maps:get(<<"etag">>, maps:get(response_body, R1))
        ),
        ?assertEqual(
            <<"\"acl:1\"">>,
            maps:get(<<"etag">>, maps:get(resp_headers, R1))
        ),

        %% PUT（T18 putGrants 形状 {grants: [...]}）：200 + acl:2 双形态
        mock_body(#{
            <<"grants">> => [
                #{
                    <<"grantee_user_id">> => U2,
                    <<"role">> => <<"viewer">>,
                    <<"allow_export">> => true,
                    <<"expires_at">> => <<"2099-01-01T00:00:00Z">>
                }
            ]
        }),
        R2 = talk_record_handler:handle_action(
            grants_collection,
            req(#{
                method => <<"PUT">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:1">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        ?assertEqual(
            <<"acl:2">>,
            maps:get(<<"etag">>, maps:get(response_body, R2))
        ),
        ?assertEqual(
            <<"\"acl:2\"">>,
            maps:get(<<"etag">>, maps:get(resp_headers, R2))
        ),
        ?assertEqual(1, length(maps:get(<<"list">>, maps:get(response_body, R2)))),

        %% PUT 缺 If-Match → 428（5525）；旧 ETag → 412（5526）
        mock_body(#{<<"grants">> => []}),
        R3 = talk_record_handler:handle_action(
            grants_collection,
            req(#{method => <<"PUT">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(428, maps:get(response_status, R3)),
        R4 = talk_record_handler:handle_action(
            grants_collection,
            req(#{
                method => <<"PUT">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:1">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(412, maps:get(response_status, R4)),
        ?assertEqual(5526, maps:get(<<"code">>, maps:get(response_body, R4))),

        %% owner 行提交 → 422（5547）；重复 → 422（5548）
        mock_body(#{<<"grants">> => [#{<<"grantee_user_id">> => U1, <<"role">> => <<"viewer">>}]}),
        R5 = talk_record_handler:handle_action(
            grants_collection,
            req(#{
                method => <<"PUT">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:2">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R5)),
        ?assertEqual(5547, maps:get(<<"code">>, maps:get(response_body, R5))),
        mock_body(#{
            <<"grants">> => [
                #{<<"grantee_user_id">> => U2, <<"role">> => <<"viewer">>},
                #{<<"grantee_user_id">> => U2, <<"role">> => <<"editor">>}
            ]
        }),
        R6 = talk_record_handler:handle_action(
            grants_collection,
            req(#{
                method => <<"PUT">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:2">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R6)),
        ?assertEqual(5548, maps:get(<<"code">>, maps:get(response_body, R6))),

        %% viewer PUT → 403；陌生人 GET → 404；405
        R7 = talk_record_handler:handle_action(
            grants_collection,
            req(#{
                method => <<"PUT">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:2">>}
            }),
            #{current_uid => U2}
        ),
        ?assertEqual(403, maps:get(response_status, R7)),
        R8 = talk_record_handler:handle_action(
            grants_collection,
            req(#{method => <<"GET">>, bindings => Bind}),
            #{current_uid => 700007}
        ),
        ?assertEqual(404, maps:get(response_status, R8)),
        R9 = talk_record_handler:handle_action(
            grants_collection,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(405, maps:get(response_status, R9)),

        %% 审计行（grants_replace）
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"grants_replace">>))
    end).

%%%===================================================================
%%% DELETE /:id/grants/:grant_id（立即生效 + 404）
%%%===================================================================

grant_revoke_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 62),
        Rid = maps:get(record, F),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
        {ok, Listed} = talk_record_grant_logic:list(U1, Rid),
        [GrantRow | _] = maps:get(<<"list">>, Listed),
        GrantId = maps:get(<<"id">>, GrantRow),
        Bind = #{id => integer_to_binary(Rid), grant_id => integer_to_binary(GrantId)},

        %% 撤销：200 + 名单清空 + acl_revision 推进 + 审计
        R1 = talk_record_handler:handle_action(
            grants_item,
            req(#{
                method => <<"DELETE">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:1">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        ?assertEqual([], maps:get(<<"list">>, maps:get(response_body, R1))),
        ?assertEqual(<<"acl:2">>, maps:get(<<"etag">>, maps:get(response_body, R1))),
        ?assertEqual(1, talk_record_t15_support:audit_count(Conn, Rid, <<"grant_revoke">>)),

        %% 被撤者立即失读（404）；重复撤销 404；viewer 删 → 403
        R2 = talk_record_handler:handle_action(
            grants_collection,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}}),
            #{current_uid => U2}
        ),
        ?assertEqual(404, maps:get(response_status, R2)),
        R3 = talk_record_handler:handle_action(
            grants_item,
            req(#{
                method => <<"DELETE">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:2">>}
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(404, maps:get(response_status, R3)),
        talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
        R4 = talk_record_handler:handle_action(
            grants_item,
            req(#{
                method => <<"DELETE">>,
                bindings => Bind,
                headers => #{<<"if-match">> => <<"acl:3">>}
            }),
            #{current_uid => U2}
        ),
        ?assertEqual(403, maps:get(response_status, R4)),
        %% 缺 If-Match → 428
        R5 = talk_record_handler:handle_action(
            grants_item,
            req(#{method => <<"DELETE">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(428, maps:get(response_status, R5))
    end).

%%%===================================================================
%%% share-card / playback（响应字段与码位）
%%%===================================================================

card_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 63),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        Bind = #{id => integer_to_binary(Rid)},

        %% share-card：200 + 严格标题「面聊记录」（不泄漏记录标题）+ 路由
        R1 = talk_record_handler:handle_action(
            share_card,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        Card = maps:get(response_body, R1),
        ?assertEqual(<<"talk_record_card">>, maps:get(<<"card_type">>, Card)),
        ?assertEqual(<<"面聊记录"/utf8>>, maps:get(<<"title">>, Card)),
        ?assertNotEqual(<<"t17-handler"/utf8>>, maps:get(<<"title">>, Card)),
        %% 陌生人 → 404；405
        R2 = talk_record_handler:handle_action(
            share_card,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => 700007}
        ),
        ?assertEqual(404, maps:get(response_status, R2)),
        R3 = talk_record_handler:handle_action(
            share_card,
            req(#{method => <<"GET">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(405, maps:get(response_status, R3))
    end).

playback_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 66),
        Rid = maps:get(record, F),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        Bind = #{id => integer_to_binary(Rid)},

        %% playback：200 + url（票据在 query）/expiry/expires_at/position_ms
        mock_body(#{<<"position_ms">> => 12000}),
        R4 = talk_record_handler:handle_action(
            playback,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R4)),
        Session = maps:get(response_body, R4),
        Url = maps:get(<<"url">>, Session),
        ?assertMatch(<<"/api/v1/talk-records/", _/binary>>, Url),
        ?assert(is_integer(maps:get(<<"expiry">>, Session))),
        ?assert(is_binary(maps:get(<<"expires_at">>, Session))),
        ?assertEqual(12000, maps:get(<<"position_ms">>, Session)),
        %% viewer 亦可开票；陌生人 → 404
        talk_record_t15_support:add_grant(maps:get(conn, F), Rid, U2, U1, <<"viewer">>),
        mock_body(#{}),
        R5 = talk_record_handler:handle_action(
            playback,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U2}
        ),
        ?assertEqual(200, maps:get(response_status, R5)),
        R6 = talk_record_handler:handle_action(
            playback,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => 700007}
        ),
        ?assertEqual(404, maps:get(response_status, R6)),

        %% 媒体未就绪 → 409（5549）；位置越界 → 422
        {ok, FreshRid} = talk_record_t15_support:create_record(U1, 67, <<"未就绪"/utf8>>),
        mock_body(#{}),
        R7 = talk_record_handler:handle_action(
            playback,
            req(#{method => <<"POST">>, bindings => #{id => integer_to_binary(FreshRid)}}),
            #{current_uid => U1}
        ),
        ?assertEqual(409, maps:get(response_status, R7)),
        ?assertEqual(5549, maps:get(<<"code">>, maps:get(response_body, R7))),
        mock_body(#{<<"position_ms">> => 999999}),
        R8 = talk_record_handler:handle_action(
            playback,
            req(#{method => <<"POST">>, bindings => Bind}),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R8))
    end).

%%%===================================================================
%%% share-candidates（通用 + record 级；qs 解析 + 码位）
%%%===================================================================

candidates_handler_test_() ->
    with_db(fun(State) ->
        F = fixture(State, 65),
        Ids = maps:get(ids, F),
        U1 = maps:get(u1, F),
        OrgA = maps:get(org_a, Ids),

        %% 通用 org 候选：200 + list/next_cursor
        R1 = talk_record_handler:handle_action(
            share_candidates,
            req(#{
                method => <<"GET">>,
                qs => <<"scope=organization&organization_id=", (integer_to_binary(OrgA))/binary>>
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        ?assert(length(maps:get(<<"list">>, maps:get(response_body, R1))) > 0),

        %% 跨企业 → 403；组合非法 → 422；q 超长 → 422
        R2 = talk_record_handler:handle_action(
            share_candidates,
            req(#{
                method => <<"GET">>,
                qs => <<"scope=organization&organization_id=", (integer_to_binary(OrgA))/binary>>
            }),
            #{current_uid => maps:get(u4, Ids)}
        ),
        ?assertEqual(403, maps:get(response_status, R2)),
        R3 = talk_record_handler:handle_action(
            share_candidates,
            req(#{method => <<"GET">>, qs => <<"scope=organization">>}),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R3)),
        LongQ = binary:copy(<<"a">>, 101),
        R4 = talk_record_handler:handle_action(
            share_candidates,
            req(#{
                method => <<"GET">>,
                qs => <<"scope=personal&q=", LongQ/binary>>
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R4)),

        %% record 级候选：owner 200（limit=100 全量，730xxx 翻页批在前）；
        %% viewer 403；陌生人 404
        Rid = maps:get(record, F),
        talk_record_t15_support:add_grant(
            maps:get(conn, F), Rid, maps:get(u2, Ids), U1, <<"viewer">>
        ),
        R5 = talk_record_handler:handle_action(
            record_share_candidates,
            req(#{
                method => <<"GET">>,
                bindings => #{id => integer_to_binary(Rid)},
                qs => <<"limit=100">>
            }),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R5)),
        Candidates = [
            maps:get(<<"user_id">>, C)
         || C <- maps:get(<<"list">>, maps:get(response_body, R5))
        ],
        ?assert(lists:member(maps:get(u3, Ids), Candidates)),
        ?assertNot(lists:member(maps:get(u2, Ids), Candidates)),
        ?assertNot(lists:member(U1, Candidates)),
        R6 = talk_record_handler:handle_action(
            record_share_candidates,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}, qs => <<>>}),
            #{current_uid => maps:get(u2, Ids)}
        ),
        ?assertEqual(403, maps:get(response_status, R6)),
        R7 = talk_record_handler:handle_action(
            record_share_candidates,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}, qs => <<>>}),
            #{current_uid => 700007}
        ),
        ?assertEqual(404, maps:get(response_status, R7)),
        %% 405
        R8 = talk_record_handler:handle_action(
            record_share_candidates,
            req(#{method => <<"POST">>, bindings => #{id => integer_to_binary(Rid)}}),
            #{current_uid => U1}
        ),
        ?assertEqual(405, maps:get(response_status, R8))
    end).
