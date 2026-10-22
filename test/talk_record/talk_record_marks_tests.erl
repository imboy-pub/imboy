%%% @doc talk_record marks 真库测试（T07 卡 + 迁移 00000213 裁决落实）。
%%%
%%% 覆盖：
%%%   * 位置真源/投影：position_ms 解析 active capture 绑定、显式
%%%     capture_id+capture_offset_ms、V1 position_ms := capture_offset_ms；
%%%   * 有效录音范围校验（越界 422 / 无采集 422）；
%%%   * client_mark_id 幂等（同内容重放 / 异内容 409）；
%%%   * 清单正序；PATCH（作者限定/If-Match CAS/revision+1/重定位范围校验）；
%%%   * DELETE（作者或 owner / If-Match / 404）；
%%%   * 授权面：viewer grant 可建 marks（author=本人）；无权 404。
-module(talk_record_marks_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REC1, <<"00000000-0000-0000-0000-000000040001">>).
-define(DS1, <<"40000000-0000-0000-0000-000000000001">>).
-define(MK(S), mark_uuid(S)).

setup() ->
    State = talk_record_t07_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t07_support:seed(State),
            State
        end,
        fun talk_record_t07_support:teardown_all/1
    ).

cleanup(State) ->
    talk_record_t07_support:teardown_all(State).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% 位置真源 / 范围 / 幂等
%%%===================================================================

mark_position_binding_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-mk-1">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-mk-c1">>),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?MK(1),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 1,
                <<"event">> => <<"start">>,
                <<"position_ms">> => 0
            },
            #{}
        ),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?MK(2),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 2,
                <<"event">> => <<"pause">>,
                <<"position_ms">> => 60_000
            },
            #{}
        ),
        %% 仅 position_ms：解析 active capture 绑定（真源 capture+offset）
        {ok, created, M} = talk_record_marks_logic:create(
            U1,
            Rid,
            #{
                <<"client_mark_id">> => ?MK(10),
                <<"position_ms">> => 30_000,
                <<"text">> => <<"重点"/utf8>>
            }
        ),
        ?assertEqual(Cid, maps:get(<<"capture_id">>, M)),
        ?assertEqual(30_000, maps:get(<<"capture_offset_ms">>, M)),
        %% V1 投影：读时 position_ms 由 offset 投影（裁决）
        ?assertEqual(30_000, maps:get(<<"position_ms">>, M)),
        Row = talk_record_t07_support:one(
            C,
            "SELECT capture_id, capture_offset_ms, position_ms FROM talk_record_mark WHERE id = " ++
                integer_to_binary(maps:get(<<"id">>, M))
        ),
        ?assertEqual(Cid, maps:get(<<"capture_id">>, Row)),
        ?assertEqual(30_000, maps:get(<<"position_ms">>, Row)),
        %% 显式真源：capture_id + capture_offset_ms
        {ok, created, M2} = talk_record_marks_logic:create(
            U1,
            Rid,
            #{
                <<"client_mark_id">> => ?MK(11),
                <<"capture_id">> => Cid,
                <<"capture_offset_ms">> => 45_000
            }
        ),
        ?assertEqual(45_000, maps:get(<<"capture_offset_ms">>, M2)),
        ?assertEqual(45_000, maps:get(<<"position_ms">>, M2))
    end).

mark_range_and_no_capture_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-mk-2">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-mk-c2">>),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?MK(1),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 1,
                <<"event">> => <<"start">>,
                <<"position_ms">> => 0
            },
            #{}
        ),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?MK(2),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 2,
                <<"event">> => <<"end">>,
                <<"position_ms">> => 60_000
            },
            #{}
        ),
        %% 越界（60_000 有效时长，60_001 拒绝）
        ?assertEqual(
            {error, mark_position_out_of_range},
            talk_record_marks_logic:create(
                U1,
                Rid,
                #{<<"client_mark_id">> => ?MK(20), <<"position_ms">> => 60_001}
            )
        ),
        %% 边界值允许（=duration）
        {ok, created, _} = talk_record_marks_logic:create(
            U1,
            Rid,
            #{<<"client_mark_id">> => ?MK(21), <<"position_ms">> => 60_000}
        ),
        %% 跨记录 capture_id → 404
        ?assertEqual(
            {error, capture_not_found},
            talk_record_marks_logic:create(
                U1,
                Rid,
                #{
                    <<"client_mark_id">> => ?MK(22),
                    <<"capture_id">> => Cid + 1,
                    <<"capture_offset_ms">> => 0
                }
            )
        ),
        %% 无采集记录 → 422 no_capture_for_position
        {ok, Rid2} = talk_record_t07_support:create_record(
            U1,
            <<"t07-mk-2b">>,
            <<"00000000-0000-0000-0000-000000040002">>
        ),
        ?assertEqual(
            {error, no_capture_for_position},
            talk_record_marks_logic:create(
                U1,
                Rid2,
                #{<<"client_mark_id">> => ?MK(23), <<"position_ms">> => 0}
            )
        )
    end).

mark_idempotent_and_conflict_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-mk-3">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-mk-c3">>),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?MK(1),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 1,
                <<"event">> => <<"start">>,
                <<"position_ms">> => 10_000
            },
            #{}
        ),
        Body = #{
            <<"client_mark_id">> => ?MK(30),
            <<"position_ms">> => 5_000,
            <<"text">> => <<"同内容"/utf8>>
        },
        {ok, created, M1} = talk_record_marks_logic:create(U1, Rid, Body),
        {ok, replay, M2} = talk_record_marks_logic:create(U1, Rid, Body),
        ?assertEqual(maps:get(<<"id">>, M1), maps:get(<<"id">>, M2)),
        %% 同 client_mark_id 异内容 → 409
        ?assertEqual(
            {error, mark_conflict},
            talk_record_marks_logic:create(U1, Rid, Body#{<<"text">> => <<"异内容"/utf8>>})
        ),
        %% 同 id 异作者（U2 viewer grant）→ 各自独立（UNIQUE 含 author_id）
        grant_viewer(Rid, U1, maps:get(u2, Ids)),
        {ok, created, _} = talk_record_marks_logic:create(
            maps:get(u2, Ids),
            Rid,
            Body#{<<"text">> => <<"viewer 的 mark"/utf8>>}
        )
    end).

%%%===================================================================
%%% 清单 / PATCH / DELETE / 授权
%%%===================================================================

mark_list_update_delete_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-mk-4">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-mk-c4">>),
        {ok, _} = talk_record_t07_support:send_event(
            U1,
            Rid,
            Cid,
            #{
                <<"client_event_id">> => ?MK(1),
                <<"device_session_id">> => ?DS1,
                <<"seq">> => 1,
                <<"event">> => <<"start">>,
                <<"position_ms">> => 50_000
            },
            #{}
        ),
        grant_editor(Rid, U1, U2),
        %% U2（editor grant，可见）可建 mark（author=U2）
        {ok, created, MU2} = talk_record_marks_logic:create(
            U2,
            Rid,
            #{<<"client_mark_id">> => ?MK(40), <<"position_ms">> => 10_000}
        ),
        {ok, created, M1} = talk_record_marks_logic:create(
            U1,
            Rid,
            #{<<"client_mark_id">> => ?MK(41), <<"position_ms">> => 5_000}
        ),
        {ok, created, M2} = talk_record_marks_logic:create(
            U1,
            Rid,
            #{<<"client_mark_id">> => ?MK(42), <<"position_ms">> => 20_000}
        ),
        %% 清单正序（position asc）
        {ok, #{<<"marks">> := Marks}} = talk_record_marks_logic:list(U1, Rid, 100),
        ?assertEqual(3, length(Marks)),
        ?assertEqual(
            [5_000, 10_000, 20_000],
            [maps:get(<<"position_ms">>, M) || M <- Marks]
        ),
        %% PATCH：非作者（U2 改 U1 的 mark）→ 403
        Mid = maps:get(<<"id">>, M1),
        ?assertEqual(
            {error, mark_not_author},
            talk_record_marks_logic:update(
                U2,
                Rid,
                Mid,
                <<"\"1\"">>,
                #{<<"text">> => <<"越权"/utf8>>}
            )
        ),
        %% PATCH：缺 If-Match → 428
        ?assertEqual(
            {error, precondition_required},
            talk_record_marks_logic:update(U1, Rid, Mid, undefined, #{<<"text">> => <<"x">>})
        ),
        %% PATCH：旧 revision → 412
        ?assertEqual(
            {error, precondition_failed},
            talk_record_marks_logic:update(U1, Rid, Mid, <<"\"9\"">>, #{<<"text">> => <<"x">>})
        ),
        %% PATCH：正确版本 → revision+1
        {ok, MU} = talk_record_marks_logic:update(
            U1,
            Rid,
            Mid,
            <<"\"1\"">>,
            #{<<"text">> => <<"改后"/utf8>>, <<"capture_offset_ms">> => 30_000}
        ),
        ?assertEqual(2, maps:get(<<"revision">>, MU)),
        ?assertEqual(30_000, maps:get(<<"position_ms">>, MU)),
        %% 重定位越界 → 422
        ?assertEqual(
            {error, mark_position_out_of_range},
            talk_record_marks_logic:update(
                U1,
                Rid,
                Mid,
                <<"\"2\"">>,
                #{<<"capture_offset_ms">> => 99_000}
            )
        ),
        %% DELETE：作者本人 + If-Match（当前 revision=2）
        {ok, #{<<"deleted">> := true}} = talk_record_marks_logic:delete(U1, Rid, Mid, <<"\"2\"">>),
        ?assertEqual(
            {error, mark_not_found},
            talk_record_marks_logic:delete(U1, Rid, Mid, <<"\"3\"">>)
        ),
        %% owner 可删他人 mark（U2 的）
        {ok, #{<<"deleted">> := true}} =
            talk_record_marks_logic:delete(U1, Rid, maps:get(<<"id">>, MU2), <<"\"1\"">>)
    end).

mark_no_permission_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        U7 = maps:get(u7, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-mk-5">>, ?REC1),
        %% 无权主体：create/list 统一 404
        ?assertEqual(
            {error, not_found},
            talk_record_marks_logic:create(
                U7,
                Rid,
                #{<<"client_mark_id">> => ?MK(50), <<"position_ms">> => 0}
            )
        ),
        ?assertEqual({error, not_found}, talk_record_marks_logic:list(U7, Rid, 100))
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec mark_uuid(integer()) -> binary().
mark_uuid(Seq) ->
    Hex = integer_to_binary(Seq, 16),
    PadLen = max(0, 12 - byte_size(Hex)),
    Pad = <<<<$0>> || _ <- lists:seq(1, PadLen)>>,
    <<"50000000-0000-0000-0000-", Pad/binary, Hex/binary>>.

-spec grant_viewer(integer(), integer(), integer()) -> ok.
grant_viewer(RecordId, Grantor, Grantee) ->
    insert_grant(RecordId, Grantor, Grantee, <<"viewer">>).

-spec grant_editor(integer(), integer(), integer()) -> ok.
grant_editor(RecordId, Grantor, Grantee) ->
    insert_grant(RecordId, Grantor, Grantee, <<"editor">>).

-spec insert_grant(integer(), integer(), integer(), binary()) -> ok.
insert_grant(RecordId, Grantor, Grantee, Role) ->
    Id = elib_tsid:generate(talk_record),
    {ok, _} = elib_pg:query(
        <<"INSERT INTO talk_record_grant (id, record_id, grantee_user_id, role, granted_by)",
            " VALUES ($1, $2, $3, $4, $5)">>,
        [Id, RecordId, Grantee, Role, Grantor]
    ),
    ok.
