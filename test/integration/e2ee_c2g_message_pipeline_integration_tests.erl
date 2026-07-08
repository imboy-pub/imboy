%% @doc E2EE C2G（群聊）消息全链路（staging → msg_store_worker 异步落库 →
%% 正式表）密文保真集成测试。
%%
%% 与 C2C 的关键差异（真库审查确认）：msg_c2c.payload 是 text 列、staging
%% 时裸密文会被包装成 JSON 字符串再在 worker 落库前 unwrap 还原；而
%% msg_c2g.payload 是 **jsonb** 列，且 msg_c2g_logic:do_send_c2g/5 在 staging
%% 前就把整条消息（含 to/e2ee 等）jsone:encode 成完整 JSON 信封，staging 阶段
%% 天然是合法 JSON，不会触发 payload 包装/unwrap 路径。E2EE 密文实际保真的
%% 关键点在于 e2ee 字段（map envelope）本身能否在 staging→worker→
%% msg_c2g_repo:write_msg 全链路中不被结构破坏地存回 msg_c2g.e2ee（jsonb）列。
-module(e2ee_c2g_message_pipeline_integration_tests).

-include_lib("eunit/include/eunit.hrl").

e2ee_c2g_pipeline_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach, fun setup/0, fun cleanup/1, [
                {"经 msg_c2g_logic:c2g/3 正常路径发送的群聊 e2ee 消息全链路保真",
                    fun test_via_c2g_logic_survives_pipeline/0},
                {"直接 staging 边界：group_to_id_list 多接收者 + e2ee envelope 落正式表保真",
                    fun test_raw_stage_multi_recipient_survives_pipeline/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    {ok, Owner} = create_test_user(<<"e2ee_c2g_owner">>),
    {ok, Member} = create_test_user(<<"e2ee_c2g_member">>),
    {ok, Gid} = create_test_group(Owner, <<"e2ee_c2g_test_group">>),
    ok = group_member_ds:add_member(Gid, Member),
    Context = #{owner => Owner, member => Member, gid => Gid},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

get_context() ->
    persistent_term:get({?MODULE, test_context}).

%% ===================================================================
%% 测试用例
%% ===================================================================

test_via_c2g_logic_survives_pipeline() ->
    #{owner := Owner, gid := Gid} = get_context(),
    MsgId = integer_to_binary(elib_tsid:generate()),
    E2EE = #{
        <<"ciphertext">> => <<"Z3JvdXBfZW5jcnlwdGVkX2NpcGhlcnRleHQ">>,
        <<"iv">> => <<"MTIzNDU2Nzg5MDEy">>,
        <<"tag">> => <<"YWJjZGVmZ2hpams">>,
        <<"alg">> => <<"AES-256-GCM">>
    },
    Data = #{
        <<"to">> => integer_to_binary(Gid),
        <<"payload">> => #{<<"body">> => <<"[E2EE]"/utf8>>},
        <<"created_at">> => elib_dt:now(),
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"e2ee">> => E2EE
    },
    ok = msg_c2g_logic:c2g(MsgId, Owner, Data),

    {ok, Row} = wait_for_final_row(MsgId),
    E2EEDecoded = jsone:decode(maps:get(<<"e2ee">>, Row), [{object_format, map}]),
    ?assertEqual(E2EE, E2EEDecoded),

    %% payload 列是 jsonb 完整消息信封，其中内嵌 payload.body 必须保真
    PayloadEnvelope = jsone:decode(maps:get(<<"payload">>, Row), [{object_format, map}]),
    ?assertEqual(
        <<"[E2EE]"/utf8>>, maps:get(<<"body">>, maps:get(<<"payload">>, PayloadEnvelope))
    ).

test_raw_stage_multi_recipient_survives_pipeline() ->
    #{owner := Owner, member := Member, gid := Gid} = get_context(),
    MsgId = integer_to_binary(elib_tsid:generate()),
    E2EE = #{
        <<"ciphertext">> => <<"14bVkXq8ZpQ2mR7sT9uWaBcDeFgHiJkLmNoPqRsTuVwXyZ">>,
        <<"iv">> => <<"OTg3NjU0MzIxMDk4">>,
        <<"tag">> => <<"a2ptaWhnZmVkY2Jh">>,
        <<"alg">> => <<"AES-256-GCM">>
    },
    Envelope = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2G">>,
        <<"from">> => Owner,
        <<"to">> => Gid,
        <<"payload">> => #{<<"body">> => <<"raw stage 群消息"/utf8>>},
        <<"e2ee">> => E2EE
    },
    Msg2 = jsone:encode(Envelope, [native_utf8]),
    Now = elib_dt:now(),

    ?assertMatch(
        {ok, _},
        msg_store_ds:stage(
            <<"c2g">>, MsgId, <<"text">>, <<"send">>, E2EE, Msg2, Owner, [Owner, Member], Now, Now
        )
    ),

    {ok, Row} = wait_for_final_row(MsgId),
    E2EEDecoded = jsone:decode(maps:get(<<"e2ee">>, Row), [{object_format, map}]),
    ?assertEqual(E2EE, E2EEDecoded),

    %% 时间线表必须给两个接收者都建行（多接收者投递）
    {ok, TimelineRows} = elib_pg:query(
        <<"SELECT to_uid FROM public.msg_c2g_timeline WHERE msg_id = $1">>, [MsgId]
    ),
    ToUids = lists:sort([maps:get(<<"to_uid">>, R) || R <- TimelineRows]),
    ?assertEqual(lists:sort([Owner, Member]), ToUids).

%% ===================================================================
%% 辅助函数
%% ===================================================================

wait_for_final_row(MsgId) ->
    wait_for_final_row(MsgId, 100).

wait_for_final_row(_MsgId, 0) ->
    error(final_row_not_ready);
wait_for_final_row(MsgId, AttemptsLeft) ->
    case
        elib_pg:query(
            <<"SELECT payload, e2ee FROM public.msg_c2g WHERE msg_id = $1 LIMIT 1">>, [MsgId]
        )
    of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            timer:sleep(50),
            wait_for_final_row(MsgId, AttemptsLeft - 1);
        {error, Reason} ->
            {error, Reason}
    end.

create_test_user(Nickname) ->
    Uid = elib_tsid:generate(),
    Suffix = integer_to_binary(erlang:phash2(Uid, 1000000000)),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => <<Nickname/binary, "_", Suffix/binary>>,
        <<"mobile">> => list_to_binary(io_lib:format("13~9..0B", [erlang:phash2(Uid, 1000000000)])),
        <<"email">> => <<"test_", Suffix/binary, "@example.com">>,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = elib_tsid:generate(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
