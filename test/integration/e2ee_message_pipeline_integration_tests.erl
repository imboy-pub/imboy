%% @doc E2EE C2C 消息全链路（staging → msg_store_worker 异步落库 → 正式表）
%% 密文保真集成测试。
%%
%% 背景：真机实测（2026-07-07）发现 E2EE 裸密文以数字开头被旧版首字符
%% 启发式误判为 JSON 数字，写入 staging 的 JSONB 列时 PG 报 22P02 崩溃
%% websocket_handler（commit 04d64ce4 已修）。该修复当时只补了 meck 拦截
%% elib_pg_sql:insert 的单测（msg_store_repo_tests.erl），从未有测试真正
%% 打到 PostgreSQL、也从未验证过异步 worker（msg_store_worker）把 staging
%% 行搬到正式表 msg_c2c 之后，密文是否仍然逐字节保真——这正是 42P08（已读
%% 回执 SQL 参数类型推断歧义）同款盲区：全 meck 单测测的是"调用姿势"，
%% 测不出真实 PostgreSQL 的类型推断/落库行为。
%%
%% 对 E2EE 而言这个保真属性尤其关键：服务端不持有解密密钥，管线中任何一次
%% JSON 转义/反转义偏差都等价于该消息永久不可解密，无法事后补救。
-module(e2ee_message_pipeline_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("imboy_frame.hrl").

e2ee_pipeline_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach, fun setup/0, fun cleanup/1, [
                {"数字开头裸密文 payload + e2ee envelope 全链路落正式表保真",
                    fun test_raw_ciphertext_payload_survives_pipeline/0},
                {"经 msg_c2c_logic:c2c/3 正常路径发送的 e2ee 消息全链路保真",
                    fun test_via_c2c_logic_survives_pipeline/0},
                {"E2EE-060 PFv3 per-device fan-out 信封落 jsonb 后再出站线上帧逐字节保真",
                    fun test_pfv3_fanout_survives_pipeline_and_wire/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    {ok, User1} = create_test_user(<<"e2ee_pipe_u1">>),
    {ok, User2} = create_test_user(<<"e2ee_pipe_u2">>),
    Context = #{user1 => User1, user2 => User2},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

%% 直接打 msg_store_ds:stage/10（staging 边界），精确复现真机事故的输入
%% 形状：payload 是裸的、数字开头的 base64 密文 binary（而非 JSON 封装）。
test_raw_ciphertext_payload_survives_pipeline() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    MsgId = integer_to_binary(elib_tsid:generate()),
    Cipher = <<"14bVkXq8ZpQ2mR7sT9uWaBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789AB+/==">>,
    E2EE = #{
        <<"ciphertext">> => Cipher,
        <<"iv">> => <<"MTIzNDU2Nzg5MDEy">>,
        <<"tag">> => <<"YWJjZGVmZ2hpams=">>,
        <<"alg">> => <<"AES-256-GCM">>
    },
    Now = elib_dt:now(),

    %% staging 写入不应崩溃（22P02 回归）
    ?assertMatch(
        {ok, _},
        msg_store_ds:stage(
            <<"c2c">>, MsgId, <<"text">>, <<"send">>, E2EE, Cipher, User1, User2, Now, Now
        )
    ),

    %% 等待 msg_store_worker 异步落正式表
    {ok, Row} = wait_for_final_row(MsgId),

    %% payload 列是 msg_c2c.payload（text，非 jsonb）：必须与原始裸密文逐字节一致，
    %% 不能带 unwrap_staging_payload 应剥离的 JSON 包装引号
    ?assertEqual(Cipher, maps:get(<<"payload">>, Row)),

    %% e2ee 列是 msg_c2c.e2ee（jsonb）：envelope 的每个字段必须原样保留
    E2EEDecoded = jsone:decode(maps:get(<<"e2ee">>, Row), [{object_format, map}]),
    ?assertEqual(E2EE, E2EEDecoded).

%% 走生产真实入口 msg_c2c_logic:c2c/3（客户端 JSON 帧解出的 payload 形态），
%% 覆盖 message_policy:encode_payload/1 之后的全链路。
test_via_c2c_logic_survives_pipeline() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    ok = ensure_friends(User1, User2),

    MsgId = integer_to_binary(elib_tsid:generate()),
    E2EE = #{
        <<"ciphertext">> => <<"bE8vQ0NXUmt2QUpvV1RPeU9OZz09">>,
        <<"iv">> => <<"OTg3NjU0MzIxMDk4">>,
        <<"tag">> => <<"a2ptaWhnZmVkY2Jh">>,
        <<"alg">> => <<"AES-256-GCM">>
    },
    Data = #{
        <<"to">> => integer_to_binary(User2),
        <<"payload">> => #{<<"body">> => <<"[E2EE]"/utf8>>},
        <<"created_at">> => elib_dt:now(),
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"send">>,
        <<"e2ee">> => E2EE
    },
    ok = msg_c2c_logic:c2c(MsgId, User1, Data),

    {ok, Row} = wait_for_final_row(MsgId),
    PayloadDecoded = jsone:decode(maps:get(<<"payload">>, Row), [{object_format, map}]),
    ?assertEqual(#{<<"body">> => <<"[E2EE]"/utf8>>}, PayloadDecoded),

    E2EEDecoded = jsone:decode(maps:get(<<"e2ee">>, Row), [{object_format, map}]),
    ?assertEqual(E2EE, E2EEDecoded).

%% E2EE-060：闭合 "客户端 fixture → 生产入口 → PostgreSQL jsonb → 出站 WS 帧"
%% 这一整条链。PFv3 信封的 protected_header/header_hash/ciphertext 是
%% base64url（含 - 与 _、无填充），任何一处转义或裁剪都等价于消息永久不可
%% 解密；服务端不持密钥，事后无法修复。
test_pfv3_fanout_survives_pipeline_and_wire() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),
    ok = ensure_friends(User1, User2),

    MsgId = integer_to_binary(elib_tsid:generate()),
    %% chat_network_service.dart buildOlmFanOutPayload 的实际产出形状
    E2EE = #{
        <<"meta_version">> => 3,
        <<"protocol">> => <<"olm">>,
        <<"version">> => 1,
        <<"fan_out">> => <<"per_device">>,
        <<"devices">> => #{
            <<"dev-a">> => #{
                <<"protected_header">> => <<"omh2ImlkIqJtZXNzYWdlX2lk-_Ag">>,
                <<"header_hash">> => <<"dGVzdC1oYXNoLTI1Ng">>,
                <<"ciphertext">> => <<"14bVkXq8ZpQ2mR7sT9uWaBcDeFgHiJkLmNoPqRsTuVwXyZ-_09">>,
                <<"protocol_metadata">> => #{
                    <<"session_id">> => <<"sess-a">>, <<"message_index">> => 0
                }
            },
            <<"dev-b">> => #{
                <<"protected_header">> => <<"omh2ImlkIqJtZXNzYWdlX2ll-_Bg">>,
                <<"header_hash">> => <<"dGVzdC1oYXNoLTI1Nw">>,
                <<"ciphertext">> => <<"25cWlYr9aqR3nS8tU0vXbCdEfGhIjKlMnOpQrStUvWxYz-_10">>,
                <<"protocol_metadata">> => #{
                    <<"session_id">> => <<"sess-b">>, <<"message_index">> => 7
                }
            }
        }
    },
    Data = #{
        <<"to">> => integer_to_binary(User2),
        <<"payload">> => #{<<"body">> => <<>>},
        <<"created_at">> => elib_dt:now(),
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => E2EE
    },
    ok = msg_c2c_logic:c2c(MsgId, User1, Data),

    %% 1) PostgreSQL jsonb 往返：信封语义完全一致，未知/嵌套字段未被裁剪
    {ok, Row} = wait_for_final_row(MsgId),
    E2EEFromDb = jsone:decode(maps:get(<<"e2ee">>, Row), [{object_format, map}]),
    ?assertEqual(E2EE, E2EEFromDb),

    %% 2) 出站线上帧（imboy.v2 连接：protocol=protobuf, framing=v2）
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => integer_to_binary(User1),
        <<"to">> => integer_to_binary(User2),
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<"message">>,
        <<"e2ee">> => E2EEFromDb,
        <<"payload">> => <<>>,
        <<"created_at">> => elib_dt:now()
    },
    {binary, Frame} = imboy_codec:encode_ws_msg(protobuf, v2, ?FRAME_TYPE_MSG_C2C, Msg),
    {ok, #imboy_frame{payload = WirePayload}} = imboy_codec:unwrap_v2_frame(Frame),
    WireMap = jsone:decode(WirePayload, [{object_format, map}]),
    ?assertEqual(E2EE, maps:get(<<"e2ee">>, WireMap)).

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

%% 轮询正式表 msg_c2c，直到 worker 异步落库完成（同 msg_forward_integration_tests
%% 的 wait_for_source_message 惯例）。find_msg_by_id/1 不选 e2ee 列，这里直接
%% 打一条原始 SQL 读 payload+e2ee，避免为测试单独扩 repo 公共 API。
wait_for_final_row(MsgId) ->
    wait_for_final_row(MsgId, 100).

wait_for_final_row(_MsgId, 0) ->
    error(final_row_not_ready);
wait_for_final_row(MsgId, AttemptsLeft) ->
    Tb = msg_c2c_repo:tablename(),
    Sql = <<"SELECT payload, e2ee FROM ", Tb/binary, " WHERE msg_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [MsgId]) of
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

ensure_friends(User1, User2) ->
    NowTs = elib_dt:now(),
    ok = friend_ds:confirm_friend(
        friend_ds:is_friend(User1, User2),
        User1,
        User2,
        <<>>,
        #{<<"is_from">> => 1, <<"source">> => <<"test">>},
        <<>>,
        NowTs
    ),
    ok = friend_ds:confirm_friend(
        friend_ds:is_friend(User2, User1),
        User2,
        User1,
        <<>>,
        #{<<"source">> => <<"test">>},
        <<>>,
        NowTs
    ),
    ok = friend_ds:invalidate_cache(User1, User2),
    imboy_cache:flush({check_relationship3, User1, User2}),
    imboy_cache:flush({check_relationship3, User2, User1}),
    ok.
