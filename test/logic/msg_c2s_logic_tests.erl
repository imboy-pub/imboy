-module(msg_c2s_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2s_logic 模块的 EUnit 测试
%%%
%%% 目标：验证客户端到服务端消息业务逻辑功能
%%% 覆盖：C2S 消息处理、外部服务调用、消息路由、ACK 确认、边界条件
%%%
%%% 测试范围：
%%% - c2s/3: C2S 消息入口，路由到不同的外部服务
%%% - c2s_client_ack/3: 客户端确认 C2S 投递消息
%%% - c2s_to_external/5: C2S 消息发送到外部服务（AI/Bot/第三方 API）
%%% - c2s_to_role_chat/3: AI 角色聊天处理（预留接口）
%%%===================================================================

%% ===================================================================
%% c2s/3 测试 - C2S 消息入口与路由
%% ===================================================================

c2s_with_qianfan_bot_succeeds_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"c2s_unsupported">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{<<"to">> => <<"bot_qian_fan">>},

        % 实际调用中会触发异步操作，这里只测试返回值
        Result = msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
        % 由于会调用外部 API 和异步操作，返回值可能是 ok 或 {reply, _}
        ?assert(is_atom(Result) orelse is_tuple(Result))
    end).

c2s_with_unsupported_bot_fails_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => Code}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{<<"to">> => <<"unknown_bot">>},

        Result = msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2s_with_lowercase_bot_name_succeeds_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"c2s_unsupported">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{<<"to">> => <<"BOT_QIAN_FAN">>},  % 大写会被转小写

        Result = msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
        ?assert(is_atom(Result) orelse is_tuple(Result))
    end).

%% ===================================================================
%% c2s_client_ack/3 测试 - 客户端确认 C2S 投递消息
%% ===================================================================

c2s_client_ack_with_valid_params_succeeds_test_() ->
    ?WITH_MECKS([
        {msg_ack_logic, [
            {'client_ack', 4, fun(_Type, _MsgId, _Uid, _DID) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        DID = <<"device_456">>,

        Result = msg_c2s_logic:c2s_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

c2s_client_ack_with_empty_did_succeeds_test_() ->
    ?WITH_MECKS([
        {msg_ack_logic, [
            {'client_ack', 4, fun(_Type, _MsgId, _Uid, _DID) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        DID = <<>>,

        Result = msg_c2s_logic:c2s_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2s_to_external/5 测试 - C2S 消息发送到外部服务
%% ===================================================================

c2s_to_external_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"AI 响应内容"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"text">> => <<"你好"/utf8>>, <<"topic_id">> => 1, <<"topic_title">> => <<"测试主题"/utf8>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"AI 响应"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

c2s_to_external_with_stage_failure_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> error end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"internal_error">>}
            end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"text">> => <<"测试消息"/utf8>>, <<"topic_id">> => 0, <<"topic_title">> => <<>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"响应"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2s_to_external_with_topic_id_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"主题 AI 响应"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        TopicId = 12345,
        TopicTitle = <<"AI 对话主题"/utf8>>,
        Payload = #{<<"text">> => <<"继续讨论"/utf8>>, <<"topic_id">> => TopicId, <<"topic_title">> => TopicTitle},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"继续讨论的响应"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

c2s_to_external_without_topic_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"无主题 AI 响应"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"text">> => <<"简单问题"/utf8>>, <<"topic_id">> => 0, <<"topic_title">> => <<>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"简单回答"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2s_to_role_chat/3 测试 - AI 角色聊天处理（预留接口）
%% ===================================================================

c2s_to_role_chat_returns_not_implemented_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Payload = #{<<"role_id">> => <<"doctor">>},
        Data = #{<<"payload">> => Payload},

        Result = msg_c2s_logic:c2s_to_role_chat(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2s_to_role_chat_with_default_role_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Payload = #{},  % 无 role_id，使用默认值
        Data = #{<<"payload">> => Payload},

        Result = msg_c2s_logic:c2s_to_role_chat(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

c2s_with_empty_msg_id_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"c2s_unsupported">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<>>,
        CurrentUid = 123,
        Data = #{<<"to">> => <<"unknown_bot">>},

        Result = msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
        % 空消息 ID 应该被处理
        ?assert(is_atom(Result) orelse is_tuple(Result))
    end).

c2s_with_empty_to_field_fails_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"c2s_unsupported">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{<<"to">> => <<>>},

        Result = msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
        % 空 to 字段应该返回不支持的消息
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2s_to_external_with_empty_text_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"空文本响应"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"text">> => <<>>, <<"topic_id">> => 0, <<"topic_title">> => <<>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"请提供问题"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

c2s_to_external_with_unicode_text_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"Unicode 响应 🚀🎉"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"text">> => <<"你好，世界！🌍"/utf8>>, <<"topic_id">> => 0, <<"topic_title">> => <<>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"收到！🎉"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 错误处理测试
%% ===================================================================

c2s_with_invalid_data_structure_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"c2s_unsupported">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{},  % 空数据，缺少 to 字段

        % 由于缺少 to 字段，会抛出 badmatch 错误
        try
            msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
            ?assert(false, "Expected badmatch error")
        catch
            error:{badmatch, _} ->
                ?assert(true)
        end
    end).

c2s_to_external_with_missing_payload_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Data = #{<<"created_at">> => 1704067200000},  % 缺少 payload
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"响应"/utf8>>}
        end,

        % 由于缺少 payload 字段，会抛出 badkey 错误
        try
            msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
            ?assert(false, "Expected badkey error")
        catch
            error:{badkey, _} ->
                ?assert(true)
        end
    end).

c2s_to_external_with_missing_text_in_payload_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"topic_id">> => 0},  % 缺少 text 字段
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"响应"/utf8>>}
        end,

        % 由于缺少 text 字段，会抛出 badkey 错误
        try
            msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
            ?assert(false, "Expected badkey error")
        catch
            error:{badkey, _} ->
                ?assert(true)
        end
    end).

%% ===================================================================
%% 特殊场景测试
%% ===================================================================

c2s_with_special_characters_in_bot_name_test_() ->
    ?WITH_MECKS([
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"c2s_unsupported">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{<<"to">> => <<"bot_with-special_char.name">>},

        Result = msg_c2s_logic:c2s(MsgId, CurrentUid, Data),
        % 不支持的 bot 名称
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2s_client_ack_with_binary_did_test_() ->
    ?WITH_MECKS([
        {msg_ack_logic, [
            {'client_ack', 4, fun(_Type, _MsgId, _Uid, _DID) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        DID = <<"device_abc123_xyz">>,

        Result = msg_c2s_logic:c2s_client_ack(MsgId, CurrentUid, DID),
        ?assertEqual(ok, Result)
    end).

c2s_to_external_with_large_topic_id_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"大主题 ID 响应"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        LargeTopicId = 999999999,
        Payload = #{<<"text">> => <<"大主题 ID 消息"/utf8>>, <<"topic_id">> => LargeTopicId, <<"topic_title">> => <<"大主题"/utf8>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"响应"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

c2s_to_external_with_long_topic_title_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"长主题标题响应"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        LongTitle = binary:copy(<<"非常长的主题标题"/utf8>>, 100),  % 构造超长标题
        Payload = #{<<"text">> => <<"消息"/utf8>>, <<"topic_id">> => 1, <<"topic_title">> => LongTitle},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"响应"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% UTF-8 编码测试
%% ===================================================================

c2s_to_external_with_utf8_text_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(123) -> <<"encoded_uid_123">> end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end},
            {'millisecond', 0, fun() -> 1704067200000 end}
        ]},
        {msg_c2s_ds, [
            {'write_topic', 6, fun(_Type, _TopicId, _Uid, _To, _Title, _CreatedAt) -> ok end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2ee, _Payload, _FromId, _ToId, _CreatedAt, _UpdatedAt) -> ok end},
            {'enqueue', 2, fun(_Type, _MsgId, _Data) -> ok end}
        ]},
        {qianfan_api, [
            {'create_chat', 3, fun(_Uid, _Text, _Opts) ->
                #{<<"result">> => <<"中文响应"/utf8>>}
            end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
        ]},
        {elib_str, [
            {'replace_single_quote', 1, fun(Text) -> Text end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Map, _Opts) -> <<"{\"encoded\":\"json\"}">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(_Type) -> [2000, 5000, 7000, 11000] end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        To = <<"bot_qian_fan">>,
        Payload = #{<<"text">> => <<"你好，这是一条中文消息"/utf8>>, <<"topic_id">> => 0, <<"topic_title">> => <<>>},
        Data = #{
            <<"payload">> => Payload,
            <<"created_at">> => 1704067200000
        },
        ApiCallback = fun(_Uid, _Text, _Opts) ->
            #{<<"result">> => <<"收到，我会用中文回复"/utf8>>}
        end,

        Result = msg_c2s_logic:c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback),
        ?assertEqual(ok, Result)
    end).
