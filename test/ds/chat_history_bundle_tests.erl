%% @doc chat_history_bundle（多选消息合并转发快照）payload 合同测试（W1-D）。
%%
%% 合同真源：docs/reference/chat-history-bundle-contract.md（冻结 v1）。
%% 守护四件事：
%%   1. DS 合同校验：bundle_version=1 / entries 1..50 / 序列化 ≤ 256 KiB /
%%      entry 最小形态（msg_type + payload），错误码为稳定原子；
%%   2. 挂点：message_ds:validate_message 在落库前拒绝非法明文 bundle，
%%      错误码转 binary 进 WS 校验错误帧；
%%   3. E2EE：不解密、不重建——bundle + E2EE 外层信封原样透传（{ok, Msg}
%%      返回值与输入逐字节相等），外层信封形态校验仍生效；仅形状合法的
%%      v3 信封（meta_version>=3 且必填字段齐全）豁免明文合同，空 map /
%%      legacy（meta_version<3）map 形态仍走明文合同校验（W3-L F-1）；
%%   4. offline envelope：msg_type / payload 原样透传（无 bundle 专属分支）。
%%
%% 本文件只调纯函数（validate / offline_envelope），无 PG / meck 依赖。
-module(chat_history_bundle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_PAYLOAD_BYTES, 256 * 1024).

%% ===================================================================
%% 1. DS 合同校验
%% ===================================================================

valid_single_entry_ok_test() ->
    ?assertEqual(ok, chat_history_bundle_ds:validate_payload(bundle(1))).

valid_fifty_entries_ok_test() ->
    ?assertEqual(ok, chat_history_bundle_ds:validate_payload(bundle(50))).

zero_entries_rejected_test() ->
    ?assertEqual(
        {error, bundle_entries_out_of_range},
        chat_history_bundle_ds:validate_payload(bundle(0))
    ).

fifty_one_entries_rejected_test() ->
    ?assertEqual(
        {error, bundle_entries_out_of_range},
        chat_history_bundle_ds:validate_payload(bundle(51))
    ).

bundle_version_two_rejected_test() ->
    ?assertEqual(
        {error, invalid_bundle_version},
        chat_history_bundle_ds:validate_payload((bundle(1))#{<<"bundle_version">> => 2})
    ).

bundle_version_missing_rejected_test() ->
    Payload = maps:without([<<"bundle_version">>], bundle(1)),
    ?assertEqual({error, invalid_bundle_version}, chat_history_bundle_ds:validate_payload(Payload)).

bundle_version_wrong_type_rejected_test() ->
    ?assertEqual(
        {error, invalid_bundle_version},
        chat_history_bundle_ds:validate_payload((bundle(1))#{<<"bundle_version">> => <<"1">>})
    ).

payload_not_map_rejected_test() ->
    %% v1/v2 E2EE 的 payload 是密文 base64 串等非 map 形态——明文路径按形态拒绝
    ?assertEqual(
        {error, invalid_bundle_shape}, chat_history_bundle_ds:validate_payload(<<"opaque">>)
    ),
    ?assertEqual({error, invalid_bundle_shape}, chat_history_bundle_ds:validate_payload(undefined)),
    ?assertEqual(
        {error, invalid_bundle_shape}, chat_history_bundle_ds:validate_payload([bundle(1)])
    ).

entries_not_list_rejected_test() ->
    ?assertEqual(
        {error, invalid_bundle_shape},
        chat_history_bundle_ds:validate_payload((bundle(1))#{<<"entries">> => #{}})
    ).

entry_not_map_rejected_test() ->
    ?assertEqual(
        {error, invalid_bundle_shape},
        chat_history_bundle_ds:validate_payload((bundle(1))#{<<"entries">> => [<<"text">>]})
    ).

entry_missing_msg_type_rejected_test() ->
    Entry = hd(entries(1)),
    ?assertEqual(
        {error, invalid_bundle_shape},
        chat_history_bundle_ds:validate_payload(#{
            <<"bundle_version">> => 1,
            <<"entries">> => [maps:remove(<<"msg_type">>, Entry)]
        })
    ).

entry_empty_msg_type_rejected_test() ->
    Entry = hd(entries(1)),
    ?assertEqual(
        {error, invalid_bundle_shape},
        chat_history_bundle_ds:validate_payload(#{
            <<"bundle_version">> => 1,
            <<"entries">> => [Entry#{<<"msg_type">> => <<>>}]
        })
    ).

entry_missing_payload_rejected_test() ->
    Entry = hd(entries(1)),
    ?assertEqual(
        {error, invalid_bundle_shape},
        chat_history_bundle_ds:validate_payload(#{
            <<"bundle_version">> => 1,
            <<"entries">> => [maps:remove(<<"payload">>, Entry)]
        })
    ).

%% payload 键只要求存在（透传管道不深解业务值；JSON null 也是合法快照值）
entry_payload_null_ok_test() ->
    ?assertEqual(
        ok,
        chat_history_bundle_ds:validate_payload(#{
            <<"bundle_version">> => 1,
            <<"entries">> => [#{<<"msg_type">> => <<"location">>, <<"payload">> => null}]
        })
    ).

%% 恰 256 KiB（含等号）合法
payload_exactly_256_kib_ok_test() ->
    Payload = sized_bundle(?MAX_PAYLOAD_BYTES),
    ?assertEqual(?MAX_PAYLOAD_BYTES, byte_size(jsone:encode(Payload, [native_utf8]))),
    ?assertEqual(ok, chat_history_bundle_ds:validate_payload(Payload)).

%% 256 KiB + 1 拒绝
payload_256_kib_plus_one_rejected_test() ->
    Payload = sized_bundle(?MAX_PAYLOAD_BYTES + 1),
    ?assertEqual(?MAX_PAYLOAD_BYTES + 1, byte_size(jsone:encode(Payload, [native_utf8]))),
    ?assertEqual(
        {error, bundle_payload_too_large}, chat_history_bundle_ds:validate_payload(Payload)
    ).

%% ===================================================================
%% 2. 入站挂点：validate_message 落库前拒绝（明文路径）
%% ===================================================================

validate_message_accepts_valid_bundle_test() ->
    Msg = c2c_bundle_msg(bundle(1)),
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

validate_message_rejects_bad_version_test() ->
    Msg = c2c_bundle_msg((bundle(1))#{<<"bundle_version">> => 2}),
    ?assertEqual({error, <<"invalid_bundle_version">>}, message_ds:validate_message(Msg)).

validate_message_rejects_out_of_range_test() ->
    Msg = c2c_bundle_msg(bundle(51)),
    ?assertEqual({error, <<"bundle_entries_out_of_range">>}, message_ds:validate_message(Msg)).

validate_message_rejects_oversized_test() ->
    Msg = c2c_bundle_msg(sized_bundle(?MAX_PAYLOAD_BYTES + 1)),
    ?assertEqual({error, <<"bundle_payload_too_large">>}, message_ds:validate_message(Msg)).

validate_message_rejects_non_map_payload_test() ->
    Msg = c2c_bundle_msg(<<"opaque">>),
    ?assertEqual({error, <<"invalid_bundle_shape">>}, message_ds:validate_message(Msg)).

%% C2G 走同一 validate_content_message 挂点（id ≤ 40 前置约束不受影响）
validate_message_c2g_rejects_zero_entries_test() ->
    Msg = (c2c_bundle_msg(bundle(0)))#{
        <<"type">> => <<"C2G">>, <<"id">> => <<"msg-bundle-c2g-01">>
    },
    ?assertEqual({error, <<"bundle_entries_out_of_range">>}, message_ds:validate_message(Msg)).

%% 其他 msg_type 不受 bundle 合同影响（回归守护）
validate_message_text_unaffected_test() ->
    Msg = (c2c_bundle_msg(bundle(51)))#{
        <<"msg_type">> => <<"text">>, <<"payload">> => #{<<"text">> => <<"hi">>}
    },
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

%% ===================================================================
%% 3. E2EE：不解密、不重建，外层信封原样透传
%% ===================================================================

%% v3 外层信封 + 不透明 payload（恒空串）：既不做 bundle 合同校验（服务端
%% 看不见密文内部），也不改动任何字段——{ok, Msg} 与输入逐键相等。
e2ee_bundle_envelope_passthrough_test() ->
    E2EE = #{
        <<"meta_version">> => 3,
        <<"protected_header">> => <<"cHJvdGVjdGVkX2hlYWRlcg">>,
        <<"header_hash">> => <<"aGVhZGVyX2hhc2g">>,
        <<"ciphertext">> => <<"Y2lwaGVydGV4dA">>
    },
    Msg = (c2c_bundle_msg(<<>>))#{<<"e2ee">> => E2EE},
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

%% per-device fan-out 信封同样透传
e2ee_bundle_devices_fanout_passthrough_test() ->
    E2EE = #{
        <<"meta_version">> => 3,
        <<"devices">> => #{
            <<"did-1">> => #{
                <<"protected_header">> => <<"cHc">>,
                <<"header_hash">> => <<"aGg">>,
                <<"ciphertext">> => <<"Y3Q">>
            }
        }
    },
    Msg = (c2c_bundle_msg(<<>>))#{<<"e2ee">> => E2EE},
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

%% E2EE 路径豁免的是 bundle 合同，不是外层信封形态校验（E2EE-060 不放松）
e2ee_bundle_invalid_envelope_still_rejected_test() ->
    Msg = (c2c_bundle_msg(<<>>))#{<<"e2ee">> => #{<<"meta_version">> => 3}},
    ?assertEqual({error, <<"e2ee_envelope_invalid">>}, message_ds:validate_message(Msg)).

%% ===================================================================
%% 3b. W3-L F-1：空 / legacy e2ee map 不是真 v3 信封，不豁免明文合同
%% ===================================================================
%%
%% e2ee 为 map 即跳过 bundle 合同的历史行为构成绕过：e2ee=#{} 或
%% legacy（meta_version<3）map 在 validate_e2ee_envelope/1 处恒为 ok，
%% 51 条 / bundle_version!=1 / >256KiB 的明文 payload 可一路落库广播。
%% 仅形状合法的 v3 信封（meta_version>=3 且必填字段齐全）才豁免合同。

e2ee_empty_map_does_not_exempt_entries_contract_test() ->
    Msg = (c2c_bundle_msg(bundle(51)))#{<<"e2ee">> => #{}},
    ?assertEqual({error, <<"bundle_entries_out_of_range">>}, message_ds:validate_message(Msg)).

e2ee_empty_map_does_not_exempt_version_contract_test() ->
    Msg = (c2c_bundle_msg((bundle(1))#{<<"bundle_version">> => 2}))#{<<"e2ee">> => #{}},
    ?assertEqual({error, <<"invalid_bundle_version">>}, message_ds:validate_message(Msg)).

e2ee_empty_map_does_not_exempt_size_contract_test() ->
    Msg = (c2c_bundle_msg(sized_bundle(?MAX_PAYLOAD_BYTES + 1)))#{<<"e2ee">> => #{}},
    ?assertEqual({error, <<"bundle_payload_too_large">>}, message_ds:validate_message(Msg)).

%% legacy（meta_version<3）map 同样不豁免（豁免判定与
%% validate_e2ee_envelope/1 的 v3 判定同口径）
e2ee_legacy_map_does_not_exempt_entries_contract_test() ->
    Msg = (c2c_bundle_msg(bundle(51)))#{<<"e2ee">> => #{<<"meta_version">> => 1}},
    ?assertEqual({error, <<"bundle_entries_out_of_range">>}, message_ds:validate_message(Msg)).

e2ee_legacy_map_does_not_exempt_version_contract_test() ->
    Msg = (c2c_bundle_msg((bundle(1))#{<<"bundle_version">> => 2}))#{
        <<"e2ee">> => #{<<"meta_version">> => 2}
    },
    ?assertEqual({error, <<"invalid_bundle_version">>}, message_ds:validate_message(Msg)).

%% 3c. S2 安全审查（2026-11-12）：伪造 v3 形状信封不得豁免明文合同
%%
%% is_v3_e2ee_envelope/1 只校验形状（meta_version>=3 + 必填字段非空
%% binary），全部字段攻击者可控；且链路上无一层强制"v3 bundle 外层
%% payload 恒为空串"（合同 §5）。伪造信封 + 非空超限 payload 曾可绕过
%% 256KiB / 1..50 / bundle_version（实测 308KB / 51 条通过校验落库）。
%% 修复：v3 豁免仅当外层 payload =:= <<>>；非空一律回落明文合同。真实
%% v3 豁免不变由 e2ee_bundle_envelope_passthrough_test（payload=<<>>）守护。

forged_v3_envelope() ->
    #{
        <<"meta_version">> => 3,
        <<"protected_header">> => <<"Zm9yZ2Vk">>,
        <<"header_hash">> => <<"aGFzaA">>,
        <<"ciphertext">> => <<"ZmFrZQ">>
    }.

forged_v3_envelope_does_not_exempt_entries_contract_test() ->
    Msg = (c2c_bundle_msg(bundle(51)))#{<<"e2ee">> => forged_v3_envelope()},
    ?assertEqual(
        {error, <<"bundle_entries_out_of_range">>},
        message_ds:validate_message(Msg)
    ).

forged_v3_envelope_does_not_exempt_size_contract_test() ->
    Msg = (c2c_bundle_msg(sized_bundle(?MAX_PAYLOAD_BYTES + 1)))#{
        <<"e2ee">> => forged_v3_envelope()
    },
    ?assertEqual(
        {error, <<"bundle_payload_too_large">>},
        message_ds:validate_message(Msg)
    ).

forged_v3_envelope_valid_bundle_with_empty_payload_still_ok_test() ->
    %% 控制组：v3 形状 + 外层 payload 恰为空串 → 与真 v3 同形态，该形态
    %% 对服务端不可区分且无明文合同违规面，保持豁免透传。
    Msg = (c2c_bundle_msg(<<>>))#{<<"e2ee">> => forged_v3_envelope()},
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

%% 空 map / legacy map 本身仍被 envelope 视为未加密：合法明文 bundle 不受影响
e2ee_empty_map_valid_bundle_still_ok_test() ->
    Msg = (c2c_bundle_msg(bundle(1)))#{<<"e2ee">> => #{}},
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

e2ee_legacy_map_valid_bundle_still_ok_test() ->
    Msg = (c2c_bundle_msg(bundle(2)))#{<<"e2ee">> => #{<<"meta_version">> => 1}},
    ?assertEqual({ok, Msg}, message_ds:validate_message(Msg)).

%% ===================================================================
%% 4. offline envelope：msg_type / payload 原样透传
%% ===================================================================

offline_envelope_preserves_bundle_test() ->
    BundlePayload = bundle(2),
    Row = #{
        <<"id">> => 424242,
        <<"msg_id">> => <<"m-bundle-offline-1">>,
        <<"from_id">> => 100,
        <<"to_id">> => 200,
        <<"payload">> => BundlePayload,
        <<"msg_type">> => <<"chat_history_bundle">>,
        <<"e2ee">> => null,
        <<"created_at">> => 1750000000000,
        <<"server_ts">> => 1750000000001
    },
    Msg = message_ds:offline_envelope(<<"C2C">>, Row),
    ?assertEqual(<<"chat_history_bundle">>, maps:get(<<"msg_type">>, Msg)),
    ?assertEqual(BundlePayload, maps:get(<<"payload">>, Msg)),
    %% 业务 msg_id 优先（PFv3 context binding 契约）
    ?assertEqual(<<"m-bundle-offline-1">>, maps:get(<<"id">>, Msg)).

%% ===================================================================
%% Fixtures
%% ===================================================================

bundle(Count) ->
    #{
        <<"bundle_version">> => 1,
        <<"entries">> => entries(Count)
    }.

entries(Count) ->
    [
        #{<<"msg_type">> => <<"text">>, <<"payload">> => #{<<"i">> => I}}
     || I <- lists:seq(1, Count)
    ].

%% 构造序列化后恰为 Target 字节的合法单条 bundle：
%% filler 全 ASCII 'a'（无转义），每加 1 字符编码恰好 +1 字节。
sized_bundle(Target) ->
    Base = sized_bundle_payload(0),
    BaseSize = byte_size(jsone:encode(Base, [native_utf8])),
    sized_bundle_payload(Target - BaseSize).

sized_bundle_payload(FillerLen) ->
    #{
        <<"bundle_version">> => 1,
        <<"entries">> => [
            #{
                <<"msg_type">> => <<"text">>,
                <<"payload">> => #{<<"filler">> => filler(FillerLen)}
            }
        ]
    }.

filler(0) ->
    <<>>;
filler(N) when N > 0 ->
    binary:copy(<<"a">>, N).

c2c_bundle_msg(Payload) ->
    #{
        <<"id">> => <<"msg-bundle-001">>,
        <<"type">> => <<"C2C">>,
        <<"from">> => 100,
        <<"to">> => 200,
        <<"msg_type">> => <<"chat_history_bundle">>,
        <<"e2ee">> => null,
        <<"payload">> => Payload
    }.
