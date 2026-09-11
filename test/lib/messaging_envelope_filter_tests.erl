-module(messaging_envelope_filter_tests).
-include_lib("eunit/include/eunit.hrl").

%%% 发生率压降路径2：E2EE per-device fan-out 信封过滤（messaging_logic）。
%%% 覆盖纯判定函数 c2c_deliverable_to_device/3 的 keep/drop 矩阵；
%%% 标记 msg_delivery 的副作用路径（filter_c2c_for_device 的 drop 分支）
%%% 依赖 PG，不在 eunit VM 覆盖范围（与本仓其余 logic 测试同口径）。

-define(UID, 1000000056).
-define(DID, <<"03F17F1F-770F-5CBC-AADA-F3722CF4E90B">>).
-define(OTHER_DID, <<"AAAAAAAA-BBBB-CCCC-DDDD-EEEEFFFFFFFF">>).

%% 收件 + per_device + devices 含本机 DID → keep
received_with_own_envelope_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m1">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => #{
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{?DID => #{<<"ciphertext">> => <<"x">>}}
        }
    },
    ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)).

%% 收件 + per_device + devices 无本机 DID（换设备后同步的历史）→ drop
received_without_own_envelope_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m2">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => #{
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{?OTHER_DID => #{<<"ciphertext">> => <<"x">>}}
        }
    },
    ?_assertNot(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)).

%% 收件 + per_device + devices 缺失（客户端同判 fan_out_missing_devices）→ drop
received_without_devices_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m3">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => #{<<"fan_out">> => <<"per_device">>}
    },
    ?_assertNot(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)).

%% 自己发出（from_id=我）：devices 只装对端设备、无本机 DID 属预期 → keep
sent_by_me_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m4">>,
        <<"from_id">> => ?UID,
        <<"to_id">> => 1000000051,
        <<"e2ee">> => #{
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{?OTHER_DID => #{<<"ciphertext">> => <<"x">>}}
        }
    },
    ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)).

%% 自己发出 + from_id 为二进制数字（行形状兜底）→ keep
sent_by_me_binary_from_id_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m5">>,
        <<"from_id">> => integer_to_binary(?UID),
        <<"to_id">> => 1000000051,
        <<"e2ee">> => #{
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{?OTHER_DID => #{}}
        }
    },
    ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)).

%% 非 per_device（Megolm 群聊 / v1-v2 RSA）→ keep
non_per_device_test_() ->
    MsgMegolm = #{
        <<"msg_id">> => <<"m6">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => #{<<"protocol">> => <<"megolm">>, <<"gid">> => <<"g1">>}
    },
    MsgV2 = #{
        <<"msg_id">> => <<"m7">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => #{<<"keys">> => [#{<<"kid">> => <<"k">>}]}
    },
    [
        ?_assert(messaging_logic:c2c_deliverable_to_device(MsgMegolm, ?DID, ?UID)),
        ?_assert(messaging_logic:c2c_deliverable_to_device(MsgV2, ?DID, ?UID))
    ].

%% 非加密消息 / e2ee=null → keep
plaintext_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m8">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => null
    },
    MsgNoField = #{<<"msg_id">> => <<"m9">>, <<"from_id">> => 1000000051, <<"to_id">> => ?UID},
    [
        ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)),
        ?_assert(messaging_logic:c2c_deliverable_to_device(MsgNoField, ?DID, ?UID))
    ].

%% e2ee 为 JSON 字符串（msg_store 旧行形状）→ 正常解析判定
e2ee_as_json_binary_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m10">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> =>
            jsone:encode(#{
                <<"fan_out">> => <<"per_device">>,
                <<"devices">> => #{?OTHER_DID => #{}}
            })
    },
    [
        ?_assertNot(messaging_logic:c2c_deliverable_to_device(Msg, ?DID, ?UID)),
        ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, ?OTHER_DID, ?UID))
    ].

%% DID 缺省（旧客户端 / 未带 did）：一律 keep，行为与旧版一致
empty_did_failopen_test_() ->
    Msg = #{
        <<"msg_id">> => <<"m11">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => #{
            <<"fan_out">> => <<"per_device">>,
            <<"devices">> => #{?OTHER_DID => #{}}
        }
    },
    [
        ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, <<>>, ?UID)),
        ?_assert(messaging_logic:c2c_deliverable_to_device(Msg, undefined, ?UID))
    ].

%% 畸形 e2ee（坏 JSON / 非对象）：宁多勿漏 → keep
malformed_e2ee_test_() ->
    MsgBad = #{
        <<"msg_id">> => <<"m12">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => <<"{not-json">>
    },
    MsgArr = #{
        <<"msg_id">> => <<"m13">>,
        <<"from_id">> => 1000000051,
        <<"to_id">> => ?UID,
        <<"e2ee">> => <<"[1,2]">>
    },
    [
        ?_assert(messaging_logic:c2c_deliverable_to_device(MsgBad, ?DID, ?UID)),
        ?_assert(messaging_logic:c2c_deliverable_to_device(MsgArr, ?DID, ?UID))
    ].
