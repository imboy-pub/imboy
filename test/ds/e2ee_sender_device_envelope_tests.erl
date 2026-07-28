%% @doc 发送者设备标识必须盖在**信封顶层**，而不是只注入 payload。
%%
%% == 背景 ==
%%
%% 客户端 PFv3 接收侧 `_validateContextBinding` 第 6 项（ADR 15 §3.3）拿
%% 信封顶层的 `sender_did` 与受认证的 `protected_header.sender_did` 硬比对。
%% 该值必须由服务端提供且客户端不可伪造——这正是绑定的意义。
%%
%% 旧实现 `inject_sender_device/2` 注入的是 **payload 内部**，只在 payload 是
%% map 或可 JSON 解码为 map 的 binary 时生效。而 E2EE 消息的 payload 对服务端
%% 不透明：
%%   - v1/v2：`base64(nonce).base64(ct)` 密文串，JSON 解码失败；
%%   - v3（PFv3）：密文在 e2ee.devices 内，外层 payload **恒为空串**。
%% 两种情况都原样返回，什么都没注入 → 客户端拿到空串 → 每条生产 C2C v3 消息
%% 被判 `context_mismatch_sender_did` 而不可读。
%%
%% 客户端侧的实证见
%% `imboyapp/test/service/e2ee/production_inbound_frame_gate_test.dart`。
%% 本文件守护服务端这一半：**payload 是什么形状，信封顶层都必须带上设备标识**。
-module(e2ee_sender_device_envelope_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DID, <<"dev-sender-01">>).
-define(DTYPE, <<"ios">>).

state() ->
    #{did => ?DID, dtype => ?DTYPE}.

%% v3：payload 恒为空串——旧的 payload 注入在这里完全失效，
%% 信封顶层是唯一有效落脚点。
stamp_v3_empty_payload_test() ->
    Data = #{
        <<"id">> => <<"m-1">>,
        <<"type">> => <<"C2C">>,
        <<"payload">> => <<>>,
        <<"e2ee">> => #{<<"meta_version">> => 3}
    },
    Out = message_ds:stamp_sender_device(Data, state()),
    ?assertEqual(?DID, maps:get(<<"sender_did">>, Out)),
    ?assertEqual(?DTYPE, maps:get(<<"sender_dtype">>, Out)),
    %% 不得改动 payload 与 e2ee（E2EE-060 不透明透传）
    ?assertEqual(<<>>, maps:get(<<"payload">>, Out)),
    ?assertEqual(#{<<"meta_version">> => 3}, maps:get(<<"e2ee">>, Out)).

%% v1/v2：payload 是密文串，JSON 解码必失败
stamp_v2_ciphertext_payload_test() ->
    Data = #{
        <<"id">> => <<"m-2">>,
        <<"payload">> => <<"YmFzZTY0bm9uY2U=.YmFzZTY0Y2lwaGVy">>
    },
    Out = message_ds:stamp_sender_device(Data, state()),
    ?assertEqual(?DID, maps:get(<<"sender_did">>, Out)),
    ?assertEqual(
        <<"YmFzZTY0bm9uY2U=.YmFzZTY0Y2lwaGVy">>, maps:get(<<"payload">>, Out)
    ).

%% 对照：旧的 payload 注入对这两种形状确实无效——
%% 证明本次修复针对的是真实缺口，而不是重复既有能力。
inject_into_payload_is_ineffective_for_e2ee_test() ->
    %% v3 空串
    ?assertEqual(<<>>, message_ds:inject_sender_device(<<>>, state())),
    %% v1/v2 密文串
    Ct = <<"YmFzZTY0bm9uY2U=.YmFzZTY0Y2lwaGVy">>,
    ?assertEqual(Ct, message_ds:inject_sender_device(Ct, state())),
    %% 明文 map 仍然照常注入（不得回归）
    Injected = message_ds:inject_sender_device(#{<<"text">> => <<"hi">>}, state()),
    ?assertEqual(?DID, maps:get(<<"sender_did">>, Injected)).

%% State 缺字段时退化为空 binary，不崩
stamp_missing_state_fields_test() ->
    Out = message_ds:stamp_sender_device(#{<<"id">> => <<"m-3">>}, #{}),
    ?assertEqual(<<>>, maps:get(<<"sender_did">>, Out)),
    ?assertEqual(<<>>, maps:get(<<"sender_dtype">>, Out)).

%% 非 map 信封原样返回
stamp_non_map_test() ->
    ?assertEqual(<<"raw">>, message_ds:stamp_sender_device(<<"raw">>, state())).

%% 投递帧必须带上设备标识
with_sender_device_carries_into_frame_test() ->
    Data = message_ds:stamp_sender_device(#{<<"payload">> => <<>>}, state()),
    Msg0 = message_ds:assemble_msg(
        <<"C2C">>, 100, 200, #{}, <<"m-4">>, <<"text">>, <<"message">>, null
    ),
    %% assemble_msg 自身不含该字段——这正是需要 with_sender_device 的原因
    ?assertEqual(error, maps:find(<<"sender_did">>, Msg0)),
    Msg = message_ds:with_sender_device(Msg0, Data),
    ?assertEqual(?DID, maps:get(<<"sender_did">>, Msg)),
    ?assertEqual(?DTYPE, maps:get(<<"sender_dtype">>, Msg)),
    %% 原有字段不得被覆盖
    ?assertEqual(<<"m-4">>, maps:get(<<"id">>, Msg)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, Msg)).

%% 服务端没提供时不得补空值：补 <<>> 会让接收侧把「没提供」
%% 误判成「设备 ID 是空串」，两者的失败语义不同。
with_sender_device_absent_does_not_fabricate_test() ->
    Msg0 = message_ds:assemble_msg(
        <<"C2C">>, 100, 200, #{}, <<"m-5">>, <<"text">>, <<"message">>, null
    ),
    Msg = message_ds:with_sender_device(Msg0, #{<<"payload">> => <<>>}),
    ?assertEqual(error, maps:find(<<"sender_did">>, Msg)),
    ?assertEqual(error, maps:find(<<"sender_dtype">>, Msg)).
