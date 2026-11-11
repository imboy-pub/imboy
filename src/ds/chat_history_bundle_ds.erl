-module(chat_history_bundle_ds).

%%%
% chat_history_bundle_ds 是 chat_history_bundle（多选消息合并转发快照）的
% payload 合同校验 domain service。
%
% 合同真源：docs/reference/chat-history-bundle-contract.md（RUN
% chat-multi-20261111 冻结 v1）。服务端是透传管道：只守护形态 / 条目数 /
% 序列化尺寸三项硬边界，不解析 entries[].payload 业务语义、不做内容审查
% （禁止字段清单由客户端 sanitizer 在构建侧强制）。
%
% 挂点：message_ds:validate_content_message/2 在 staging 落库之前调用
% （仅 msg_type=chat_history_bundle 且明文；E2EE bundle 的 payload 对服务端
% 不透明，走外层信封形态校验，本模块不参与）。
%%%

-export([validate_payload/1]).

%% bundle 合同常量（与 docs/reference/chat-history-bundle-contract.md §3 一致）
-define(BUNDLE_VERSION, 1).
-define(BUNDLE_MIN_ENTRIES, 1).
-define(BUNDLE_MAX_ENTRIES, 50).
%% 256 KiB：整体 payload 序列化后的字节上限（含等号）。
-define(BUNDLE_MAX_PAYLOAD_BYTES, 256 * 1024).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 校验解码后的 chat_history_bundle payload map
%%
%% 校验顺序（错误码稳定，供 WS 校验错误帧 payload.reason 使用）：
%%   1. payload 是 map，且可 JSON 序列化 → invalid_bundle_shape
%%   2. bundle_version == 1              → invalid_bundle_version
%%   3. entries 为 list 且 1..50 条      → bundle_entries_out_of_range
%%   4. 每条 entry 为 map 且含非空 msg_type binary
%%      与 payload 键（不深解业务类型）   → invalid_bundle_shape
%%   5. 整体序列化 ≤ 256 KiB（含等号）   → bundle_payload_too_large
%%
%% @param Payload 解码后的 payload term（期望 map）
%% @returns ok | {error, invalid_bundle_version | bundle_entries_out_of_range
%%                | bundle_payload_too_large | invalid_bundle_shape}
-spec validate_payload(term()) -> ok | {error, atom()}.
validate_payload(Payload) when is_map(Payload) ->
    case maps:get(<<"bundle_version">>, Payload, undefined) of
        ?BUNDLE_VERSION ->
            validate_entries(Payload);
        _ ->
            {error, invalid_bundle_version}
    end;
validate_payload(_NotAMap) ->
    {error, invalid_bundle_shape}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec validate_entries(map()) -> ok | {error, atom()}.
validate_entries(Payload) ->
    case maps:get(<<"entries">>, Payload, undefined) of
        Entries when is_list(Entries) ->
            case length(Entries) of
                N when N >= ?BUNDLE_MIN_ENTRIES, N =< ?BUNDLE_MAX_ENTRIES ->
                    validate_entry_shapes(Entries, Payload);
                _OutOfRange ->
                    {error, bundle_entries_out_of_range}
            end;
        _NotAList ->
            {error, invalid_bundle_shape}
    end.

-spec validate_entry_shapes([term()], map()) -> ok | {error, atom()}.
validate_entry_shapes([], Payload) ->
    validate_payload_size(Payload);
validate_entry_shapes([Entry | Tail], Payload) when is_map(Entry) ->
    MsgType = maps:get(<<"msg_type">>, Entry, <<>>),
    case is_binary(MsgType) andalso MsgType =/= <<>> andalso maps:is_key(<<"payload">>, Entry) of
        true -> validate_entry_shapes(Tail, Payload);
        false -> {error, invalid_bundle_shape}
    end;
validate_entry_shapes([_NotAMap | _Tail], _Payload) ->
    {error, invalid_bundle_shape}.

%% @private
%% @doc 整体 payload 序列化尺寸上界
%%
%% 与 message_ds:e2ee_envelope_within_limit/1 同口径：jsone:encode 后
%% byte_size（含等号；恰 256 KiB 合法，256 KiB + 1 拒绝）。
%% ponytail: 取尺寸多做一次 encode；条目数已先被 ≤50 收敛，可接受。
-spec validate_payload_size(map()) -> ok | {error, atom()}.
validate_payload_size(Payload) ->
    try
        case byte_size(jsone:encode(Payload, [native_utf8])) of
            Size when Size =< ?BUNDLE_MAX_PAYLOAD_BYTES -> ok;
            _ -> {error, bundle_payload_too_large}
        end
    catch
        _:_ ->
            %% 不可序列化的 term（pid/ref 等）不可能来自 JSON 解码结果，
            %% 属于内部调用方契约违规，按形态错误拒绝。
            {error, invalid_bundle_shape}
    end.
