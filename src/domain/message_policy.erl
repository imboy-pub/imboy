%%% @doc 消息策略纯函数 / Message Policy (pure functions)
%%%
%%% DDD 充血改造 Phase 1 / T1.1：从 msg_c2c_logic:c2c/3 与
%%% stage_and_send_c2c/11 中抽离「可发 / 暂存 / 构建」三类**纯决策**，
%%% 使其零副作用、可 eunit 独测；I/O 时序保留在 logic 外壳（T1.2 接线）。
%%%
%%% Pure decision core extracted from the C2C send transaction script.
%%% No I/O, no process messaging — just deterministic decisions the
%%% imperative shell consumes. Enables zero-mock unit testing.
-module(message_policy).

-moduledoc "消息策略纯函数 / Message Policy (pure functions)。".
-export([send_decision/2, encode_payload/1, reply_mode/1, build_server_ack/2, validate_text_size/4]).

-export_type([send_decision/0, reply_mode/0]).

-define(MAX_PLAINTEXT_TEXT_BYTES, 65536).

%% UTF-8 正文字节，不计 JSON 转义或信封字段 / UTF-8 body bytes, excluding JSON/envelope overhead.
-spec validate_text_size(binary(), binary(), term(), boolean()) -> ok | {error, binary()}.
validate_text_size(<<"text">>, Action, Payload, Encrypted) when
    Action =:= <<>>; Action =:= <<"message_edit">>
->
    validate_text_fields(text_fields(Payload, Encrypted));
validate_text_size(_, _, _, _) ->
    ok.

validate_text_fields([]) ->
    ok;
validate_text_fields([Text | Rest]) when is_binary(Text) ->
    case unicode:characters_to_binary(Text, utf8, utf8) of
        Valid when is_binary(Valid), byte_size(Text) =< ?MAX_PLAINTEXT_TEXT_BYTES ->
            validate_text_fields(Rest);
        Valid when is_binary(Valid) -> {error, <<"message_text_too_large">>};
        _ ->
            {error, <<"invalid_message_format">>}
    end;
validate_text_fields([_ | _]) ->
    {error, <<"invalid_message_format">>}.

text_fields(Payload, Encrypted) when is_binary(Payload) ->
    case text_json_prefix(Payload) of
        false ->
            raw_text_fields(Payload, Encrypted);
        true ->
            try jsone:decode(Payload, [{object_format, map}]) of
                Decoded when is_map(Decoded) -> text_fields(Decoded, Encrypted);
                Text when is_binary(Text) -> raw_text_fields(Text, Encrypted);
                _ -> raw_text_fields(Payload, Encrypted)
            catch
                _:_ -> raw_text_fields(Payload, Encrypted)
            end
    end;
text_fields(Payload, Encrypted) when is_map(Payload) ->
    % 显式明文字段不能因 e2ee 声明绕过 / An e2ee claim cannot exempt visible plaintext.
    Keys =
        case Encrypted of
            true -> [<<"text">>, <<"content">>];
            false -> [<<"text">>, <<"content">>, <<"body">>]
        end,
    Texts = [maps:get(Key, Payload) || Key <- Keys, maps:is_key(Key, Payload)],
    case maps:find(<<"payload">>, Payload) of
        {ok, Inner} -> Texts ++ text_fields(Inner, Encrypted);
        error -> Texts
    end;
text_fields(_, true) ->
    [];
text_fields(Payload, false) ->
    [Payload].

raw_text_fields(_, true) -> [];
raw_text_fields(Payload, false) -> [Payload].

%% 不把数值正文或密文二次解析为 bigint / Do not parse raw numeric bodies/ciphertext as bigints.
text_json_prefix(<<C, Rest/binary>>) when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    text_json_prefix(Rest);
text_json_prefix(<<C, _/binary>>) when C =:= ${; C =:= $" -> true;
text_json_prefix(_) ->
    false.

%% 可发决策结果。
-type send_decision() ::
    allow
    | {reject, not_a_friend}
    | {reject, in_denylist}.

%% 暂存/存储路径：无引用 vs 带引用。
-type reply_mode() ::
    none
    | {reply, MsgId :: binary(), FromId :: integer(), Snippet :: binary()}.

%% @doc 【可发】基于好友关系与黑名单状态裁决能否发送。
%%
%% 镜像 msg_c2c_logic:c2c/3 的发送裁决语义。拉黑优先于好友判定：
%% InDenylist 为真（boolean true 或正整数）→ in_denylist；好友且未拉黑 → allow；
%% 其余（非好友且未拉黑）→ not_a_friend。
%% 修正历史（原 T1.1 留痕 bug）：原 guard `InDenylist > 0` 对 boolean 输入有缺陷——
%% friend_ds:check_relationship/2 返回 {boolean(), boolean()}，而 Erlang term 序中
%% atom > number，故 `false > 0` =:= true，使「非好友且未拉黑」({false,false}) 误落
%% in_denylist 分支（应为 not_a_friend）。现 guard 显式区分 boolean/integer 修正之。
-spec send_decision(
    IsFriend :: boolean(),
    InDenylist :: boolean() | integer()
) -> send_decision().
send_decision(true, false) ->
    allow;
send_decision(_, InDenylist) when
    InDenylist =:= true orelse (is_integer(InDenylist) andalso InDenylist > 0)
->
    {reject, in_denylist};
send_decision(false, _) ->
    {reject, not_a_friend}.

%% @doc 【构建】payload 归一化：map 编码为 JSON binary，binary 原样透传。
%% 镜像 stage_and_send_c2c/11 中的 PayloadJson 编码分支。
-spec encode_payload(map() | binary()) -> binary().
encode_payload(P) when is_map(P) ->
    jsone:encode(P, [native_utf8]);
encode_payload(P) when is_binary(P) ->
    P.

%% @doc 【暂存】从 extract_reply_info/1 的三元组裁决存储路径。
%% {<<>>, 0, <<>>} 表示无引用；否则带引用（需校验被引用消息存在性）。
-spec reply_mode({binary(), integer(), binary()}) -> reply_mode().
reply_mode({<<>>, 0, <<>>}) ->
    none;
reply_mode({MsgId, FromId, Snippet}) ->
    {reply, MsgId, FromId, Snippet}.

%% @doc 【构建】组装 C2C_SERVER_ACK 响应 map（纯，时间戳由外壳注入）。
%% 【T15/R9】in_reply_to = 被响应的请求消息 id（纯加性字段，旧客户端忽略）；
%% 客户端凭 in_reply_to 存在即可判定"这是对某请求的响应"，长期让 type 回归纯方向枚举。
%% 注意：不能叫 reply_to——该名已被上行消息的引用回复占用（map 类型，见 extract_reply_info/1）。
-spec build_server_ack(MsgId :: binary(), ServerTs :: integer()) -> map().
build_server_ack(MsgId, ServerTs) ->
    #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C_SERVER_ACK">>,
        <<"in_reply_to">> => MsgId,
        <<"server_ts">> => ServerTs
    }.
