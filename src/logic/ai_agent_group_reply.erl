-module(ai_agent_group_reply).

%%%
% Agent 群触发回复 / Agent group-mention reply dispatch（Phase 4 T4.2 群触发地基）
%
% 群里 @agent → 触发 agent 回复。作为 msg_c2g_logic:do_send_c2g 的 fire-and-forget
% 旁路：人发群消息 @了某 agent → 门控放行 → 异步 LLM → 流式 fan-out 给群内观察者 +
% 定稿作为一条正常 C2G 消息回群（agent 作为一等群成员发送）。
%
% 门控（复用 Phase 1 + 本会话已建）：
%   agent_trigger_policy:should_trigger/2（@触发/关键词/白名单，防刷屏）
%   agent_rate_limiter:allow/2（金钱 DoS 闸门，scope=agent uid）
%
% 红线与防环：
%   - E2EE 群消息一律跳过（不解密做 AI）。
%   - 发送者本身是 agent 的消息不触发（防 agent↔agent 循环；A2A 多 agent 协作留待后续）。
%   - agent 的回复不 @agent，故经 c2g 定稿回群不会自触发。
%%%

-export([maybe_dispatch/4]).
-export([run_and_reply/5]).
%% 导出供单测：支付指令路由 + 收款人解析（T4.3 ②）
-export([try_pay_command/5, payee_mention/2]).

-include("log.hrl").

%% @doc 群消息旁路：对每个被 @ 且启用的 agent，门控放行则异步触发 LLM 回群。
%% 恒 fire-and-forget 返回 ok；任何异常不得拖垮正常 C2G 投递。
-spec maybe_dispatch(integer(), integer(), map(), [integer()]) -> ok.
maybe_dispatch(FromUid, ToGID, Data, MemberUids) ->
    try
        do_maybe_dispatch(FromUid, ToGID, Data, MemberUids)
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_GROUP_GUARD] gid=~p ~p:~p~n", [ToGID, Class, Reason]),
            ok
    end.

-spec do_maybe_dispatch(integer(), integer(), map(), [integer()]) -> ok.
do_maybe_dispatch(FromUid, ToGID, Data, MemberUids) ->
    case is_e2ee(Data) orelse is_agent_sender(FromUid) of
        true ->
            ok;
        false ->
            Text = extract_text(Data),
            MentionedAgents = mentioned_agents(Data),
            %% T4.3 ②：先看是否是确定性支付指令（LLM 不进资金路径）；不是才走 LLM 回复。
            case try_pay_command(FromUid, ToGID, Data, Text, MentionedAgents) of
                handled ->
                    ok;
                ignore ->
                    lists:foreach(
                        fun({AgentUid, Agent}) ->
                            maybe_trigger(FromUid, ToGID, AgentUid, Agent, Text, MemberUids)
                        end,
                        MentionedAgents
                    )
            end
    end.

%% 对单个被 @ 的 agent：过触发策略 + 限流闸门，放行则异步 LLM
-spec maybe_trigger(integer(), integer(), integer(), map(), binary(), [integer()]) -> ok.
maybe_trigger(_FromUid, _ToGID, _AgentUid, _Agent, <<>>, _MemberUids) ->
    ok;
maybe_trigger(FromUid, ToGID, AgentUid, Agent, Text, MemberUids) ->
    Policy = maps:get(<<"trigger_policy">>, Agent, #{}),
    Ctx = #{mentioned => true, text => Text, group_id => ToGID},
    case agent_trigger_policy:should_trigger(Policy, Ctx) of
        false ->
            ok;
        true ->
            case agent_rate_limiter:allow(AgentUid, FromUid) of
                {deny, Reason} ->
                    ok = ?WARN_LOG(
                        "[AGENT_GROUP_RATE_LIMITED] agent=~p gid=~p from=~p reason=~p~n",
                        [AgentUid, ToGID, FromUid, Reason]
                    ),
                    ok;
                allow ->
                    dispatch(ToGID, AgentUid, Agent, Text, MemberUids)
            end
    end.

-spec dispatch(integer(), integer(), map(), binary(), [integer()]) -> ok.
dispatch(ToGID, AgentUid, Agent, Text, MemberUids) ->
    Provider = maps:get(<<"provider">>, Agent, <<>>),
    case imboy_llm_registry:lookup(Provider) of
        {ok, #{module := Mod, opts := Opts0}} ->
            Opts = merge_model(Opts0, Agent),
            Messages = build_messages(Agent, Text),
            elib_async:async(fun() ->
                ?MODULE:run_and_reply(Mod, Opts, ToGID, {AgentUid, Agent}, {Messages, MemberUids})
            end),
            ok;
        undefined ->
            ok = ?ERROR_LOG("[AGENT_GROUP] provider not found: ~p~n", [Provider]),
            ok
    end.

%% @doc 调 LLM 并把结果作为 C2G 从 agent 回投群。流式则逐 delta fan-out 给群成员，
%% 结束用同 id 定稿回群；否则一次性回复。（async 闭包体，独立导出便于测试）
-spec run_and_reply(module(), map(), integer(), {integer(), map()}, {[map()], [integer()]}) -> ok.
run_and_reply(Mod, Opts, ToGID, {AgentUid, _Agent}, {Messages, MemberUids}) ->
    case llm_stream:stream_capable(Mod) of
        true ->
            run_stream(Mod, Opts, ToGID, AgentUid, Messages, MemberUids);
        false ->
            run_sync(Mod, Opts, ToGID, AgentUid, Messages)
    end.

run_sync(Mod, Opts, ToGID, AgentUid, Messages) ->
    case Mod:chat(AgentUid, Messages, Opts) of
        {ok, RespMap} when is_map(RespMap) ->
            deliver_group_reply(ToGID, AgentUid, maps:get(<<"result">>, RespMap, <<>>));
        {error, Reason} ->
            ok = ?ERROR_LOG("[AGENT_GROUP_FAILED] gid=~p reason=~p~n", [ToGID, Reason]),
            ok
    end.

run_stream(Mod, Opts, ToGID, AgentUid, Messages, MemberUids) ->
    StreamId = ec_cnv:to_binary(elib_tsid:generate()),
    %% 流式 delta 是 ephemeral（离线成员本就收不到），只扇出给在线成员——避免大群逐帧
    %% 对全体（含离线）做 imboy_syn:publish 查找的放大（定稿仍经 c2g 投递全体/离线可补拉）。
    Observers = [U || U <- MemberUids, user_logic:is_online(U)],
    Ctx = #{
        stream_id => StreamId,
        observer_uids => Observers,
        from => ec_cnv:to_binary(AgentUid),
        to => ec_cnv:to_binary(ToGID),
        type => <<"C2G">>
    },
    StreamFun = fun(Delta) -> llm_stream:push(Ctx, Delta) end,
    case Mod:chat_stream(AgentUid, Messages, Opts, StreamFun) of
        {ok, RespMap} when is_map(RespMap) ->
            llm_stream:flush_tail(Ctx),
            deliver_group_reply(ToGID, AgentUid, StreamId, maps:get(<<"result">>, RespMap, <<>>));
        {error, Reason} ->
            ok = ?ERROR_LOG("[AGENT_GROUP_STREAM_FAILED] gid=~p reason=~p~n", [ToGID, Reason]),
            llm_stream:flush_tail(Ctx),
            deliver_group_reply(ToGID, AgentUid, StreamId, llm_stream:full_text(Ctx))
    end.

%% ===================================================================
%% Payment command (T4.3 ②：确定性支付指令，LLM 不进资金路径)
%% ===================================================================

%% 仅当：文本是支付指令 + 恰好 1 个被@ agent + 恰好 1 个非 agent 收款人 → 执行支付。
%% 否则 ignore（交回 LLM 路径，向后兼容）。
-spec try_pay_command(integer(), integer(), map(), binary(), [{integer(), map()}]) ->
    handled | ignore.
try_pay_command(FromUid, ToGID, Data, Text, [{AgentUid, _Agent}]) ->
    case agent_payment_command:parse_amount(Text) of
        {ok, AmountFen} ->
            case payee_mention(Data, AgentUid) of
                {ok, PayeeUid} ->
                    %% 确是结构化支付指令 → 先过金钱DoS限流闸门（与 LLM 路径同一闸门，
                    %% 防非 owner 高频刷 find_active/授权查询），放行再授权扣款。
                    case agent_rate_limiter:allow(AgentUid, FromUid) of
                        {deny, Reason} ->
                            ok = ?WARN_LOG(
                                "[AGENT_PAY_RATE_LIMITED] agent=~p from=~p reason=~p~n",
                                [AgentUid, FromUid, Reason]
                            ),
                            handled;
                        allow ->
                            do_pay_command(FromUid, ToGID, AgentUid, PayeeUid, AmountFen, Data)
                    end;
                error ->
                    ignore
            end;
        nomatch ->
            ignore
    end;
try_pay_command(_FromUid, _ToGID, _Data, _Text, _Agents) ->
    %% 0 或 >1 个 agent 被@：收款/执行者有歧义，不做支付，交回 LLM 路径
    ignore.

%% 收款人 = mentions 中唯一的「非 agent」uid（排除被@的 agent 自己）。
%% 恰好 1 个才认，0 个或多个视为歧义 → error。
-spec payee_mention(map(), integer()) -> {ok, integer()} | error.
payee_mention(Data, AgentUid) ->
    Payload = maps:get(<<"payload">>, Data, #{}),
    Mentions = maps:get(<<"mentions">>, Payload, []),
    Uids = lists:usort(
        lists:filtermap(
            fun(M) ->
                case to_uid(M) of
                    undefined -> false;
                    AgentUid -> false;
                    U -> {true, U}
                end
            end,
            Mentions
        )
    ),
    case Uids of
        [PayeeUid] -> {ok, PayeeUid};
        _ -> error
    end.

-spec do_pay_command(integer(), integer(), integer(), integer(), pos_integer(), map()) ->
    handled.
do_pay_command(FromUid, ToGID, AgentUid, PayeeUid, AmountFen, Data) ->
    MsgId = maps:get(<<"id">>, Data, <<>>),
    case agent_payment_command:authorize_and_pay(FromUid, AgentUid, PayeeUid, AmountFen, MsgId) of
        {ok, _Result} ->
            deliver_group_reply(ToGID, AgentUid, pay_ok_text(PayeeUid, AmountFen)),
            handled;
        {error, Reason} when Reason =:= not_authorized; Reason =:= mandate_invalid ->
            %% 授权阶段拒绝：静默丢弃（不回群、不泄露 mandate 是否存在/发起人是否 owner），
            %% 仅告警。也不落回 LLM 路径——这明显是一次支付企图，不该让 agent 闲聊回应。
            %% not_authorized 与 mandate_invalid 统一静默，避免二者差异构成"有无 mandate"oracle。
            ok = ?WARN_LOG(
                "[AGENT_PAY_DENIED] agent=~p from=~p gid=~p reason=~p~n",
                [AgentUid, FromUid, ToGID, Reason]
            ),
            handled;
        {error, Reason} ->
            %% 结算阶段失败（余额不足/超限额等，均已过 owner 授权）→ 可回执给 owner
            deliver_group_reply(ToGID, AgentUid, pay_fail_text(Reason)),
            handled
    end.

pay_ok_text(PayeeUid, AmountFen) ->
    iolist_to_binary([
        <<"已向 "/utf8>>,
        ec_cnv:to_binary(PayeeUid),
        <<" 付款 "/utf8>>,
        yuan(AmountFen),
        <<" 元"/utf8>>
    ]).

pay_fail_text(insufficient_balance) -> <<"支付失败：余额不足"/utf8>>;
pay_fail_text(exceeds_single_limit) -> <<"支付失败：超单笔限额"/utf8>>;
pay_fail_text(exceeds_total_limit) -> <<"支付失败：超周期累计限额"/utf8>>;
pay_fail_text(mandate_invalid) -> <<"支付失败：无有效支付授权"/utf8>>;
pay_fail_text(invalid_payee) -> <<"支付失败：收款人无效"/utf8>>;
pay_fail_text(_) -> <<"支付失败"/utf8>>.

%% 分 → "元.角分" 展示（整数运算，不经 float）
yuan(Fen) ->
    iolist_to_binary(io_lib:format("~B.~2..0B", [Fen div 100, Fen rem 100])).

%% ===================================================================
%% Internal
%% ===================================================================

deliver_group_reply(ToGID, AgentUid, Result) ->
    deliver_group_reply(ToGID, AgentUid, ec_cnv:to_binary(elib_tsid:generate()), Result).

deliver_group_reply(_ToGID, _AgentUid, _MsgId, <<>>) ->
    ok;
deliver_group_reply(ToGID, AgentUid, MsgId, Result) ->
    Content = elib_str:replace_single_quote(Result),
    %% agent 作为一等群成员，经 msg_c2g_logic:c2g 正常投递（复用 QoS/归档/成员扇出）。
    %% agent 回复不含 @agent，故不会自触发。
    Data = #{
        <<"to">> => ec_cnv:to_binary(ToGID),
        <<"msg_type">> => <<"text">>,
        <<"payload">> => #{<<"content">> => Content, <<"text">> => Content},
        <<"created_at">> => elib_dt:millisecond()
    },
    _ = msg_c2g_logic:c2g(MsgId, AgentUid, Data),
    ok.

%% 从 payload.mentions 里筛出「是启用中 agent」的被 @ 成员，返回 [{Uid, AgentMap}]
-spec mentioned_agents(map()) -> [{integer(), map()}].
mentioned_agents(Data) ->
    Payload = maps:get(<<"payload">>, Data, #{}),
    Mentions = maps:get(<<"mentions">>, Payload, []),
    Agents = lists:filtermap(
        fun(M) ->
            case to_uid(M) of
                undefined ->
                    false;
                Uid ->
                    case ai_agent_ds:is_agent(Uid) of
                        {true, Agent} -> {true, {Uid, Agent}};
                        false -> false
                    end
            end
        end,
        Mentions
    ),
    %% 按 Uid 去重：防同一条消息 mentions=[42,42,...] 单帧触发 N 次 LLM、一次烧光限流预算
    %% （不同表示如 42 与 <<"42">> 归一到同一 Uid 后由 ukeysort 折叠）。
    lists:ukeysort(1, Agents).

%% mention 项可能是 uid（integer/binary）或 <<"all">>；<<"all">> 与非数值忽略
to_uid(<<"all">>) ->
    undefined;
to_uid(N) when is_integer(N), N > 0 -> N;
to_uid(B) when is_binary(B) ->
    try ec_cnv:to_integer(B) of
        I when is_integer(I), I > 0 -> I;
        _ -> undefined
    catch
        _:_ -> undefined
    end;
to_uid(_) ->
    undefined.

is_agent_sender(FromUid) ->
    case ai_agent_ds:is_agent(FromUid) of
        {true, _} -> true;
        false -> false
    end.

build_messages(Agent, Text) ->
    User = #{<<"role">> => <<"user">>, <<"content">> => Text},
    case maps:get(<<"system_prompt">>, Agent, <<>>) of
        SP when is_binary(SP), SP =/= <<>> ->
            [#{<<"role">> => <<"system">>, <<"content">> => SP}, User];
        _ ->
            [User]
    end.

merge_model(Opts, Agent) ->
    case maps:get(<<"model">>, Agent, <<>>) of
        M when is_binary(M), M =/= <<>> -> Opts#{model => M};
        _ -> Opts
    end.

is_e2ee(Data) ->
    E2EE = maps:get(<<"e2ee">>, Data, null),
    MsgType = maps:get(<<"msg_type">>, Data, <<>>),
    (is_map(E2EE) andalso map_size(E2EE) > 0) orelse MsgType =:= <<"e2ee">>.

extract_text(Data) ->
    Payload = maps:get(<<"payload">>, Data, #{}),
    case Payload of
        P when is_map(P) ->
            first_nonempty([maps:get(<<"content">>, P, <<>>), maps:get(<<"text">>, P, <<>>)]);
        _ ->
            <<>>
    end.

first_nonempty([B | _]) when is_binary(B), B =/= <<>> -> B;
first_nonempty([_ | Rest]) -> first_nonempty(Rest);
first_nonempty([]) -> <<>>.
