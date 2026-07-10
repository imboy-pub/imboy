-module(agent_payment_command).

%%%
% Agent 群内确定性支付指令解析与执行 / Deterministic in-group agent payment command
% （Phase 4 T4.3 ②：群 @agent 触发受控支付）。
%
% ⚠️ LLM 绝不进资金路径：仅当群消息文本是**结构化支付指令**（含支付关键词 + 金额）
%    时，才凭 agent 的 mandate 受控扣款；普通消息仍走 LLM 回复路径。
%
% 安全红线：
%   - **只有 mandate.owner_uid 能命令 agent 花自己的钱**（付款人=owner）。发起人
%     （群消息认证发送者 FromUid）必须 = owner_uid，否则拒绝——防任意群成员 @bot
%     掏空 owner 钱包。
%   - 金额一律「元→分」整数运算，绝不经 float（避免精度丢钱）。
%   - RefNo 由群消息 MsgId 派生 → 天然幂等（同一指令消息重投不重复扣款）；
%     且不以 "_CR" 结尾（agent_payment_logic 保留后缀），过其入参校验。
%%%

-export([parse_amount/1]).
-export([authorize_and_pay/5]).

%% 支付指令关键词，且必须带**右边界**（其后为 空白/数字/@/冒号/结尾），
%% 杜绝复合词子串误触发真实转账：
%%   "支付宝"(支付+宝)、"转账记录"(转账+记)、"payment"(pay+ment)、"paypal"(pay+pal) 均不算指令。
-define(CMD_RE, <<"(?:付款|转账|支付|pay|transfer)(?=[\\s0-9@:：]|$)"/utf8>>).

%% @doc 解析支付金额：文本须含支付关键词 + 一个金额（元，最多 2 位小数）。
%% 金额转「分」全程整数运算，绝不经 float。
%% @returns {ok, AmountFen::pos_integer()} | nomatch
-spec parse_amount(binary()) -> {ok, pos_integer()} | nomatch.
parse_amount(Text) when is_binary(Text) ->
    case has_command(Text) of
        false -> nomatch;
        true -> extract_amount(Text)
    end;
parse_amount(_) ->
    nomatch.

%% @doc 授权校验 + 受控扣款。FromUid 必须 = agent mandate 的 owner_uid
%% （只有 owner 能命令 agent 花自己的钱）。RefNo 由 MsgId 派生保证幂等。
%% @returns {ok, ResultMap} | {error, Reason}
%%   Reason: not_authorized | mandate_invalid | pay_with_mandate 的错误原子
-spec authorize_and_pay(integer(), integer(), integer(), pos_integer(), binary() | integer()) ->
    {ok, map()} | {error, atom()}.
authorize_and_pay(FromUid, AgentUid, PayeeUid, AmountFen, MsgId) ->
    case agent_payment_mandate_ds:find_active(AgentUid) of
        {ok, Mandate} ->
            OwnerUid = to_int(maps:get(<<"owner_uid">>, Mandate)),
            case FromUid =:= OwnerUid of
                true ->
                    RefNo = <<"AGP_", (ec_cnv:to_binary(MsgId))/binary>>,
                    agent_payment_logic:pay_with_mandate(AgentUid, PayeeUid, AmountFen, RefNo);
                false ->
                    {error, not_authorized}
            end;
        {error, _} ->
            {error, mandate_invalid}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

has_command(Text) ->
    re:run(Text, ?CMD_RE, [unicode, {capture, none}]) =:= match.

%% 提取金额（元）→ 分。匹配「整数[.小数1~2位]」，取第一处。
extract_amount(Text) ->
    case re:run(Text, "([0-9]+)(?:\\.([0-9]{1,2}))?", [{capture, all_but_first, binary}]) of
        {match, [YuanBin]} -> to_fen(YuanBin, <<>>);
        {match, [YuanBin, FenBin]} -> to_fen(YuanBin, FenBin);
        nomatch -> nomatch
    end.

to_fen(YuanBin, FenBin) ->
    Yuan = binary_to_integer(YuanBin),
    Fen = Yuan * 100 + normalize_fen(FenBin),
    case Fen > 0 of
        true -> {ok, Fen};
        false -> nomatch
    end.

%% FenBin 由 re 保证是 0~2 位数字：""=0 分；单位=角(×10)；两位=分
normalize_fen(<<>>) -> 0;
normalize_fen(<<A>>) -> (A - $0) * 10;
normalize_fen(<<A, B>>) -> (A - $0) * 10 + (B - $0).

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) -> binary_to_integer(V);
to_int(_) -> 0.
