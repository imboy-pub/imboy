-module(agent_trigger_policy).

%%%
% Agent 群触发策略 / Agent group trigger policy（Phase 1 T1.5）
%
% 纯函数：给定 agent 的 trigger_policy 与一条群消息的上下文，判定 agent 是否应回复。
% 目的：群里防刷屏——默认「仅被 @ 才回」，普通消息不回。
%
% Policy（来自 ai_agent.trigger_policy jsonb，binary key）：
%   <<"mention">>         boolean，被 @ 时触发（默认 true = 安全默认）
%   <<"suffix_q">>        boolean，文本以 ? / ？ 结尾时触发（默认 false）
%   <<"keywords">>        [binary]，文本包含任一关键词时触发（默认 []）
%   <<"group_allowlist">> [integer]，非空时仅在这些群触发（默认 [] = 不限群）
%
% Ctx（由 msg_c2g_logic 前置组装，atom key）：
%   mentioned => boolean，本条消息是否 @ 了该 agent
%   text      => binary，消息文本
%   group_id  => integer，所在群
%%%

-export([should_trigger/2]).

%% @doc 判定 agent 是否应回复该群消息
-spec should_trigger(map(), map()) -> boolean().
should_trigger(Policy, Ctx) when is_map(Policy), is_map(Ctx) ->
    allowed_group(Policy, Ctx) andalso
        (mention_trigger(Policy, Ctx) orelse
            suffix_trigger(Policy, Ctx) orelse
            keyword_trigger(Policy, Ctx)).

%% ===================================================================
%% Internal
%% ===================================================================

%% 群白名单门：空 = 不限群；非空 = 仅命中群放行
-spec allowed_group(map(), map()) -> boolean().
allowed_group(Policy, Ctx) ->
    case normalize_ids(maps:get(<<"group_allowlist">>, Policy, [])) of
        [] -> true;
        Ids -> lists:member(ec_cnv:to_integer(maps:get(group_id, Ctx, 0)), Ids)
    end.

%% 被 @ 触发（默认开启，安全默认）
-spec mention_trigger(map(), map()) -> boolean().
mention_trigger(Policy, Ctx) ->
    truthy(maps:get(<<"mention">>, Policy, true)) andalso
        maps:get(mentioned, Ctx, false) =:= true.

%% 疑问句结尾触发
-spec suffix_trigger(map(), map()) -> boolean().
suffix_trigger(Policy, Ctx) ->
    truthy(maps:get(<<"suffix_q">>, Policy, false)) andalso
        ends_with_question(maps:get(text, Ctx, <<>>)).

%% 关键词触发
-spec keyword_trigger(map(), map()) -> boolean().
keyword_trigger(Policy, Ctx) ->
    Text = maps:get(text, Ctx, <<>>),
    lists:any(
        fun(K) -> is_binary(K) andalso K =/= <<>> andalso binary:match(Text, K) =/= nomatch end,
        maps:get(<<"keywords">>, Policy, [])
    ).

%% 文本去尾空白后是否以半角 ? 或全角 ？ 结尾
-spec ends_with_question(binary()) -> boolean().
ends_with_question(Text) when is_binary(Text) ->
    T = string:trim(Text, trailing),
    has_suffix(T, <<"?">>) orelse has_suffix(T, <<"？"/utf8>>);
ends_with_question(_) ->
    false.

-spec has_suffix(binary(), binary()) -> boolean().
has_suffix(Bin, Suf) ->
    SS = byte_size(Suf),
    BS = byte_size(Bin),
    BS >= SS andalso binary:part(Bin, BS - SS, SS) =:= Suf.

-spec normalize_ids(list()) -> [integer()].
normalize_ids(List) when is_list(List) ->
    [ec_cnv:to_integer(X) || X <- List].

-spec truthy(term()) -> boolean().
truthy(true) -> true;
truthy(<<"true">>) -> true;
truthy(1) -> true;
truthy(_) -> false.
