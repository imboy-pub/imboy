-module(ai_agent_kb_logic).

%%%
%%% AI 知识库配置 / AI knowledge-base config（P0-3 A3-1）
%%%
%%% 运营者在管理后台粘贴群规/FAQ，存 config 表，供 @管家 答疑时注入上下文
%%% （注入点在 ai_agent_reply / ai_agent_group_reply / ai_agent_proactive 的
%%% build_messages，A3-2 落地）。
%%%
%%% 设计与 user_onboarding_logic 同构：config 表 KV-store（text+JSON），白名单
%%% 半量更新，任一非法整体拒绝零写入，未知键忽略。
%%%
%%% 命名空间：ai_agent.kb.*（config.key varchar(40) 足够）。
%%%

-export([get_config/0, put_config/1]).
%% 供 Agent 运行时按策略读取有限知识库上下文
-export([kb_text/0, context/2]).

-include("log.hrl").

%% config 表键（管理后台 /api/adm/ai_agent/knowledge_config 可配）
-define(CFG_ENABLED, <<"ai_agent.kb.enabled">>).
-define(CFG_GROUP_RULE, <<"ai_agent.kb.group_rule">>).
-define(CFG_FAQ, <<"ai_agent.kb.faq">>).

%% 配置写入约束（群规/FAQ 都是运营长文本，放宽到 8000 字节）
-define(MAX_KB_BYTES, 8000).
-define(DEFAULT_CONTEXT_BYTES, 2400).

%% ===================================================================
%% Admin 配置读写（白名单键 + 类型校验；半量更新；未知键忽略）
%% ===================================================================

%% @doc 读取知识库全量配置（admin 视图，binary 键）
-spec get_config() -> map().
get_config() ->
    #{
        <<"enabled">> => config_ds:get(?CFG_ENABLED, false),
        <<"group_rule">> => config_ds:get(?CFG_GROUP_RULE, <<>>),
        <<"faq">> => config_ds:get(?CFG_FAQ, <<>>)
    }.

%% @doc 半量写入知识库配置：白名单键逐个校验，任一非法则整体拒绝
%% （零写入）；未知键忽略。成功返回 {ok, Patch}。
-spec put_config(map()) -> {ok, map()} | {error, binary()}.
put_config(Patch) when is_map(Patch) ->
    case validate_patch(maps:to_list(Patch)) of
        ok ->
            maps:foreach(fun write_key/2, Patch),
            {ok, Patch};
        {error, _} = Err ->
            Err
    end.

%% @doc 供 agent 回复链路注入：拼好的知识库文本。
%% enabled=false 或全空 → <<>>（注入点据此判空跳过）。
%% 任何异常恒返回 <<>>，绝不拖垮回复主链路。
-spec kb_text() -> binary().
kb_text() ->
    kb_text(<<"all">>).

kb_text(Source) ->
    try
        case config_ds:get(?CFG_ENABLED, false) of
            true ->
                GroupRule = config_ds:get(?CFG_GROUP_RULE, <<>>),
                Faq = config_ds:get(?CFG_FAQ, <<>>),
                assemble_source(Source, GroupRule, Faq);
            _ ->
                <<>>
        end
    catch
        Class:Reason ->
            ?WARN_LOG("[KB] read config failed ~p:~p~n", [Class, Reason]),
            <<>>
    end.

%% @doc 按角色知识策略取上下文。
%% off：完全不读取知识库；on_demand：只返回命中问题关键词的行；
%% required：返回知识库前缀，但仍受 max_context_bytes 限制。
%% 未绑定新角色的旧 agent 走 required 兼容路径，角色模板会显式写入 on_demand。
-spec context(map(), binary()) -> binary().
context(Agent, Query) when is_map(Agent), is_binary(Query) ->
    Policy = maps:get(
        <<"knowledge_policy">>,
        Agent,
        #{<<"knowledge">> => #{<<"mode">> => <<"required">>}}
    ),
    Knowledge = maps:get(<<"knowledge">>, Policy, #{}),
    Mode = maps:get(<<"mode">>, Knowledge, <<"required">>),
    Source = maps:get(<<"source">>, Knowledge, <<"all">>),
    MaxBytes = bounded_limit(maps:get(<<"max_context_bytes">>, Knowledge, ?DEFAULT_CONTEXT_BYTES)),
    Result =
        case ai_agent_policy:allows(Agent, <<"knowledge">>) of
            false ->
                <<>>;
            true ->
                case Mode of
                    <<"off">> ->
                        <<>>;
                    <<"on_demand">> ->
                        select_matching_context(kb_text(Source), Query, MaxBytes);
                    <<"required">> ->
                        bounded(kb_text(Source), MaxBytes);
                    _ ->
                        <<>>
                end
        end,
    log_context(Agent, Mode, Source, Query, Result),
    Result;
context(_, _) ->
    <<>>.

log_context(Agent, Mode, Source, Query, Result) ->
    Attempted = Mode =/= <<"off">> andalso Query =/= <<>>,
    Hit = Result =/= <<>>,
    SkipReason =
        case {Mode, Query, Hit} of
            {<<"off">>, _, _} -> <<"mode_off">>;
            {_, <<>>, _} -> <<"empty_query">>;
            {_, _, true} -> <<>>;
            _ -> <<"no_match_or_empty">>
        end,
    Event = #{
        event => knowledge_context,
        agent_role => maps:get(<<"role_code">>, Agent, <<"legacy_fallback">>),
        knowledge_attempted => Attempted,
        knowledge_hit => Hit,
        source => Source,
        context_bytes => byte_size(Result),
        chunks => count_chunks(Result),
        skip_reason => SkipReason
    },
    try
        ?INFO_LOG([ai_agent_knowledge, Event])
    catch
        _:_ -> ok
    end,
    ok.

count_chunks(<<>>) ->
    0;
count_chunks(Context) ->
    length(binary:split(Context, <<"\n">>, [global])).

-spec assemble(binary(), binary()) -> binary().
assemble(<<>>, <<>>) ->
    <<>>;
assemble(GroupRule, <<>>) ->
    <<"【群规】\n"/utf8, GroupRule/binary>>;
assemble(<<>>, Faq) ->
    <<"【常见问答】\n"/utf8, Faq/binary>>;
assemble(GroupRule, Faq) ->
    %% 两段非空：中间空行分隔
    <<
        "【群规】\n"/utf8,
        GroupRule/binary,
        "\n\n【常见问答】\n"/utf8,
        Faq/binary
    >>.

assemble_source(<<"faq">>, _GroupRule, Faq) ->
    assemble(<<>>, Faq);
assemble_source(<<"group_rule">>, GroupRule, _Faq) ->
    assemble(GroupRule, <<>>);
assemble_source(_, GroupRule, Faq) ->
    assemble(GroupRule, Faq).

-spec select_matching_context(binary(), binary(), non_neg_integer()) -> binary().
select_matching_context(<<>>, _Query, _MaxBytes) ->
    <<>>;
select_matching_context(_Kb, <<>>, _MaxBytes) ->
    <<>>;
select_matching_context(Kb, Query, MaxBytes) ->
    Lines = binary:split(Kb, <<"\n">>, [global]),
    Matched = [Line || Line <- Lines, line_matches(Line, Query)],
    bounded(join_lines(Matched), MaxBytes).

-spec line_matches(binary(), binary()) -> boolean().
line_matches(<<>>, _Query) ->
    false;
line_matches(_Line, <<>>) ->
    false;
line_matches(<<_:8, _/binary>> = Line, <<_:8, _/binary>> = Query) ->
    case binary:match(Line, Query) of
        nomatch ->
            Tokens = string:lexemes(binary_to_list(Query), " \t\r\n,.;:!?，。！？："),
            lists:any(
                fun(Token) ->
                    case unicode:characters_to_binary(Token) of
                        <<_:16, _/binary>> = TokenBin -> binary:match(Line, TokenBin) =/= nomatch;
                        _ -> false
                    end
                end,
                Tokens
            );
        _ ->
            true
    end.

-spec join_lines([binary()]) -> binary().
join_lines([]) ->
    <<>>;
join_lines(Lines) ->
    iolist_to_binary(lists:join(<<"\n">>, Lines)).

-spec bounded(binary(), non_neg_integer()) -> binary().
bounded(<<>>, _MaxBytes) ->
    <<>>;
bounded(Value, MaxBytes) when byte_size(Value) =< MaxBytes ->
    Value;
bounded(Value, MaxBytes) ->
    binary:part(Value, 0, MaxBytes).

-spec bounded_limit(term()) -> non_neg_integer().
bounded_limit(Value) when is_integer(Value), Value >= 0, Value =< ?MAX_KB_BYTES ->
    Value;
bounded_limit(_) ->
    ?DEFAULT_CONTEXT_BYTES.

-spec validate_patch([{binary(), term()}]) -> ok | {error, binary()}.
validate_patch([]) ->
    ok;
validate_patch([{K, V} | Rest]) ->
    case validate_key(K, V) of
        ok -> validate_patch(Rest);
        {error, _} = Err -> Err
    end.

-spec validate_key(binary(), term()) -> ok | {error, binary()}.
validate_key(<<"enabled">>, V) when is_boolean(V) ->
    ok;
validate_key(<<"group_rule">>, V) when is_binary(V), byte_size(V) =< ?MAX_KB_BYTES ->
    ok;
validate_key(<<"faq">>, V) when is_binary(V), byte_size(V) =< ?MAX_KB_BYTES ->
    ok;
validate_key(K, _V) ->
    case cfg_key(K) of
        undefined -> ok;
        _ -> {error, <<K/binary, " 值非法"/utf8>>}
    end.

-spec write_key(binary(), term()) -> ok.
write_key(K, V) ->
    case cfg_key(K) of
        undefined -> ok;
        CfgKey -> config_ds:set(CfgKey, V)
    end.

%% admin 视图键 → config 表键映射（白名单）
-spec cfg_key(binary()) -> binary() | undefined.
cfg_key(<<"enabled">>) -> ?CFG_ENABLED;
cfg_key(<<"group_rule">>) -> ?CFG_GROUP_RULE;
cfg_key(<<"faq">>) -> ?CFG_FAQ;
cfg_key(_) -> undefined.
