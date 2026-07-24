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
%% 供 A3-2 build_messages 注入：读拼好的知识库文本（enabled=false 或空 → <<>>）
-export([kb_text/0]).

-include("log.hrl").

%% config 表键（管理后台 /api/adm/ai_agent/knowledge_config 可配）
-define(CFG_ENABLED, <<"ai_agent.kb.enabled">>).
-define(CFG_GROUP_RULE, <<"ai_agent.kb.group_rule">>).
-define(CFG_FAQ, <<"ai_agent.kb.faq">>).

%% 配置写入约束（群规/FAQ 都是运营长文本，放宽到 8000 字节）
-define(MAX_KB_BYTES, 8000).

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
    try
        case config_ds:get(?CFG_ENABLED, false) of
            true ->
                GroupRule = config_ds:get(?CFG_GROUP_RULE, <<>>),
                Faq = config_ds:get(?CFG_FAQ, <<>>),
                assemble(GroupRule, Faq);
            _ ->
                <<>>
        end
    catch
        Class:Reason ->
            ?WARN_LOG("[KB] read config failed ~p:~p~n", [Class, Reason]),
            <<>>
    end.

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
