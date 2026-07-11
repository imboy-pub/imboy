-module(ai_agent_logic).

%%%
% AI 助手发现业务逻辑 / AI assistant discovery logic
%
% 职责：面向普通用户列出「可发起 C2S 会话的助手」，把 repo 返回的存储行
%       映射为前端卡片 {id, name, avatar, description}。可见性口径由 repo 收口
%       （status=1 且 visibility=1 公开可发现），本层只做字段投影，不做权限判定。
%%%

-export([list_assistants/1]).

-include("log.hrl").

%% @doc 分页列出可发现助手。
%% 入参 map：page(必填) size(必填) keyword?（按 nickname 模糊）
%% 返回分页信封，list 内每项为 #{id, name, avatar, description}。
-spec list_assistants(map()) -> {ok, map()} | {error, term()}.
list_assistants(#{page := Page, size := Size} = Params) ->
    Keyword = maps:get(keyword, Params, <<>>),
    case ai_agent_repo:page_assistants(Keyword, Page, Size) of
        {ok, #{list := Rows} = Paged} ->
            {ok, Paged#{list => [to_card(R) || R <- Rows]}};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_logic:list_assistants error ~p~n", [Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 存储行 → 前端助手卡片（description 取 ai_agent.description 真实列）
-spec to_card(map()) -> map().
to_card(Row) ->
    #{
        <<"id">> => maps:get(<<"user_id">>, Row),
        <<"name">> => maps:get(<<"nickname">>, Row, <<>>),
        <<"avatar">> => maps:get(<<"avatar">>, Row, <<>>),
        <<"description">> => maps:get(<<"description">>, Row, <<>>)
    }.
