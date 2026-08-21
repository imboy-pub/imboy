-module(ai_agent_handler).

%%%
% AI 助手发现控制器 / AI assistant discovery controller
%
% 路由（客户端 baseURL=/api/v1）：
%   GET /api/v1/agent/list       -> 分页列出可发起 C2S 会话的助手（keyword 可选）
%   GET /api/v1/agent/discover   -> 分页公开发现（同 list，语义别名）
%   GET /api/v1/agent/search     -> 按关键词搜索助手
%   GET /api/v1/agent/categories -> 列出公开助手分类
%
% 认证：进 ApiV1Routes 认证块，须携带有效 JWT。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            list -> list(cowboy_req:method(Req0), Req0, State);
            discover -> list(cowboy_req:method(Req0), Req0, State);
            search -> search(cowboy_req:method(Req0), Req0, State);
            categories -> categories(cowboy_req:method(Req0), Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"kwd">>, Qs, <<>>),
    case ai_agent_logic:list_assistants(#{page => Page, size => Size, keyword => Keyword}) of
        {ok, Paged} ->
            elib_response:success(Req0, Paged);
        {error, _} ->
            elib_response:error(Req0, <<"读取助手列表失败"/utf8>>, ?ERR_BAD_REQUEST)
    end;
list(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"q">>, Qs, <<>>),
    case ai_agent_logic:list_assistants(#{page => Page, size => Size, keyword => Keyword}) of
        {ok, Paged} ->
            elib_response:success(Req0, Paged);
        {error, _} ->
            elib_response:error(Req0, <<"搜索助手失败"/utf8>>, ?ERR_BAD_REQUEST)
    end;
search(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec categories(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
categories(<<"GET">>, Req0, _State) ->
    case ai_agent_logic:categories() of
        {ok, List} ->
            elib_response:success(Req0, #{<<"categories">> => List});
        {error, _} ->
            elib_response:error(Req0, <<"读取分类失败"/utf8>>, ?ERR_BAD_REQUEST)
    end;
categories(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
