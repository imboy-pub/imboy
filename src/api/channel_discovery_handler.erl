-module(channel_discovery_handler).
%%%
% channel_discovery_handler 是频道发现 API 处理器
% 处理频道搜索、发现、分类浏览、热门频道等 HTTP REST API 请求
%%%

-dialyzer({nowarn_function, [init/2, handle_action/3]}).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化频道发现处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(search, Req, State) -> search(Req, State);
handle_action(discover, Req, State) -> discover(Req, State);
handle_action(featured, Req, State) -> featured(Req, State);
handle_action(trending, Req, State) -> trending(Req, State);
handle_action(categories, Req, State) -> categories(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action Handlers
%% ===================================================================

%% @doc 搜索频道（全文搜索）
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    Keyword = proplists:get_value(<<"q">>, Qs, <<>>),
    CategoryId = safe_int_qs(<<"category_id">>, Qs),
    {ok, Page} = elib_param:int(page, Req0, 1),
    {ok, Size} = elib_param:int(size, Req0, 20),

    case channel_discovery_logic:search(Keyword, Page, Size, CategoryId) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 发现页频道列表
-spec discover(cowboy_req:req(), map()) -> cowboy_req:req().
discover(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    CategoryId = safe_int_qs(<<"category_id">>, Qs),
    Sort = proplists:get_value(<<"sort">>, Qs, <<"popular">>),
    {ok, Page} = elib_param:int(page, Req0, 1),
    {ok, Size} = elib_param:int(size, Req0, 20),

    case channel_discovery_logic:discover(Page, Size, CategoryId, Sort) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 精选频道
-spec featured(cowboy_req:req(), map()) -> cowboy_req:req().
featured(Req0, _State) ->
    {ok, Limit} = elib_param:int(limit, Req0, 10),

    case channel_discovery_logic:featured(Limit) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 热门频道
-spec trending(cowboy_req:req(), map()) -> cowboy_req:req().
trending(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    Period = case proplists:get_value(<<"period">>, Qs) of
        <<"30d">> -> 30;
        _ -> 7
    end,
    {ok, Limit} = elib_param:int(limit, Req0, 20),

    case channel_discovery_logic:trending(Period, Limit) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 频道分类列表
-spec categories(cowboy_req:req(), map()) -> cowboy_req:req().
categories(Req0, _State) ->
    case channel_discovery_logic:categories() of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 安全地从查询字符串中获取整数值
-spec safe_int_qs(binary(), list()) -> integer() | undefined.
safe_int_qs(Key, Qs) ->
    case proplists:get_value(Key, Qs) of
        undefined -> undefined;
        Val -> elib_cnv:safe_to_integer(Val)
    end.