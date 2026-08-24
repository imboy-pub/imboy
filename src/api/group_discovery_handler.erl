-module(group_discovery_handler).
%%%
% group_discovery_handler 是群组发现 API 处理器
% 处理公开群搜索、发现、分类浏览等 HTTP REST API 请求
%%%

-dialyzer({nowarn_function, [init/2, handle_action/3]}).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化群组发现处理器
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
handle_action(hot, Req, State) -> hot(Req, State);
handle_action(categories, Req, State) -> categories(Req, State);
handle_action(preview, Req, State) -> preview(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action Handlers
%% ===================================================================

%% @doc 搜索公开群组
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    Keyword = proplists:get_value(<<"q">>, Qs, <<>>),
    CategoryId = safe_int_qs(<<"category_id">>, Qs),
    {ok, Page0} = elib_param:int(page, Req0, 1),
    {ok, Size0} = elib_param:int(size, Req0, 20),
    Page = positive_integer(Page0, 1),
    Size = positive_integer(Size0, 20),

    case group_discovery_logic:search(Keyword, Page, Size, CategoryId) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 发现页群组列表
-spec discover(cowboy_req:req(), map()) -> cowboy_req:req().
discover(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    CategoryId = safe_int_qs(<<"category_id">>, Qs),
    Sort = proplists:get_value(<<"sort">>, Qs, <<"popular">>),
    {ok, Page0} = elib_param:int(page, Req0, 1),
    {ok, Size0} = elib_param:int(size, Req0, 20),
    Page = positive_integer(Page0, 1),
    Size = positive_integer(Size0, 20),

    case group_discovery_logic:discover(Page, Size, CategoryId, Sort) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 精选群组
-spec featured(cowboy_req:req(), map()) -> cowboy_req:req().
featured(Req0, _State) ->
    {ok, Limit0} = elib_param:int(limit, Req0, 10),
    Limit = positive_integer(Limit0, 10),

    case group_discovery_logic:featured(Limit) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 热门群组
-spec hot(cowboy_req:req(), map()) -> cowboy_req:req().
hot(Req0, _State) ->
    {ok, Limit0} = elib_param:int(limit, Req0, 20),
    Limit = positive_integer(Limit0, 20),

    case group_discovery_logic:hot(Limit) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

-spec positive_integer(integer(), pos_integer()) -> pos_integer().
positive_integer(Value, _Default) when Value > 0 -> Value;
positive_integer(_, Default) -> Default.

%% @doc 公开群分类列表
-spec categories(cowboy_req:req(), map()) -> cowboy_req:req().
categories(Req0, _State) ->
    case group_discovery_logic:categories() of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 群组预览（公开访问，无需加入）
-spec preview(cowboy_req:req(), map()) -> cowboy_req:req().
preview(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),

    GroupId = safe_int_qs(<<"group_id">>, Qs),

    case GroupId of
        undefined ->
            elib_response:error(Req0, <<"群组ID不能为空"/utf8>>);
        GroupId2 when GroupId2 > 0 ->
            case group_discovery_logic:preview(GroupId2) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, Reason} ->
                    elib_response:error(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"无效的群组ID"/utf8>>)
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
