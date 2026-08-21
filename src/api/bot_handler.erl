-module(bot_handler).
%%%
% bot_handler 是 Bot 管理 API 处理器
% 处理 Bot 注册、查询、更新、启停等 HTTP REST API 请求
%%%

-dialyzer({nowarn_function, [init/2, handle_action/3]}).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

-compile({no_auto_import, [register/2]}).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(register, Req, State) -> register(Req, State);
handle_action(get, Req, State) -> get(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(disable, Req, State) -> disable(Req, State);
handle_action(enable, Req, State) -> enable(Req, State);
handle_action(list_mine, Req, State) -> list_mine(Req, State);
handle_action(search, Req, State) -> search(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action Handlers
%% ===================================================================

%% @doc 注册 Bot
-spec register(cowboy_req:req(), map()) -> cowboy_req:req().
register(Req0, _State) ->
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            Name = maps:get(<<"name">>, Body, <<>>),
            Username = maps:get(<<"username">>, Body, <<>>),
            OwnerUid = maps:get(<<"owner_uid">>, Body, 0),
            Data = #{
                name => Name,
                username => Username,
                owner_uid => OwnerUid,
                description => maps:get(<<"description">>, Body, <<>>),
                avatar => maps:get(<<"avatar">>, Body, <<>>),
                webhook_url => maps:get(<<"webhook_url">>, Body, <<>>),
                commands => maps:get(<<"commands">>, Body, <<"[]">>),
                permissions => maps:get(<<"permissions">>, Body, <<"[]">>),
                events => maps:get(<<"events">>, Body, <<"[]">>),
                is_public => maps:get(<<"is_public">>, Body, false)
            },
            case bot_logic:register(Data) of
                {ok, Result} ->
                    elib_response:success(Req1, Result);
                {error, Reason} ->
                    elib_response:error(Req1, Reason)
            end;
        {error, _} = Err ->
            elib_response:error(Req0, Err)
    end.

%% @doc 获取 Bot 信息
-spec get(cowboy_req:req(), map()) -> cowboy_req:req().
get(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    BotId = safe_int_qs(<<"bot_id">>, Qs),
    case BotId of
        undefined ->
            elib_response:error(Req0, <<"Bot ID 不能为空"/utf8>>);
        BotId2 when BotId2 > 0 ->
            case bot_logic:get(BotId2) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, Reason} ->
                    elib_response:error(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"无效的 Bot ID"/utf8>>)
    end.

%% @doc 更新 Bot 信息
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, _State) ->
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            BotId = maps:get(<<"bot_id">>, Body, 0),
            case BotId > 0 of
                true ->
                    Data = maps:with(
                        [
                            <<"name">>,
                            <<"username">>,
                            <<"description">>,
                            <<"avatar">>,
                            <<"webhook_url">>,
                            <<"commands">>,
                            <<"permissions">>,
                            <<"events">>,
                            <<"is_public">>
                        ],
                        Body
                    ),
                    case bot_logic:update(BotId, Data) of
                        {ok, Result} ->
                            elib_response:success(Req1, Result);
                        {error, Reason} ->
                            elib_response:error(Req1, Reason)
                    end;
                false ->
                    elib_response:error(Req1, <<"Bot ID 不能为空"/utf8>>)
            end;
        {error, _} = Err ->
            elib_response:error(Req0, Err)
    end.

%% @doc 停用 Bot
-spec disable(cowboy_req:req(), map()) -> cowboy_req:req().
disable(Req0, _State) ->
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            BotId = maps:get(<<"bot_id">>, Body, 0),
            case BotId > 0 of
                true ->
                    case bot_logic:set_status(BotId, 0) of
                        {ok, Result} ->
                            elib_response:success(Req1, Result);
                        {error, Reason} ->
                            elib_response:error(Req1, Reason)
                    end;
                false ->
                    elib_response:error(Req1, <<"Bot ID 不能为空"/utf8>>)
            end;
        {error, _} = Err ->
            elib_response:error(Req0, Err)
    end.

%% @doc 启用 Bot
-spec enable(cowboy_req:req(), map()) -> cowboy_req:req().
enable(Req0, _State) ->
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            BotId = maps:get(<<"bot_id">>, Body, 0),
            case BotId > 0 of
                true ->
                    case bot_logic:set_status(BotId, 1) of
                        {ok, Result} ->
                            elib_response:success(Req1, Result);
                        {error, Reason} ->
                            elib_response:error(Req1, Reason)
                    end;
                false ->
                    elib_response:error(Req1, <<"Bot ID 不能为空"/utf8>>)
            end;
        {error, _} = Err ->
            elib_response:error(Req0, Err)
    end.

%% @doc 列出我的 Bot
-spec list_mine(cowboy_req:req(), map()) -> cowboy_req:req().
list_mine(Req0, State) ->
    CurrentUid = proplists:get_value(current_uid, State, 0),
    {ok, Page} = elib_param:int(page, Req0, 1),
    case bot_logic:list_mine(CurrentUid, Page) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc 搜索公开 Bot
-spec search(cowboy_req:req(), map()) -> cowboy_req:req().
search(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"q">>, Qs, <<>>),
    {ok, Page} = elib_param:int(page, Req0, 1),
    {ok, Size} = elib_param:int(size, Req0, 20),
    case bot_logic:search(Keyword, Page, Size) of
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
