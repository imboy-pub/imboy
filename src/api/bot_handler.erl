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
handle_action(send_message, Req, State) -> send_message(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% Action Handlers
%% ===================================================================

%% @doc 注册 Bot
%% owner_uid 一律取自 JWT 身份（current_uid），请求体中的 owner_uid 被忽略，
%% 防止冒充他人注册。
-spec register(cowboy_req:req(), map()) -> cowboy_req:req().
register(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            Data = #{
                name => maps:get(<<"name">>, Body, <<>>),
                username => maps:get(<<"username">>, Body, <<>>),
                owner_uid => CurrentUid,
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
            end
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

%% @doc 更新 Bot 信息（仅属主）
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
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
                    case bot_logic:update(BotId, Data, CurrentUid) of
                        {ok, Result} ->
                            elib_response:success(Req1, Result);
                        {error, Reason} ->
                            elib_response:error(Req1, Reason)
                    end;
                false ->
                    elib_response:error(Req1, <<"Bot ID 不能为空"/utf8>>)
            end
    end.

%% @doc 停用 Bot（仅属主）
-spec disable(cowboy_req:req(), map()) -> cowboy_req:req().
disable(Req0, State) ->
    change_status(Req0, State, 0).

%% @doc 启用 Bot（仅属主）
-spec enable(cowboy_req:req(), map()) -> cowboy_req:req().
enable(Req0, State) ->
    change_status(Req0, State, 1).

%% @doc 列出我的 Bot
-spec list_mine(cowboy_req:req(), map()) -> cowboy_req:req().
list_mine(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
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
    {ok, Page0} = elib_param:int(page, Req0, 1),
    {ok, Size0} = elib_param:int(size, Req0, 20),
    Page = positive_integer(Page0),
    Size = positive_integer(Size0),
    case bot_logic:search(Keyword, Page, Size) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

%% @doc Bot 发送消息（Bot 服务器调用，api_token 认证，无用户 JWT）
%%
%% 认证：Authorization: Bearer <api_token>（bot.api_token 查表比对）
%% 防护：① agent_rate_limiter 以 bot_id 为 scope 限流（open 端点必须设闸）；
%%       ② 仅允许回复已有往来消息的用户（has_exchange，Telegram started-chat
%%          范式），阻止 Bot 主动骚扰任意用户。
%% 该路由在 imboy_router:open() 白名单内（外部服务器无 JWT），凭证校验收敛于此。
-spec send_message(cowboy_req:req(), map()) -> cowboy_req:req().
send_message(Req0, _State) ->
    case authenticate(Req0) of
        {ok, #{<<"user_id">> := BotId} = Bot} ->
            %% scope=BotId（计费身份），requester=BotId（Bot 是唯一请求者）：
            %% 与 ai_agent_reply 的 allow(ToId, FromUid) 同一闸门
            case agent_rate_limiter:allow(BotId, BotId) of
                {deny, _Reason} ->
                    elib_response:error(Req0, <<"请求过于频繁，请稍后再试"/utf8>>);
                allow ->
                    do_send_message(Req0, BotId, Bot)
            end;
        {error, Reason} ->
            elib_response:error(Req0, Reason)
    end.

-spec do_send_message(cowboy_req:req(), integer(), map()) -> cowboy_req:req().
do_send_message(Req0, BotId, _Bot) ->
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            ToUid = maps:get(<<"to_uid">>, Body, 0),
            case elib_cnv:safe_to_integer(ToUid) of
                ToId when ToId > 0 ->
                    case bot_logic:has_exchange(BotId, ToId) of
                        true ->
                            MsgData = #{
                                <<"msg_type">> => maps:get(<<"msg_type">>, Body, <<"text">>),
                                <<"payload">> => maps:get(<<"payload">>, Body, #{})
                            },
                            case bot_logic:send_message(BotId, ToId, MsgData) of
                                {ok, Result} ->
                                    elib_response:success(Req1, Result);
                                {error, Reason} ->
                                    elib_response:error(Req1, Reason)
                            end;
                        false ->
                            elib_response:error(
                                Req1, <<"用户未与 Bot 建立会话，不可主动发送"/utf8>>
                            )
                    end;
                _ ->
                    elib_response:error(Req1, <<"to_uid 不能为空"/utf8>>)
            end;
        {error, _} = Err ->
            elib_response:error(Req0, Err)
    end.

%% @doc 从 Authorization: Bearer <api_token> 认证 Bot
%% 仅信任 bot.api_token（48 位强随机 hex），查表命中且状态正常即通过。
-spec authenticate(cowboy_req:req()) -> {ok, map()} | {error, binary()}.
authenticate(Req0) ->
    Authorization = cowboy_req:header(<<"authorization">>, Req0, <<>>),
    case Authorization of
        <<"Bearer ", Token/binary>> when byte_size(Token) > 0 ->
            case bot_ds:find_by_token(Token) of
                {ok, #{<<"status">> := 1} = Bot} ->
                    {ok, Bot};
                {ok, _} ->
                    {error, <<"Bot 已停用"/utf8>>};
                {error, _} ->
                    {error, <<"无效的 Bot 凭证"/utf8>>}
            end;
        _ ->
            {error, <<"缺少 Bearer api_token"/utf8>>}
    end.

%% @doc 启停 Bot 的公共路径（仅属主）
-spec change_status(cowboy_req:req(), map(), -1 | 0 | 1) -> cowboy_req:req().
change_status(Req0, State, Status) ->
    CurrentUid = maps:get(current_uid, State),
    case elib_req:body(Req0, []) of
        {ok, Body, Req1} ->
            BotId = maps:get(<<"bot_id">>, Body, 0),
            case BotId > 0 of
                true ->
                    case bot_logic:set_status(BotId, Status, CurrentUid) of
                        {ok, Result} ->
                            elib_response:success(Req1, Result);
                        {error, Reason} ->
                            elib_response:error(Req1, Reason)
                    end;
                false ->
                    elib_response:error(Req1, <<"Bot ID 不能为空"/utf8>>)
            end
    end.

%% @doc 安全地从查询字符串中获取整数值
-spec safe_int_qs(binary(), list()) -> integer() | undefined.
safe_int_qs(Key, Qs) ->
    case proplists:get_value(Key, Qs) of
        undefined -> undefined;
        Val -> elib_cnv:safe_to_integer(Val)
    end.

-spec positive_integer(integer()) -> pos_integer().
positive_integer(Value) when Value > 0 -> Value;
positive_integer(_) -> 1.
