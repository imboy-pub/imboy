-module(bot_logic).

%%%
% bot_logic 是 Bot 业务逻辑模块
% 处理 Bot 注册、管理、搜索、消息发送等业务逻辑
%%%

-export([register/1]).
-export([get/1]).
-export([update/3]).
-export([set_status/3]).
-export([admin_set_status/2]).
-export([list_mine/2]).
-export([search/3]).
-export([send_message/3]).
-export([send_message/4]).

-include("log.hrl").
-include("imboy_const.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 注册 Bot
%% Data 键：name(必填), username(必填), owner_uid(必填), description, avatar,
%%          webhook_url, commands, permissions, events, is_public
%% 自动生成 api_token 和 verify_token
-spec register(map()) -> {ok, map()} | {error, binary()}.
register(#{name := _Name, username := _Username, owner_uid := OwnerUid} = Data) ->
    %% 验证所有者用户存在
    case user_repo:find_by_id(OwnerUid, <<"id, status">>) of
        #{<<"id">> := _} when OwnerUid > 0 ->
            ApiToken = gen_token(),
            VerifyToken = gen_token(),
            FullData = Data#{api_token => ApiToken, verify_token => VerifyToken},
            case bot_ds:create(FullData) of
                {ok, #{<<"user_id">> := BotUid}} ->
                    {ok, #{
                        <<"user_id">> => BotUid,
                        <<"api_token">> => ApiToken,
                        <<"verify_token">> => VerifyToken
                    }};
                {error, _} = Err ->
                    Err
            end;
        _ ->
            {error, <<"用户不存在或已停用"/utf8>>}
    end.

%% @doc 获取 Bot 信息
-spec get(integer()) -> {ok, map()} | {error, binary()}.
get(BotId) ->
    case bot_repo:find(BotId) of
        {ok, Bot} ->
            %% 过滤敏感字段，不返回 api_token/verify_token
            Safe = maps:with(
                [
                    <<"user_id">>,
                    <<"name">>,
                    <<"username">>,
                    <<"description">>,
                    <<"avatar">>,
                    <<"owner_uid">>,
                    <<"webhook_url">>,
                    <<"commands">>,
                    <<"permissions">>,
                    <<"events">>,
                    <<"is_public">>,
                    <<"status">>,
                    <<"created_at">>,
                    <<"updated_at">>
                ],
                Bot
            ),
            {ok, Safe};
        {error, notfound} ->
            {error, <<"Bot 不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 更新 Bot 信息（仅属主可操作）
-spec update(integer(), map(), integer()) -> {ok, map()} | {error, binary()}.
update(BotId, Data, ActorUid) when is_integer(BotId), is_map(Data), is_integer(ActorUid) ->
    case bot_repo:find(BotId) of
        {ok, Bot} ->
            case ensure_owner(Bot, ActorUid) of
                ok ->
                    case bot_repo:update(BotId, Data) of
                        {ok, _} -> {ok, #{<<"user_id">> => BotId}};
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, notfound} ->
            {error, <<"Bot 不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 启用/停用 Bot（仅属主可操作；status 1=active, 0=disabled, -1=deleted）
-spec set_status(integer(), -1 | 0 | 1, integer()) -> {ok, map()} | {error, binary()}.
set_status(BotId, Status, ActorUid) ->
    case bot_repo:find(BotId) of
        {ok, Bot} ->
            case ensure_owner(Bot, ActorUid) of
                ok ->
                    do_set_status(BotId, Status);
                {error, _} = Err ->
                    Err
            end;
        {error, notfound} ->
            {error, <<"Bot 不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 管理端启停（平台处置权，无属主校验；供 adm_bot_handler）
-spec admin_set_status(integer(), -1 | 0 | 1) -> {ok, map()} | {error, binary()}.
admin_set_status(BotId, Status) ->
    case bot_repo:find(BotId) of
        {ok, _Bot} ->
            do_set_status(BotId, Status);
        {error, notfound} ->
            {error, <<"Bot 不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec do_set_status(integer(), -1 | 0 | 1) -> {ok, map()} | {error, binary()}.
do_set_status(BotId, Status) ->
    case bot_repo:set_status(BotId, Status) of
        {ok, _} ->
            {ok, #{<<"user_id">> => BotId, <<"status">> => Status}};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 列出我的 Bot
-spec list_mine(integer(), pos_integer()) -> {ok, map()} | {error, binary()}.
list_mine(OwnerUid, Page) ->
    Size = 20,
    case bot_repo:page_by_owner(Page, Size, OwnerUid) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 搜索公开 Bot
-spec search(binary(), pos_integer(), pos_integer()) -> {ok, map()} | {error, binary()}.
search(Keyword, Page, Size) ->
    case bot_repo:search(Keyword, Page, Size) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc Bot 发送消息
%% Bot 以自身身份向指定用户发送 C2C 消息。
%% MsgData 键：msg_type(必填), payload(必填), created_at, e2ee
%% 先校验 Bot 存在且启用，再生成 MsgId（fail fast，避免无效请求消耗 TSID）。
-spec send_message(integer(), integer(), map()) -> {ok, map()} | {error, binary()}.
send_message(BotId, ToUid, MsgData) ->
    case bot_repo:find(BotId) of
        {ok, #{<<"status">> := 1}} ->
            MsgId = integer_to_binary(elib_tsid:generate()),
            send_message(MsgId, BotId, ToUid, MsgData);
        {ok, _} ->
            {error, <<"Bot 已停用"/utf8>>};
        {error, notfound} ->
            {error, <<"Bot 不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc Bot 发送消息（指定消息 ID）
-spec send_message(binary(), integer(), integer(), map()) -> {ok, map()} | {error, binary()}.
send_message(MsgId, BotId, ToUid, MsgData) ->
    case bot_repo:find(BotId) of
        {ok, #{<<"status">> := 1}} ->
            To = integer_to_binary(ToUid),
            Data = #{
                <<"to">> => To,
                <<"payload">> => maps:get(<<"payload">>, MsgData, #{}),
                <<"created_at">> => maps:get(<<"created_at">>, MsgData, elib_dt:now()),
                <<"msg_type">> => maps:get(<<"msg_type">>, MsgData, <<"text">>),
                <<"e2ee">> => maps:get(<<"e2ee">>, MsgData, null)
            },
            case msg_c2c_logic:c2c(MsgId, BotId, Data) of
                ok ->
                    {ok, #{<<"msg_id">> => MsgId}};
                {reply, _S2c} ->
                    {ok, #{<<"msg_id">> => MsgId}};
                Other ->
                    ?ERROR_LOG("bot_logic:send_message bot_id=~p error: ~p~n", [BotId, Other]),
                    {error, <<"消息发送失败"/utf8>>}
            end;
        {ok, _} ->
            {error, <<"Bot 已停用"/utf8>>};
        {error, notfound} ->
            {error, <<"Bot 不存在"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 校验操作者是 Bot 属主
-spec ensure_owner(map(), integer()) -> ok | {error, binary()}.
ensure_owner(#{<<"owner_uid">> := OwnerUid}, ActorUid) when ActorUid =:= OwnerUid ->
    ok;
ensure_owner(_, _) ->
    {error, <<"无权操作此 Bot"/utf8>>}.

%% @doc 生成随机 token（24 字节强随机 → 48 位小写 hex）
-spec gen_token() -> binary().
gen_token() ->
    string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(24))).
