-module(websocket_logic).
%%%
% websocket 业务逻辑模块
%%%

-include_lib("imboy/include/log.hrl").

% -export ([subprotocol/1]).
-export([c2c/3]).
-export([c2c_client_ack/3]).
-export([c2c_revoke/3]).
-export([c2g/3]).
-export([s2c/3]).
-export([s2c_client_ack/3]).

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% 单聊消息
-spec c2c(binary(), integer(), Data :: list()) ->
          ok | {reply, Msg :: list()}.
c2c(Id, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = imboy_hashids:uid_decode(To),
    % CurrentUid = imboy_hashids:uid_decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    case friend_ds:is_friend(CurrentUid, ToId) of
        true ->
            NowTs = imboy_dt:millisecond(),
            From = imboy_hashids:uid_encode(CurrentUid),
            Payload = proplists:get_value(<<"payload">>, Data),
            CreatedAt = proplists:get_value(<<"created_at">>, Data),
            % 存储消息
            msg_c2c_ds:write_msg(CreatedAt, Id, Payload,
                CurrentUid, ToId, NowTs),
            self() ! {reply, [{<<"id">>, Id},
                     {<<"type">>, <<"C2C_SERVER_ACK">>},
                     {<<"server_ts">>, NowTs}]},
            Msg = [{<<"id">>, Id},
               {<<"type">>, <<"C2C">>},
               {<<"from">>, From},
               {<<"to">>, To},
               {<<"payload">>, Payload},
               {<<"created_at">>, CreatedAt},
               {<<"server_ts">>, NowTs}
            ],
            MsgJson = jsone:encode(Msg, [native_utf8]),
            MsLi = [0, 1500, 1500, 3000, 1000, 3000, 5000],
            message_ds:send_next(ToId, Id, MsgJson, MsLi),
            ok;
        false ->
            Msg = message_ds:assemble_s2c(<<"isnotfriend">>, Id),
            {reply, Msg}
    end.


%% 客户端确认C2C投递消息
-spec c2c_client_ack(MsgId :: binary(),
                     CurrentUid :: integer(),
                     Data :: binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"`id`">>,
    Where = <<"WHERE `msg_id` = ? AND `to_id` = ?">>,
    Vals = [MsgId, CurrentUid],
    {ok, _ColumnList, Rows} = msg_c2c_repo:read_msg(Where,
                                                    Vals,
                                                    Column,
                                                    1),
    [msg_c2c_repo:delete_msg(Id) || [Id] <- Rows],
    ok.


%% 客户端撤回消息
-spec c2c_revoke(binary(), Data :: list(), binary()) ->
          ok | {reply, Msg :: list()}.
c2c_revoke(Id, Data, Type) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToId = imboy_hashids:uid_decode(To),
    ?LOG([From, To, ToId, Type, Data]),
    NowTs = imboy_dt:millisecond(),

    Msg = [
        {<<"id">>, Id},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"server_ts">>, NowTs}
    ],
    % 判断是否在线
    case user_logic:is_offline(ToId) of
        {ToPid, _UidBin, _ClientSystemBin} ->
            erlang:start_timer(0, ToPid,
               jsone:encode([{<<"type">>, Type} | Msg], [native_utf8])
            ),
            ok;
        true ->  % 对端离线处理
            FromId = imboy_hashids:uid_decode(From),
            msg_c2c_ds:revoke_offline_msg(NowTs, Id, FromId, ToId),
            {reply, [{<<"type">>, <<"C2C_REVOKE_ACK">>} | Msg]}
    end.


%% 群聊发送消息
-spec c2g(binary(), integer(), Data :: list()) ->
          ok | {reply, Msg :: list()}.
c2g(Id, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = imboy_hashids:uid_decode(Gid),
    % TODO check is group member
    Column = <<"`user_id`">>,
    {ok, _ColumnLi, Members} = group_member_repo:find_by_group_id(ToGID,
                                                                  Column),
    Uids = [Uid || [Uid] <- Members, Uid /= CurrentUid],
    % Uids.
    NowTs = imboy_dt:millisecond(),
    Msg = [{<<"id">>, Id},
           {<<"type">>, <<"C2G">>},
           {<<"from">>, imboy_hashids:uid_encode(CurrentUid)},
           {<<"to">>, Gid},
           {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
           {<<"created_at">>,
            proplists:get_value(<<"created_at">>, Data)},
           {<<"server_ts">>, NowTs}],
    % ?LOG(Msg),
    Msg2 = jsone:encode(Msg, [native_utf8]),

    % TODO use pg module
    % _UidsOnline = lists:filtermap(fun(Uid) ->
    %                                      % message_ds:send(Uid, Msg2, 1)
    %                               end,
    %                               Uids),
    % 存储消息
    msg_c2g_ds:write_msg(NowTs, Id, Msg2, CurrentUid, Uids, ToGID),
    ok.


%% 系统消息
-spec s2c(binary(), integer(), Data :: list()) ->
          ok | {reply, Msg :: list()}.
s2c(_Id, _CurrentUid, _Data) ->
    ok.

%% 客户端确认S2C投递消息
-spec s2c_client_ack(MsgId :: binary(),
                     CurrentUid :: integer(),
                     Data :: binary()) -> ok.
s2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"`id`">>,
    Where = <<"WHERE `msg_id` = ? AND `to_id` = ?">>,
    Vals = [MsgId, CurrentUid],
    {ok, _ColumnList, Rows} = msg_s2c_repo:read_msg(Where,
                                                    Vals,
                                                    Column,
                                                    1),
    [msg_s2c_repo:delete_msg(Id) || [Id] <- Rows],
    ok.


