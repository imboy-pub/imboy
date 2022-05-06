-module(logic_websocket).
%%%
% websocket 业务逻辑模块
%%%
% -export ([subprotocol/1]).
-export([c2c/3]).
-export([c2c_client_ack/3]).
-export([c2c_revoke/3]).
-export([c2g/3]).
-export([system/3]).

-include("common.hrl").


%% 单聊消息
-spec c2c(binary(), integer(), Data :: list()) ->
          ok | {reply, Msg :: list()}.
c2c(Id, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = hashids_translator:uid_decode(To),
    % CurrentUid = hashids_translator:uid_decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    case ds_friend:is_friend(CurrentUid, ToId) of
        true ->
            NowTs = util_dt:milliseconds(),
            From = hashids_translator:uid_encode(CurrentUid),
            Payload = proplists:get_value(<<"payload">>, Data),
            CreatedAt = proplists:get_value(<<"created_at">>, Data),
            % 存储消息
            case is_binary(Payload) of
                true ->
                    ds_msg_c2c:write_msg(CreatedAt,
                                         Id,
                                         Payload,
                                         CurrentUid,
                                         ToId,
                                         NowTs);
                _ ->
                    ds_msg_c2c:write_msg(CreatedAt,
                                         Id,
                                         jsone:encode(Payload, [native_utf8]),
                                         CurrentUid,
                                         ToId,
                                         NowTs)
            end,
            Msg = [{<<"id">>, Id},
               {<<"type">>, <<"C2C">>},
               {<<"from">>, From},
               {<<"to">>, To},
               {<<"payload">>, Payload},
               {<<"created_at">>, CreatedAt},
               {<<"server_ts">>, NowTs}
            ],
            _TimerRefList = ds_message:send(ToId,
                                            jsone:encode(Msg, [native_utf8]),
                                            1),
            {reply, [{<<"id">>, Id},
                     {<<"type">>, <<"C2C_SERVER_ACK">>},
                     {<<"server_ts">>, NowTs}]};
        false ->
            Msg = [{<<"type">>, <<"error">>},
                   {<<"code">>, 1},
                   {<<"msg">>, <<"Is not a friend">>},
                   {<<"timestamp">>, util_dt:milliseconds()}],
            {reply, Msg}
    end.


%% 客户端确认投递消息
-spec c2c_client_ack(MsgId :: binary(),
                     CurrentUid :: integer(),
                     Data :: binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"`id`">>,
    Where = <<"WHERE `msg_id` = ? AND `to_id` = ?">>,
    Vals = [MsgId, CurrentUid],
    {ok, _ColumnList, Rows} = repo_msg_c2c:read_msg(Where,
                                                    Vals,
                                                    Column,
                                                    1),
    [repo_msg_c2c:delete_msg(Id) || [Id] <- Rows],
    ok.


%% 客户端撤回消息
-spec c2c_revoke(binary(), Data :: list(), binary()) ->
          ok | {reply, Msg :: list()}.
c2c_revoke(Id, Data, Type) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToId = hashids_translator:uid_decode(To),
    ?LOG([From, To, ToId, Type, Data]),
    NowTs = util_dt:milliseconds(),

    Msg = [
        {<<"id">>, Id},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"server_ts">>, NowTs}
    ],
    % 判断是否在线
    case ds_user:is_offline(ToId) of
        {ToPid, _UidBin, _ClientSystemBin} ->
            erlang:start_timer(1, ToPid,
               jsone:encode([{<<"type">>, Type} | Msg], [native_utf8])
            ),
            ok;
        true ->  % 对端离线处理
            FromId = hashids_translator:uid_decode(From),
            ds_msg_c2c:revoke_offline_msg(NowTs, Id, FromId, ToId),
            {reply, [{<<"type">>, <<"C2C_REVOKE_ACK">>} | Msg]}
    end.


%% 群聊发送消息
c2g(Id, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = hashids_translator:uid_decode(Gid),
    % TODO check is group member
    Column = <<"`user_id`">>,
    {ok, _ColumnLi, Members} = repo_group_member:find_by_group_id(ToGID,
                                                                  Column),
    Uids = [Uid || [Uid] <- Members, Uid /= CurrentUid],
    % Uids.
    NowTs = util_dt:milliseconds(),
    Msg = [{<<"id">>, Id},
           {<<"type">>, <<"C2G">>},
           {<<"from">>, hashids_translator:uid_encode(CurrentUid)},
           {<<"to">>, Gid},
           {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
           {<<"created_at">>,
            proplists:get_value(<<"created_at">>, Data)},
           {<<"server_ts">>, NowTs}],
    % ?LOG(Msg),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    _UidsOnline = lists:filtermap(fun(Uid) ->
                                         ds_message:send(Uid, Msg2, 1)
                                  end,
                                  Uids),
    % 存储消息
    ds_msg_c2g:write_msg(NowTs, Id, Msg2, CurrentUid, Uids, ToGID),
    ok.


%% 系统消息
-spec c2g(binary(), integer(), Data :: list()) ->
          ok | {reply, Msg :: list()}.
system(_Id, _CurrentUid, _Data) ->
    ok.
