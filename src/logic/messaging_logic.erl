-module(messaging_logic).

-export([handle_rest_action/3,
         offline/2,
         offline_ack/2,
         read_stats/2,
         reaction_add/2,
         reaction_remove/2,
         reaction_list/2,
         route_ws/5]).

-include("error_code.hrl").
-include("log.hrl").

%% Stable messaging entry for REST read models and websocket routing.
-spec handle_rest_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_rest_action(offline, Req0, State) ->
    offline(Req0, State);
handle_rest_action(offline_ack, Req0, State) ->
    offline_ack(Req0, State);
handle_rest_action(read_stats, Req0, State) ->
    read_stats(Req0, State);
handle_rest_action(pin, Req0, State) ->
    msg_handler:pin(Req0, State);
handle_rest_action(forward, Req0, State) ->
    msg_handler:forward(Req0, State);
handle_rest_action(reaction_add, Req0, State) ->
    reaction_add(Req0, State);
handle_rest_action(reaction_remove, Req0, State) ->
    reaction_remove(Req0, State);
handle_rest_action(reaction_list, Req0, State) ->
    reaction_list(Req0, State);
handle_rest_action(false, Req0, _State) ->
    Req0.

-spec offline(cowboy_req:req(), map()) -> cowboy_req:req().
offline(Req0, State) ->
    {ok, Limit} = elib_param:int(limit, Req0, 1000),
    {ok, C2CLastMsgAtInt} = elib_param:int(c2c_last_msg_at, Req0, 0),
    {ok, C2GLastMsgAtInt} = elib_param:int(c2g_last_msg_at, Req0, 0),
    {ok, S2CLastMsgAtInt} = elib_param:int(s2c_last_msg_at, Req0, 0),

    C2CLastMsgAt = elib_dt:to_rfc3339(C2CLastMsgAtInt, millisecond),
    C2GLastMsgAt = elib_dt:to_rfc3339(C2GLastMsgAtInt, millisecond),
    S2CLastMsgAt = elib_dt:to_rfc3339(S2CLastMsgAtInt, millisecond),

    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            CountC2CMsg = get_c2c_msg_count(CurrentUid, C2CLastMsgAt),
            CountC2GMsg = get_c2g_msg_count(CurrentUid, C2GLastMsgAt),
            CountS2CMsg = get_s2c_msg_count(CurrentUid, S2CLastMsgAt),

            C2CMsgs = msg_c2c_ds:read_msg(CurrentUid, Limit, C2CLastMsgAt),
            C2GMsgs = msg_c2g_ds:read_msg(CurrentUid, Limit, C2GLastMsgAt),
            S2CMsgs = msg_s2c_ds:read_msg(CurrentUid, Limit, S2CLastMsgAt),

            ProcessedC2CMsgs = [process_message(Msg) || Msg <- C2CMsgs],
            ProcessedC2GMsgs = [process_message(Msg) || Msg <- C2GMsgs],
            ProcessedS2CMsgs = [process_message(Msg) || Msg <- S2CMsgs],

            Payload =
                #{<<"c2c">> =>
                      #{<<"has_more">> => length(ProcessedC2CMsgs) < CountC2CMsg,
                        <<"next_last_msg_at">> =>
                            calculate_next_last_msg_at(ProcessedC2CMsgs, C2CLastMsgAt),
                        <<"total">> => CountC2CMsg,
                        <<"list">> => ProcessedC2CMsgs},
                  <<"c2g">> =>
                      #{<<"has_more">> => length(ProcessedC2GMsgs) < CountC2GMsg,
                        <<"next_last_msg_at">> =>
                            calculate_next_last_msg_at(ProcessedC2GMsgs, C2GLastMsgAt),
                        <<"total">> => CountC2GMsg,
                        <<"list">> => ProcessedC2GMsgs},
                  <<"s2c">> =>
                      #{<<"has_more">> => length(ProcessedS2CMsgs) < CountS2CMsg,
                        <<"next_last_msg_at">> =>
                            calculate_next_last_msg_at(ProcessedS2CMsgs, S2CLastMsgAt),
                        <<"total">> => CountS2CMsg,
                        <<"list">> => ProcessedS2CMsgs}},
            elib_response:success(Req0, Payload)
    end.

-spec read_stats(cowboy_req:req(), map()) -> cowboy_req:req().
read_stats(Req0, State) ->
    MsgId = cowboy_req:qs_val(<<"msg_id">>, Req0, undefined),

    case MsgId of
        undefined ->
            elib_response:error(Req0, <<"缺少 msg_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case maps:get(current_uid, State, undefined) of
                undefined ->
                    elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
                CurrentUid ->
                    case msg_c2g_logic:read_stats(MsgId, CurrentUid) of
                        {ok, ReadCount, TotalCount} ->
                            Payload = #{
                                <<"read_count">> => ReadCount,
                                <<"total_count">> => TotalCount
                            },
                            elib_response:success(Req0, Payload);
                        {error, not_found} ->
                            elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, permission_denied} ->
                            elib_response:error(Req0, <<"无权限访问该消息"/utf8>>, ?ERR_ACCESS_DENIED);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

-spec offline_ack(cowboy_req:req(), map()) -> cowboy_req:req().
offline_ack(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Type = string:lowercase(maps:get(<<"type">>, PostVals, <<>>)),
    MsgIds = maps:get(<<"msg_ids">>, PostVals, []),

    ok =
        ?INFO_LOG("Processing offline_ack for user: ~p, type: ~p, msg_count: ~p",
                  [CurrentUid, Type, length(MsgIds)]),

    case process_offline_ack(CurrentUid, Type, MsgIds) of
        {ok, ProcessedCount} ->
            Payload =
                #{<<"msg">> => <<"offline_messages_acknowledged">>,
                  <<"type">> => Type,
                  <<"processed_count">> => ProcessedCount,
                  <<"msg_ids_count">> => length(MsgIds)},
            ok =
                ?INFO_LOG("Offline ack processed successfully: ~p messages for user: ~p",
                          [ProcessedCount, CurrentUid]),
            elib_response:success(Req0, Payload);
        {error, Reason} ->
            ok =
                ?ERROR_LOG("Failed to process offline_ack for user: ~p, reason: ~p",
                           [CurrentUid, Reason]),
            elib_response:error(Req0, Reason)
    end.

-spec route_ws(binary(), integer(), map(), binary(), binary()) -> ok | {reply, map()}.
route_ws(MsgId, CurrentUid, Data, Type, OriginalMsg) ->
    message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg).

-spec reaction_add(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_add(Req0, State) ->
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            {ok, Body, _Req1} = elib_req:body(Req0, []),
            MsgId = maps:get(<<"msg_id">>, Body, undefined),
            MsgType = maps:get(<<"msg_type">>, Body, <<"c2c">>),
            Emoji = maps:get(<<"emoji">>, Body, undefined),

            case {MsgId, Emoji} of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少emoji参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, <<>>} ->
                    elib_response:error(Req0, <<"emoji不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case msg_reaction_logic:add(MsgId, MsgType, CurrentUid, Emoji) of
                        {ok, Result} ->
                            Payload = #{
                                <<"msg_id">> => MsgId,
                                <<"emoji">> => Emoji,
                                <<"user_id">> => maps:get(<<"user_id">>, Result),
                                <<"created_at">> => maps:get(<<"created_at">>, Result)
                            },
                            elib_response:success(Req0, Payload, <<"添加表情成功"/utf8>>);
                        {error, {invalid_param, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {error, msg_not_found} ->
                            elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_MESSAGE_NOT_FOUND);
                        {error, permission_denied} ->
                            elib_response:error(Req0, <<"无权限访问该消息"/utf8>>, ?ERR_ACCESS_DENIED);
                        {error, not_group_member} ->
                            elib_response:error(Req0, <<"不是群成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

-spec reaction_remove(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_remove(Req0, State) ->
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            {ok, Body, _Req1} = elib_req:body(Req0, []),
            MsgId = maps:get(<<"msg_id">>, Body, undefined),
            MsgType = maps:get(<<"msg_type">>, Body, <<"c2c">>),
            Emoji = maps:get(<<"emoji">>, Body, undefined),

            case {MsgId, Emoji} of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少emoji参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, <<>>} ->
                    elib_response:error(Req0, <<"emoji不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case msg_reaction_logic:remove(MsgId, MsgType, CurrentUid, Emoji) of
                        ok ->
                            Payload = #{
                                <<"msg_id">> => MsgId,
                                <<"emoji">> => Emoji
                            },
                            elib_response:success(Req0, Payload, <<"移除表情成功"/utf8>>);
                        {error, msg_not_found} ->
                            elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_MESSAGE_NOT_FOUND);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

-spec reaction_list(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_list(Req0, _State) ->
    MsgId = cowboy_req:qs_val(<<"msg_id">>, Req0, undefined),
    MsgType = cowboy_req:qs_val(<<"msg_type">>, Req0, <<"c2c">>),

    case MsgId of
        undefined ->
            elib_response:error(Req0, <<"缺少 msg_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case msg_reaction_logic:list(MsgId, MsgType) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, Reason} ->
                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

-spec calculate_next_last_msg_at([map()], binary() | integer()) -> binary() | integer().
calculate_next_last_msg_at([], LastMsgAt) ->
    LastMsgAt;
calculate_next_last_msg_at(Msgs, _LastMsgAt) when length(Msgs) > 0 ->
    LastMsg = lists:last(Msgs),
    get_created_at(LastMsg).

-spec get_created_at(map()) -> binary() | integer().
get_created_at(Msg) when is_map(Msg) ->
    maps:get(<<"created_at">>, Msg, 0).

-spec get_c2c_msg_count(binary() | integer(), binary()) -> integer().
get_c2c_msg_count(Uid, LastMsgAt) ->
    Tb = msg_c2c_repo:tablename(),
    Sql = <<"SELECT count(*) as count FROM ",
            Tb/binary,
            " WHERE to_id = $1 AND created_at >= $2">>,
    case elib_pg:query(Sql, [Uid, LastMsgAt]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

-spec get_c2g_msg_count(integer(), binary()) -> integer().
get_c2g_msg_count(Uid, LastMsgAt) ->
    Tb = msg_c2g_timeline_repo:tablename(),
    Sql = <<"SELECT count(*) as count FROM ",
            Tb/binary,
            " WHERE to_id = $1 AND client_ack = 0 AND created_at >= $2">>,
    case elib_pg:query(Sql, [Uid, LastMsgAt]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

-spec get_s2c_msg_count(integer(), binary()) -> integer().
get_s2c_msg_count(Uid, LastMsgAt) ->
    Tb = msg_s2c_repo:tablename(),
    Sql = <<"SELECT count(*) as count FROM ",
            Tb/binary,
            " WHERE to_id = $1 AND created_at >= $2">>,
    case elib_pg:query(Sql, [Uid, LastMsgAt]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

-spec process_message(map()) -> map().
process_message(Msg) when is_map(Msg) ->
    FromId = maps:get(<<"from_id">>, Msg, undefined),
    ToId = maps:get(<<"to_id">>, Msg, undefined),

    Msg2 = maps:remove(<<"from_id">>, Msg),
    Msg3 = maps:remove(<<"to_id">>, Msg2),

    Msg4 =
        case FromId of
            undefined ->
                Msg3;
            _ ->
                Msg3#{<<"from">> => elib_hashids:encode(FromId)}
        end,

    case ToId of
        undefined ->
            Msg4;
        ToList when is_list(ToList) ->
            ToEncoded = [elib_hashids:encode(ToUid) || ToUid <- ToList],
            Msg4#{<<"to">> => ToEncoded};
        _ ->
            Msg4#{<<"to">> => elib_hashids:encode(ToId)}
    end.

-spec process_offline_ack(integer(), binary(), list()) -> {ok, integer()} | {error, binary()}.
process_offline_ack(Uid, Type, MsgIds) ->
    case Type of
        <<"c2c">> ->
            Count = msg_c2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        <<"c2g">> ->
            Count = msg_c2g_timeline_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        <<"s2c">> ->
            Count = msg_s2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        _ ->
            {error, <<"unsupported_message_type">>}
    end.
