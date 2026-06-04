-module(channel_handler_message).
-behavior(cowboy_rest).
-export([init/2, handle_action/3]).
-export([
    pin_message/2,
    unpin_message/2,
    pinned_messages/2,
    send_message/2,
    retract_message/2,
    react_message/2,
    message_reactions/2,
    record_view/2,
    delete_message/2,
    revoke_message/2,
    add_reaction/2,
    remove_reaction/2,
    subscribers/2
]).
-include("error_code.hrl").

init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

handle_action(pin_message, Req, State) -> pin_message(Req, State);
handle_action(unpin_message, Req, State) -> unpin_message(Req, State);
handle_action(pinned_messages, Req, State) -> pinned_messages(Req, State);
handle_action(send_message, Req, State) -> send_message(Req, State);
handle_action(retract_message, Req, State) -> retract_message(Req, State);
handle_action(react_message, Req, State) -> react_message(Req, State);
handle_action(message_reactions, Req, State) -> message_reactions(Req, State);
handle_action(record_view, Req, State) -> record_view(Req, State);
handle_action(delete_message, Req, State) -> delete_message(Req, State);
handle_action(revoke_message, Req, State) -> revoke_message(Req, State);
handle_action(add_reaction, Req, State) -> add_reaction(Req, State);
handle_action(remove_reaction, Req, State) -> remove_reaction(Req, State);
handle_action(subscribers, Req, State) -> subscribers(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% 消息管理 API
%% ===================================================================

-spec pin_message(cowboy_req:req(), map()) -> cowboy_req:req().
pin_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(message_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        MessageId ->
            PostVals = elib_param:post(Req0),
            Pinned = maps:get(<<"pinned">>, PostVals, true),
            case channel_logic:pin_message(Uid, MessageId, Pinned) of
                {ok, Message} ->
                    elib_response:success(Req0, Message);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec unpin_message(cowboy_req:req(), map()) -> cowboy_req:req().
unpin_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(message_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        MessageId ->
            case channel_logic:pin_message(Uid, MessageId, false) of
                {ok, Message} ->
                    elib_response:success(Req0, Message);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec pinned_messages(cowboy_req:req(), map()) -> cowboy_req:req().
pinned_messages(Req0, _State) ->
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            case channel_logic:get_pinned_messages(ChannelId) of
                {ok, Messages} ->
                    elib_response:success(Req0, #{list => Messages});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec delete_message(cowboy_req:req(), map()) -> cowboy_req:req().
delete_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case cowboy_req:binding(message_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        MessageId ->
            case channel_logic:delete_message(Uid, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec revoke_message(cowboy_req:req(), map()) -> cowboy_req:req().
revoke_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId =:= <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:revoke_message(Uid, ChannelId, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec record_view(cowboy_req:req(), map()) -> cowboy_req:req().
record_view(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:record_message_view(Uid, ChannelId, MessageId) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec add_reaction(cowboy_req:req(), map()) -> cowboy_req:req().
add_reaction(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    ReactionType = maps:get(<<"reaction_type">>, PostVals, <<"like">>),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:add_reaction(Uid, ChannelId, MessageId, ReactionType) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec remove_reaction(cowboy_req:req(), map()) -> cowboy_req:req().
remove_reaction(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    ReactionType = resolve_reaction_type(Req0, PostVals),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:remove_reaction(Uid, ChannelId, MessageId, ReactionType) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec send_message(cowboy_req:req(), map()) -> cowboy_req:req().
send_message(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    Content = maps:get(<<"content">>, PostVals, <<>>),
    MsgType = maps:get(<<"msg_type">>, PostVals, <<"text">>),
    Payload = maps:get(<<"payload">>, PostVals, #{}),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when Content == <<>> ->
            elib_response:error(Req0, <<"消息内容不能为空"/utf8>>);
        _ ->
            case channel_logic:publish_message(Uid, ChannelId, Content, MsgType, Payload) of
                {ok, Message} ->
                    elib_response:success(Req0, Message);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec retract_message(cowboy_req:req(), map()) -> cowboy_req:req().
retract_message(Req0, State) ->
    revoke_message(Req0, State).

-spec react_message(cowboy_req:req(), map()) -> cowboy_req:req().
react_message(Req0, State) ->
    add_reaction(Req0, State).

-spec message_reactions(cowboy_req:req(), map()) -> cowboy_req:req().
message_reactions(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    MessageId = resolve_message_id(Req0, PostVals),
    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ when MessageId == <<>> ->
            elib_response:error(Req0, <<"消息ID不能为空"/utf8>>);
        _ ->
            case channel_logic:get_message_reactions(ChannelId, MessageId) of
                {ok, Reactions} ->
                    elib_response:success(Req0, #{list => Reactions});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% 订阅者管理 API
%% ===================================================================

-spec subscribers(cowboy_req:req(), map()) -> cowboy_req:req().
subscribers(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        ChannelId ->
            ChannelIdInt = elib_cnv:to_integer(ChannelId),
            case channel_ds:is_subscribed(ChannelIdInt, Uid) of
                false ->
                    elib_response:error(Req0, <<"只有订阅者才能查看订阅者列表"/utf8>>, 403);
                true ->
                    Qs = cowboy_req:parse_qs(Req0),
                    CursorBin = proplists:get_value(<<"cursor">>, Qs, <<>>),
                    Limit = parse_qs_int(proplists:get_value(<<"limit">>, Qs), 50, 1, 200),
                    Cursor =
                        case CursorBin of
                            <<>> -> 0;
                            _ -> parse_qs_int(CursorBin, 0, 0, 16#7fffffff)
                        end,
                    case channel_logic:get_subscribers(ChannelId, Cursor, Limit) of
                        {ok, Subscribers} ->
                            elib_response:success(Req0, #{
                                list => Subscribers, cursor => Cursor, limit => Limit
                            });
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec resolve_channel_id(cowboy_req:req(), map()) -> binary().
resolve_channel_id(Req0, PostVals) ->
    case binding_or_empty(channel_id, Req0) of
        <<>> -> maps:get(<<"channel_id">>, PostVals, <<>>);
        ChannelId -> ChannelId
    end.

-spec resolve_message_id(cowboy_req:req(), map()) -> binary().
resolve_message_id(Req0, PostVals) ->
    case binding_or_empty(message_id, Req0) of
        <<>> -> maps:get(<<"message_id">>, PostVals, <<>>);
        MessageId -> MessageId
    end.

-spec resolve_reaction_type(cowboy_req:req(), map()) -> binary().
resolve_reaction_type(Req0, PostVals) ->
    case binding_or_empty(reaction_type, Req0) of
        <<>> -> maps:get(<<"reaction_type">>, PostVals, <<"like">>);
        ReactionType -> ReactionType
    end.

-spec binding_or_empty(atom(), cowboy_req:req()) -> binary().
binding_or_empty(Key, Req0) ->
    case cowboy_req:binding(Key, Req0) of
        undefined -> <<>>;
        Val -> Val
    end.

-spec parse_qs_int(term(), integer(), integer(), integer()) -> integer().
parse_qs_int(undefined, Default, _Min, _Max) ->
    Default;
parse_qs_int(Value, Default, Min, Max) ->
    case safe_to_integer(Value) of
        {ok, Int} when Int < Min -> Min;
        {ok, Int} when Int > Max -> Max;
        {ok, Int} -> Int;
        error -> Default
    end.

-spec safe_to_integer(term()) -> {ok, integer()} | error.
safe_to_integer(Value) when is_integer(Value) ->
    {ok, Value};
safe_to_integer(Value) when is_binary(Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(Value) when is_list(Value) ->
    try
        {ok, list_to_integer(Value)}
    catch
        _:_ -> error
    end;
safe_to_integer(_) ->
    error.
