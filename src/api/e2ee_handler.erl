-module(e2ee_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            user_keys ->
                user_keys(Req0, State);
            group_member_keys ->
                group_member_keys(Req0, State);
            _ ->
                elib_response:error(Req0, <<"not_found">>, 404)
        end,
    {ok, Req1, State}.

-spec user_keys(cowboy_req:req(), map()) -> cowboy_req:req().
user_keys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    TargetUidEnc = elib_param:get(<<"uid">>, Req0, <<"">>),
    TargetUid = elib_hashids:decode(TargetUidEnc),
    case is_integer(TargetUid) andalso TargetUid > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case e2ee_logic:user_keys(CurrentUid, TargetUid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

-spec group_member_keys(cowboy_req:req(), map()) -> cowboy_req:req().
group_member_keys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    GidEnc = elib_param:get(<<"gid">>, Req0, <<"">>),
    Gid = elib_hashids:decode(GidEnc),
    case is_integer(Gid) andalso Gid > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case e2ee_logic:group_member_keys(CurrentUid, Gid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.
