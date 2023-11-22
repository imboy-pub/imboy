-module(conversation_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            online ->
                online(Req0, State);
            mine ->
                mine(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


online(Req0, _State) ->
    CountUser = imboy_syn:count_user(),
    Count = imboy_syn:count(),
    Msg = io_lib:format("在线总人数: ~p, 在线设备数~p", [CountUser, Count]),
    Res = cowboy_req:match_qs([{type, [], undefined}], Req0),
    % ?LOG(Res),
    List2 =
        case maps:get(type, Res) of
            <<"list">> ->
                #{limit := Limit} = cowboy_req:match_qs([{limit, [], "10"}], Req0),
                % ?LOG([limit, Limit]),
                {Limit2, _} = string:to_integer(Limit),

                % imboy_syn:list_by_limit(Limit);
                List1 = imboy_syn:list_by_limit(Limit2),
                Column = [<<"uid">>, <<"pid">>, <<"dtype">>, <<"did">>, <<"time">>, <<"ref">>, <<"node">>],
                [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                Column,
                                [Uid, Pid, DType, DID, list_to_binary(imboy_dt:to_rfc3339(Nano, nanosecond)), Ref, Node])
                  || {{Uid, Pid}, {DType, DID}, Nano, Ref, Node} <- List1 ];
            _ ->
                []
        end,
    imboy_response:success(Req0, List2, Msg).


mine(Req0, State) ->
    #{last_server_ts := ServerTS} = cowboy_req:match_qs([{last_server_ts, [], undefined}], Req0),
    % ?LOG(ServerTS),
    CurrentUid = maps:get(current_uid, State),
    List = msg_c2c_ds:read_msg(CurrentUid, 1000, ServerTS),
    % ?LOG(["mine_list", List]),
    List2 = mine_transfer(List),
    imboy_response:success(Req0, List2).


mine_transfer(List) ->
    [ [{<<"id">>, proplists:get_value(<<"id">>, Msg)} | jsone:decode(proplists:get_value(<<"payload">>, Msg), [{object_format, proplist}])] || Msg <- List ].
