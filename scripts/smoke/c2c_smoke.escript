#!/usr/bin/env escript
%%
%% C2C smoke: programmatically send one text message from Alice to Bob
%% via RPC into the running imboy@127.0.0.1 node.
%%
%% Prints: MSG_ID=<id>  (to stdout, last line)
%% Exit 0 on success, 1 on failure.

-define(NODE, 'imboy@127.0.0.1').

main(Args) ->
    From = arg_int(Args, 1, 1000000051),
    To   = arg_bin(Args, 2, <<"1000000056">>),
    {ok, _} = net_kernel:start(['probe_c2c@127.0.0.1', longnames]),
    erlang:set_cookie(node(), imboy),
    pong = case net_adm:ping(?NODE) of
               pong -> pong;
               pang ->
                   io:format(standard_error, "ERROR: cannot reach ~p (is imboy node up?)~n", [?NODE]),
                   halt(1)
           end,
    Now = erlang:system_time(millisecond),
    MsgId = iolist_to_binary([<<"smoke_c2c_">>, integer_to_binary(Now)]),
    Text  = iolist_to_binary([<<"[smoke] C2C ">>, integer_to_binary(From),
                              <<" -> ">>, To, <<" @ ">>, integer_to_binary(Now)]),
    Payload = #{<<"msg_type">> => <<"text">>,
                <<"text">> => Text,
                <<"body">> => Text},
    PayloadJson = rpc:call(?NODE, jsone, encode, [Payload, [native_utf8]]),
    case rpc:call(?NODE, msg_c2c_logic, c2c, [MsgId, From, To, PayloadJson]) of
        ok ->
            io:format("MSG_ID=~s~n", [MsgId]),
            halt(0);
        Other ->
            io:format(standard_error, "ERROR: c2c returned ~p~n", [Other]),
            halt(1)
    end.

arg_int(Args, Idx, Default) ->
    case length(Args) >= Idx of
        true  -> list_to_integer(lists:nth(Idx, Args));
        false -> Default
    end.

arg_bin(Args, Idx, Default) ->
    case length(Args) >= Idx of
        true  -> list_to_binary(lists:nth(Idx, Args));
        false -> Default
    end.
