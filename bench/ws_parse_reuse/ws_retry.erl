-module(ws_retry).
-export([main/0]).
main()->
    ws_perf_core:boot(),
    [begin ws_perf_core:load(V),run(V) end||V<-[baseline,candidate]],halt().
run(Variant)->
    Uid=800001,Sockets=ws_perf_core:open([Uid],2,json,v2),
    try
        [scenario(Variant,Uid,Mode,Sockets)||Mode<-[no_ack,ack_first,ack_after_first_retry]],
        stale_reference(Variant,Uid,Sockets)
    after ws_perf_core:close(Sockets) end.
scenario(Variant,Uid,Mode,#{connections:=Connections})->
    Id=atom_to_binary(Mode,utf8),
    Bin=ws_perf_core:make_message(Uid,Id,<<"opaque.synthetic.ciphertext">>),
    Expected={binary,imboy_codec:wrap_v2_frame(16#20,0,Bin)},
    DidMap=maps:from_list([{C,integer_to_binary(D)}||{{C,_},D}<-lists:zip(Connections,[1,2])]),
    ok=message_ds:send_next(Uid,Id,Bin,[0,30,40]),
    Counts=collect(ws_perf_core:now_us()+200000,#{},DidMap,Uid,Id,Expected,Mode),
    ExpectedCount=case Mode of no_ack->3;ack_first->1;ack_after_first_retry->2 end,
    [ExpectedCount=maps:get(C,Counts,0)||{C,_}<-Connections],
    [begin
        undefined=ack_retry_cache:get({Uid,Did,Id}),
        ack_retry_cache:delete_if_value({ack_received,Uid,Did,Id},true)
     end||Did<-[<<"1">>,<<"2">>]],
    Total=lists:sum(maps:values(Counts)),
    io:format("RETRY_RESULT ~s~n",[jsone:encode(#{variant=>Variant,scenario=>Mode,
        business_messages=>1,unique_devices_received=>2,received_frames=>Total,
        retransmitted_frames=>Total-2,delays_ms=>[0,30,40],errors=>0,
        ack_event_fixture=>true,production_ack_persistence=>false})]).
collect(End,Counts,DidMap,Uid,Id,Expected,Mode)->
    Remaining=max(0,(End-ws_perf_core:now_us()) div 1000),
    receive
        {gun_ws,C,_,Frame}->
            Expected=Frame,
            Count=maps:get(C,Counts,0)+1,
            AckAt=case Mode of no_ack->0;ack_first->1;ack_after_first_retry->2 end,
            case Count=:=AckAt of
                true->ok=websocket_logic:handle_ack_cancel(Uid,maps:get(C,DidMap),Id);
                false->ok
            end,
            collect(End,Counts#{C=>Count},DidMap,Uid,Id,Expected,Mode);
        {gun_error,_,_,Reason}->error({gun_error,Reason})
    after Remaining->Counts end.
stale_reference(Variant,Uid,_Sockets)->
    Id= <<"stale_reference">>,Did= <<"1">>,Key={Uid,Did,Id},
    [{Pid,_}]=[{P,M}||{P,{_,D}=M}<-imboy_syn:list_by_uid(Uid),D=:=Did],
    Old=make_ref(),Replacement=make_ref(),
    ack_retry_cache:set(Key,Replacement,10000),
    Bin=ws_perf_core:make_message(Uid,Id,<<"opaque">>),
    Pid ! {timeout,Old,{[10],Key,Bin}},
    receive {gun_ws,_,_,_}->error(stale_timeout_delivered) after 100->ok end,
    {ok,Replacement}=ack_retry_cache:get(Key),
    true=ack_retry_cache:delete_if_value(Key,Replacement),
    io:format("RETRY_RESULT ~s~n",[jsone:encode(#{variant=>Variant,scenario=>stale_reference,
        injected_callbacks=>1,received_frames=>0,replacement_reference_preserved=>true,errors=>0})]).
