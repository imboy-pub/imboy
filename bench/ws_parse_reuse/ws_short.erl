-module(ws_short).
-export([main/0]).
main()->
    ws_perf_core:boot(),
    Cases=[{json,none,512,1},{json,v2,512,1},{protobuf,none,512,1},
           {json,none,8192,2},{json,v2,8192,2},{protobuf,none,8192,2},
           {json,v2,65536,2},{json,v2,8192,8}],
    [begin ws_perf_core:load(V),run(C,64,warmup,V,0) end||C<-Cases,V<-[baseline,candidate]],
    [begin
        Order=case R rem 2 of 0->[candidate,baseline];_->[baseline,candidate] end,
        [begin ws_perf_core:load(V),N=case element(3,C) of 65536->300;_->1500 end,
             run(C,N,measure,V,R) end||C<-Cases,V<-Order]
     end||R<-lists:seq(1,5)],halt().
run({Protocol,Framing,Size,Devices},N,Kind,Variant,Round)->
    U=600000+Round,Sockets=ws_perf_core:open([U],Devices,Protocol,Framing),
    Pids=maps:get(pids,Sockets),
    rand:seed(exsss,{2026,10,20}),
    Payload=base64:encode(list_to_binary([rand:uniform(256)-1||_<-lists:seq(1,Size)])),
    Bin=ws_perf_core:make_message(U,<<"1000000">>,Payload),
    [erlang:garbage_collect(P)||P<-Pids],erlang:garbage_collect(),
    S0=ws_perf_core:sample(Pids),
    {Lat,Peak}=batches(N,U,Payload,Sockets,[],S0),S1=ws_perf_core:sample(Pids),
    D=ws_perf_core:delta(S0,S1),Wall=maps:get(at_us,D),
    io:format("SHORT_RESULT ~s~n",[jsone:encode(#{variant=>Variant,round=>Round,kind=>Kind,
        protocol=>Protocol,framing=>Framing,raw_payload_bytes=>Size,wire_json_bytes=>byte_size(Bin),
        business_messages=>N,devices=>Devices,successful_device_deliveries=>N*Devices,errors=>0,
        device_per_s=>N*Devices*1000000 div Wall,business_per_s=>N*1000000 div Wall,
        p95_delivery_us=>ws_perf_core:quantile(Lat,95),p99_delivery_us=>ws_perf_core:quantile(Lat,99),
        delta=>D,first_sample=>S0,final_sample=>S1,peak_sampled=>Peak})]),
    ws_perf_core:close(Sockets).
batches(0,_,_,_,Lat,Peak)->{Lat,Peak};
batches(N,U,Payload,Sockets,Lat,Peak)->
    K=min(32,N),
    Messages=[begin Id=integer_to_binary(1000000+N-I),
        {Id,ws_perf_core:make_message(U,Id,Payload)} end||I<-lists:seq(1,K)],
    Times=ws_perf_core:batch(U,Messages,Sockets,[0]),
    S=ws_perf_core:sample(maps:get(pids,Sockets)),
    P=maps:merge(Peak,maps:from_list([{Key,max(maps:get(Key,Peak),maps:get(Key,S))}
        ||Key<-[binary_bytes,rss_kib,rss_highwater_kib,max_ws_queue_sampled]])),
    batches(N-K,U,Payload,Sockets,Times++Lat,P).
