-module(ws_stress).
-export([main/0]).
main()->
    cancel_check(),
    ws_perf_core:boot(),
    Duration=env_integer("WS_STRESS_SECONDS",600),
    Rate=env_integer("WS_STRESS_BUSINESS_RATE",500),
    Variant=list_to_existing_atom(os:getenv("WS_STRESS_VARIANT","baseline")),
    true=lists:member(Variant,[baseline,candidate]),
    true=Duration>=600,true=Rate>0,true=Rate=<1000,
    ws_perf_core:load(Variant),
    Users=lists:seq(700001,700004), Devices=8,
    Sockets=ws_perf_core:open(Users,Devices,json,v2),Pids=maps:get(pids,Sockets),
    %% Deterministic pseudorandom ciphertext-like bytes; no secrets or real users.
    rand:seed(exsss,{2026,10,20}),
    Raw=list_to_binary([rand:uniform(256)-1||_<-lists:seq(1,8192)]),
    Payload=base64:encode(Raw),
    lists:foreach(fun(U)->
        _=ws_perf_core:batch(U,[{<<"warmup">>,ws_perf_core:make_message(U,<<"warmup">>,Payload)}],Sockets,[0])
    end,Users),
    [erlang:garbage_collect(P)||P<-Pids],erlang:garbage_collect(),
    InitialJson=ws_perf_core:make_message(hd(Users),<<"1">>,Payload),
    S0=ws_perf_core:sample(Pids),Start=ws_perf_core:now_us(),
    put(ws_stress_next_ping,Start+20000000),put(ws_stress_ping_count,0),
    io:format("STRESS_START ~s~n",[jsone:encode(#{variant=>Variant,duration_seconds=>Duration,
        target_business_per_s=>Rate,uid_count=>4,devices_per_uid=>8,connections=>32,
        raw_payload_bytes=>8192,encoded_ciphertext_bytes=>byte_size(Payload),
        initial_wire_json_bytes=>byte_size(InitialJson),
        initial_wire_frame_bytes=>byte_size(imboy_codec:wrap_v2_frame(16#20,0,InitialJson)),
        inflight_business_bound=>16,safety_rss_kib=>1048576,safety_binary_bytes=>268435456,
        safety_ws_queue=>1024,production_login=>false,production_ack_persistence=>false,
        storage_path=>false,group_acl_path=>false,fixture=>synthetic_loopback_c2c,websocket_idle_timeout_ms=>180000,
        client_rfc6455_ping_interval_ms=>20000,production_session_revalidation=>false,
        first_sample=>S0})]),
    try
        {Count,Hist,Peak}=loop(Start,Start+Duration*1000000,Rate,Users,Payload,Sockets,
            0,#{},S0,S0,0,[],S0),
        S1=ws_perf_core:sample(Pids),D=ws_perf_core:delta(S0,S1),
        Elapsed=maps:get(at_us,D),Delivered=Count*Devices,
        Delivered=lists:sum(maps:values(Hist)),
        io:format("STRESS_RESULT ~s~n",[jsone:encode(#{variant=>Variant,status=>completed,
            business_messages=>Count,successful_device_deliveries=>Delivered,errors=>0,
            client_ping_frames_sent=>get(ws_stress_ping_count),
            max_wire_json_bytes=>byte_size(ws_perf_core:make_message(hd(Users),integer_to_binary(Count),Payload)),
            measured_wall_us=>Elapsed,device_per_s=>Delivered*1000000 div Elapsed,
            business_per_s=>Count*1000000 div Elapsed,
            p95_delivery_us_upper_bound=>hist_quantile(Hist,95),
            p99_delivery_us_upper_bound=>hist_quantile(Hist,99),latency_histogram_resolution_us=>10,
            delta=>D,peak_sampled=>Peak,final_sample=>S1})]),
        ws_perf_core:cooldown(Pids,30),ws_perf_core:close(Sockets),halt(0)
    catch Class:Reason:Stack ->
        io:format("STRESS_FAILURE ~p~n",[#{class=>Class,reason=>Reason,stack=>Stack,
            sample=>ws_perf_core:sample(Pids)}]),
        ws_perf_core:close(Sockets),halt(1)
    end.
loop(Start,End,Rate,Users,Payload,Sockets,N,Hist,S0,WindowStart,WindowCount,WindowLat,Peak)->
    Now=ws_perf_core:now_us(),
    maybe_ping(Now,Sockets),
    case Now>=End of
        true->{N,Hist,Peak};
        false->
            U=lists:nth((N div 16 rem length(Users))+1,Users),
            Messages=[begin Id=integer_to_binary(I),{Id,ws_perf_core:make_message(U,Id,Payload)}
                end||I<-lists:seq(N+1,N+16)],
            Lat=ws_perf_core:batch(U,Messages,Sockets,[0]),
            H1=lists:foldl(fun(L,H)->maps:update_with(L div 10,fun(V)->V+1 end,1,H) end,Hist,Lat),
            N1=N+16,WL=Lat++WindowLat,WC=WindowCount+16,
            {NextStart,NextWC,NextWL,NextPeak}=case ws_perf_core:now_us()-maps:get(at_us,WindowStart)>=1000000 of
                true->
                    Sample=ws_perf_core:sample(maps:get(pids,Sockets)),
                    safety(Sample),Delta=ws_perf_core:delta(WindowStart,Sample),
                    io:format("STRESS_WINDOW ~s~n",[jsone:encode(#{elapsed_us=>ws_perf_core:now_us()-Start,
                        business_messages=>WC,successful_device_deliveries=>WC*8,errors=>0,
                        p95_delivery_us=>ws_perf_core:quantile(WL,95),
                        p99_delivery_us=>ws_perf_core:quantile(WL,99),delta=>Delta,sample=>Sample})]),
                    {Sample,0,[],peak(Peak,Sample)};
                false->{WindowStart,WC,WL,Peak}
            end,
            Due=Start+N1*1000000 div Rate,
            Sleep=min(End,Due)-ws_perf_core:now_us(),
            case Sleep>=1000 of true->timer:sleep(Sleep div 1000);false->ok end,
            loop(Start,End,Rate,Users,Payload,Sockets,N1,H1,S0,NextStart,NextWC,NextWL,NextPeak)
    end.
safety(S)->
    cancel_check(),
    true=maps:get(rss_kib,S)<1048576,
    true=maps:get(binary_bytes,S)<268435456,
    true=maps:get(max_ws_queue_sampled,S)<1024,ok.
peak(A,B)->maps:merge(A,maps:from_list([{K,max(maps:get(K,A),maps:get(K,B))}
    ||K<-[binary_bytes,erlang_total_bytes,rss_kib,rss_highwater_kib,max_ws_queue_sampled]])).
hist_quantile(H,P)->
    Need=(lists:sum(maps:values(H))*P+99) div 100,
    hist_find(lists:sort(maps:to_list(H)),Need,0).
hist_find([{Bucket,Count}|Rest],Need,Acc)->
    case Acc+Count>=Need of true->(Bucket+1)*10;false->hist_find(Rest,Need,Acc+Count) end.
env_integer(Key,Default)->list_to_integer(os:getenv(Key,integer_to_list(Default))).

cancel_check()->
    Path=os:getenv("WS_STRESS_STOP_FILE","STOP"),
    case filelib:is_file(Path) of true->error(stop_requested);false->ok end.

maybe_ping(Now,#{connections:=Connections})->
    case Now>=get(ws_stress_next_ping) of
        true->
            [ok=gun:ws_send(C,Ref,ping)||{C,Ref}<-Connections],
            put(ws_stress_ping_count,get(ws_stress_ping_count)+length(Connections)),
            put(ws_stress_next_ping,Now+20000000),ok;
        false->ok
    end.
