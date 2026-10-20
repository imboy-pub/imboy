-module(ws_perf_core).
-export([boot/0,load/1,open/4,close/1,batch/4,make_message/3,sample/1,quantile/2,
         delta/2,now_us/0,cooldown/2]).
boot() ->
    4=erlang:system_info(schedulers_online),
    ClockTicks=list_to_integer(string:trim(os:cmd("getconf CLK_TCK"))),
    put(ws_perf_clock_ticks,ClockTicks),
    put(ws_perf_out_of_order,0),
    {ok,_}=application:ensure_all_started(cowboy),
    {ok,_}=application:ensure_all_started(gun),
    ok=imboy_syn:init(), ok=ack_retry_cache:init_table(),
    {ok,_}=elib_metric:start_link(),
    {ok,OtpVersion}=file:read_file(filename:join([code:root_dir(),"releases",
        erlang:system_info(otp_release),"OTP_VERSION"])),
    io:format("ENV ~s~n",[jsone:encode(#{otp=>string:trim(OtpVersion),
        erts=>list_to_binary(erlang:system_info(version)),
        started_utc=>list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second),
            [{unit,second},{offset,"Z"}])) ,schedulers=>erlang:system_info(schedulers_online),
        logical_available=>erlang:system_info(logical_processors_available),
        dirty_cpu_schedulers_online=>erlang:system_info(dirty_cpu_schedulers_online),
        async_threads=>erlang:system_info(thread_pool_size),clock_ticks_per_second=>ClockTicks,
        cpu_quota=>unknown,exclusive_cpus=>false,jit=>erlang:system_info(emu_flavor),
        synthetic_seed=>[2026,10,20],random_algorithm=>exsss})]).
load(Variant) ->
    Root=filename:dirname(filename:dirname(code:which(?MODULE))),
    [begin code:purge(M),code:delete(M),
        {module,M}=code:load_abs(filename:join([Root,atom_to_list(Variant),atom_to_list(M)]))
     end || M<-[websocket_handler,websocket_delivery_guard]], ok.
open(Users,Devices,Protocol,Framing) ->
    Routes=[{path(U,D),ws_perf_socket,#{parent=>self(),uid=>U,current_uid=>U,
        did=>integer_to_binary(D),protocol=>Protocol,framing=>Framing}}
        ||U<-Users,D<-lists:seq(1,Devices)],
    Dispatch=cowboy_router:compile([{'_',Routes}]),
    {ok,_}=cowboy:start_clear(perf_listener,[{ip,{127,0,0,1}},{port,0}],
        #{env=>#{dispatch=>Dispatch}}),
    Port=ranch:get_port(perf_listener),
    Connections=[connect(Port,path(U,D))||U<-Users,D<-lists:seq(1,Devices)],
    Pids=[receive {registered,P}->P after 10000->error(register_timeout) end||_<-Connections],
    #{connections=>Connections,pids=>Pids,users=>Users,devices=>Devices,protocol=>Protocol,framing=>Framing}.
path(U,D)->iolist_to_binary(["/",integer_to_binary(U),"/",integer_to_binary(D)]).
connect(Port,Path)->
    {ok,C}=gun:open("127.0.0.1",Port,#{protocols=>[http],retry=>0}),
    {ok,http}=gun:await_up(C), Ref=gun:ws_upgrade(C,Path),
    receive {gun_upgrade,C,Ref,[<<"websocket">>],_}->ok
    after 10000->error(upgrade_timeout) end,{C,Ref}.
close(#{connections:=Connections,pids:=Pids,users:=Users}) ->
    [gun:close(C)||{C,_}<-Connections],ok=cowboy:stop_listener(perf_listener),
    [begin R=monitor(process,P),receive {'DOWN',R,process,P,_}->ok
        after 5000->error(close_timeout) end end||P<-Pids],
    wait_members_empty(Users,100),flush().
wait_members_empty(_,0)->error(syn_cleanup_timeout);
wait_members_empty(Users,N)->
    case lists:all(fun(U)->imboy_syn:list_by_uid(U)=:=[] end,Users) of
        true->ok;
        false->timer:sleep(10),wait_members_empty(Users,N-1)
    end.
flush()->receive _->flush() after 0->ok end.
make_message(Uid,Id,Payload)->
    jsone:encode(#{<<"type">>=><<"C2C">>,<<"id">>=>Id,<<"from">>=><<"99">>,
        <<"to">>=>integer_to_binary(Uid),<<"sender_did">>=><<"sender">>,<<"conv_seq">>=>42,
        <<"payload">>=>#{<<"ciphertext">>=>Payload},
        <<"e2ee">>=>#{<<"protected_header">>=><<"opaque">>}}).
batch(Uid,Messages,#{devices:=Devices,protocol:=Protocol,framing:=Framing},Delays)->
    %% Match the complete unique frame, independent of equal-deadline timer order.
    %% No client JSON decode; duplicates/unexpected frames remain hard failures.
    Sent=[begin
        Expected=case {Protocol,Framing} of
            {_,v2}->{binary,imboy_codec:wrap_v2_frame(16#20,0,Bin)};
            {json,_}->{text,Bin};
            {protobuf,_}->case imboy_codec:encode(protobuf,jsone:decode(Bin,[{object_format,map}])) of
                <<>>->{text,Bin}; Encoded->{binary,Encoded}
            end
        end,
        T=now_us(),ok=message_ds:send_next(Uid,Id,Bin,Delays),{T,Expected}
    end||{Id,Bin}<-Messages],
    ExpectedMap=maps:from_list([{Frame,{T,I}}||{{T,Frame},I}<-lists:zip(Sent,lists:seq(1,length(Sent)))]),
    true=map_size(ExpectedMap)=:=length(Sent),
    receive_frames(length(Messages)*Devices,ExpectedMap,#{},[]).
receive_frames(0,_,_,Acc)->Acc;
receive_frames(N,ExpectedMap,Seen,Acc)->
    receive
        {gun_ws,C,_,Frame}->
            case maps:find(Frame,ExpectedMap) of
                error->error({unexpected_frame,frame_description(Frame)});
                {ok,{T,Index}}->
                    Identity={C,Frame},
                    false=maps:is_key(Identity,Seen),
                    Count=maps:get(C,Seen,0)+1,
                    case Index=:=Count of
                        true->ok;
                        false->put(ws_perf_out_of_order,get(ws_perf_out_of_order)+1)
                    end,
                    receive_frames(N-1,ExpectedMap,Seen#{C=>Count,Identity=>true},[now_us()-T|Acc])
            end;
        {gun_error,_,_,Reason}->error({gun_error,Reason});
        {gun_down,_,_,Reason,_,_}->error({gun_down,Reason})
    after 10000->error({delivery_timeout,N}) end.
frame_description({Type, Bytes}) when is_binary(Bytes) -> {Type, byte_size(Bytes)};
frame_description(Other) -> Other.
now_us()->erlang:monotonic_time(microsecond).
sample(Pids)->
    {Red,_}=erlang:statistics(reductions),{GC,Words,_}=erlang:statistics(garbage_collection),
    {CPU,_}=erlang:statistics(runtime),
    #{out_of_order_receipts=>get(ws_perf_out_of_order),at_us=>now_us(),reductions=>Red,gc_count=>GC,gc_words=>Words,cpu_ms=>CPU,
        os_process_cpu_ms=>proc_cpu_ms(),
        binary_bytes=>erlang:memory(binary),erlang_total_bytes=>erlang:memory(total),
        rss_kib=>proc_kib(<<"VmRSS:">>),rss_highwater_kib=>proc_kib(<<"VmHWM:">>),
        max_ws_queue_sampled=>lists:max([0|[case process_info(P,message_queue_len) of
            {_,N}->N;undefined->0 end||P<-Pids]])}.
proc_kib(Key)->
    case file:read_file("/proc/self/status") of
        {ok,Bin}->case [Line||Line<-binary:split(Bin,<<"\n">>,[global]),
            binary:match(Line,Key)=:={0,byte_size(Key)}] of
            [Line]->[N|_]=string:lexemes(binary_to_list(binary:part(Line,byte_size(Key),byte_size(Line)-byte_size(Key)))," \t"),list_to_integer(N);
            _->0 end;
        _->0
    end.
proc_cpu_ms()->
    case file:read_file("/proc/self/stat") of
        {ok,Bin}->
            [_,Rest]=binary:split(Bin,<<") ">>),
            Fields=string:lexemes(binary_to_list(Rest)," "),
            User=list_to_integer(lists:nth(12,Fields)),
            System=list_to_integer(lists:nth(13,Fields)),
            (User+System)*1000 div get(ws_perf_clock_ticks);
        _->0
    end.
delta(A,B)->maps:from_list([{K,maps:get(K,B)-maps:get(K,A)}
    ||K<-[out_of_order_receipts,at_us,reductions,gc_count,gc_words,cpu_ms,os_process_cpu_ms]]).
quantile([],_) -> 0;
quantile(L,P)->S=lists:sort(L),lists:nth(max(1,(length(S)*P+99) div 100),S).
cooldown(Pids,Seconds)->
    [erlang:garbage_collect(P)||P<-Pids],erlang:garbage_collect(),
    [begin timer:sleep(1000),io:format("COOLDOWN ~s~n",[jsone:encode(sample(Pids))])
     end||_<-lists:seq(1,Seconds)],ok.
