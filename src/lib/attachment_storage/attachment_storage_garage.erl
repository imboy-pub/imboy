%%% Garage I/O 使用已校验位置与凭据快照，不在派发时重读 registry/global endpoint。
-module(attachment_storage_garage).
-behaviour(attachment_storage_driver).
-export([put/4,head/3,delete/3,presign_put/4,presign_get/4,read/3,put_snapshot/4,read_snapshot/1,delete_snapshot/1,presign_snapshot/2]).
read(Ns,C,K) -> with_snapshot(Ns,C,K,fun read_snapshot/1).
read_snapshot(I) ->
    Url=signed(I,<<"GET">>,<<>>,calendar:universal_time(),60),
    case httpc:request(get,{binary_to_list(Url),[]},[{timeout,30000}],[{body_format,binary}]) of
        {ok,{{_,200,_},_,Bytes}}->{ok,Bytes};
        {ok,{{_,404,_},_,_}}->{error,not_found};
        {ok,{{_,Status,_},_,_}}->{error,{storage_status,Status}};
        {error,R}->{error,R}
    end.
put(Ns,C,K,Body) ->
    Now=calendar:universal_time(),
    Exp=calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now)+300),
    with_snapshot(Ns,C,K,fun(I)->put_snapshot(I,Body,<<"application/octet-stream">>,#{issued_at=>Now,expires_at=>Exp}) end).
put_snapshot(I,{file,Path},Mime,Ctx) ->
    case file:read_file(Path) of {ok,B}->put_snapshot(I,B,Mime,Ctx);E->E end;
put_snapshot(I,Bytes,Mime,#{issued_at:=Issued,expires_at:=Expiry}) when is_binary(Bytes) ->
    Diff=calendar:datetime_to_gregorian_seconds(Expiry)-calendar:datetime_to_gregorian_seconds(Issued),
    case Diff>0 andalso Diff=<604800 andalso calendar:datetime_to_gregorian_seconds(Expiry)>calendar:datetime_to_gregorian_seconds(calendar:universal_time()) of
        true->
            Url=signed(I,<<"PUT">>,Mime,Issued,Diff),
            case httpc:request(put,{binary_to_list(Url),[],binary_to_list(Mime),Bytes},[{timeout,30000}],[]) of
                {ok,{{_,Status,_},_,_}} when Status>=200,Status<300->ok;
                {ok,{{_,Status,_},_,_}}->{error,{storage_status,Status}};
                {error,R}->{error,R}
            end;
        false->{error,invalid_signature_context}
    end;
put_snapshot(_,_,_,_)->{error,invalid_signature_context}.
head(Ns,C,K) ->
    case read(Ns,C,K) of {ok,B}->{ok,#{size=>byte_size(B),content_type=><<"application/octet-stream">>}};E->E end.
delete(Ns,C,K) -> with_snapshot(Ns,C,K,fun delete_snapshot/1).
delete_snapshot(I) ->
    Url=signed(I,<<"DELETE">>,<<>>,calendar:universal_time(),60),
    case httpc:request(delete,{binary_to_list(Url),[]},[{timeout,30000}],[]) of
        {ok,{{_,Status,_},_,_}} when Status>=200,Status<300->ok;
        {ok,{{_,404,_},_,_}}->{error,not_found};
        {ok,{{_,Status,_},_,_}}->{error,{storage_status,Status}};
        {error,R}->{error,R}
    end.

presign_put(Ns,C,K,T)->with_snapshot(Ns,C,K,fun(I)->{ok,signed(I,<<"PUT">>,<<"application/octet-stream">>,calendar:universal_time(),T)} end).
presign_get(Ns,C,K,T)->with_snapshot(Ns,C,K,fun(I)->{ok,signed(I,<<"GET">>,<<>>,calendar:universal_time(),T)} end).
presign_snapshot(I,Expiry) -> {ok,signed(I,<<"GET">>,<<>>,calendar:universal_time(),Expiry)}.

signed(#{endpoint:=EP,container:=C,key:=K,garage_config:=Cfg},Method,Mime,Now,Ttl)->
    elib_s3_sign:presign_url_at(Method,EP,C,K,Mime,Now,Ttl,Cfg).
with_snapshot(Ns,C,K,Fun) ->
    case attachment_storage:resolve(Ns) of
        {ok,#{driver:=<<"garage">>,endpoint:=EP,container:=C}} ->
            Cfg=application:get_env(imboy,garage,#{}),
            case maps:get(endpoint,Cfg,undefined)=:=EP of
                true->Fun(#{endpoint=>EP,container=>C,key=>K,garage_config=>Cfg});
                false->{error,location_changed}
            end;
        _->{error,location_unverified}
    end.
