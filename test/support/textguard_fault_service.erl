-module(textguard_fault_service).

-export([start/1, stop/1, set/1, count/0, check_text/2, get_info/2]).

start(Port) ->
    ets:new(tg19_fault, [named_table, public, set]),
    ets:insert(tg19_fault, [{count, 0}, {mode, hang}]),
    grpcbox:start_server(#{
        grpc_opts => #{
            service_protos => [textguard_pb],
            services => #{'imboy.textguard.v1.TextGuard' => ?MODULE}
        },
        listen_opts => #{ip => {127, 0, 0, 1}, port => Port},
        pool_opts => #{size => 2}
    }).

stop(Pid) ->
    supervisor:terminate_child(grpcbox_services_simple_sup, Pid),
    catch ets:delete(tg19_fault),
    ok.

set(Mode) ->
    ets:insert(tg19_fault, [{mode, Mode}, {count, 0}]),
    ok.

count() ->
    ets:lookup_element(tg19_fault, count, 2).

check_text(Ctx, Request) ->
    respond(Ctx, Request).

get_info(Ctx, Request) ->
    respond(Ctx, Request).

respond(Ctx, Request) ->
    ets:update_counter(tg19_fault, count, 1),
    Mode = ets:lookup_element(tg19_fault, mode, 2),
    reply(Mode, Ctx, maps:get(request_id, Request, <<>>)).

reply(hang, _Ctx, _RequestId) ->
    receive
        tg19_stop -> exit(normal)
    after 5000 -> exit(normal)
    end;
reply({success, Response}, Ctx, RequestId) ->
    Result =
        case maps:is_key(request_id, Response) of
            true -> Response#{request_id => RequestId};
            false -> Response
        end,
    {ok, Result, Ctx};
reply({error, Status, Code, MetadataMode}, _Ctx, RequestId) ->
    Metadata = metadata(MetadataMode, Code, RequestId),
    throw(
        {grpc_extended_error, #{
            status => integer_to_binary(Status), message => <<>>, trailers => Metadata
        }}
    ).

metadata(missing, _Code, _RequestId) ->
    #{};
metadata(wrong_id, Code, _RequestId) ->
    metadata(valid, Code, <<"tg19-wrong-id">>);
metadata(valid, Code, RequestId) ->
    #{<<"x-textguard-error-code">> => Code, <<"x-textguard-request-id">> => RequestId}.
