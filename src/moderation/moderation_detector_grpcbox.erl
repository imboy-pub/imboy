-module(moderation_detector_grpcbox).

-include_lib("grpcbox/include/grpcbox.hrl").

-export([call/4, request/3]).

-define(MAX_BYTES, 262144).

call(_Endpoint, Method, Request, Budget) ->
    normalize(request(Method, Request, Budget)).

request(Method, Request, Budget) ->
    Conf = config_ds:env(moderation_detector, []),
    case proplists:get_value(token, Conf) of
        Token when is_binary(Token), Token =/= <<>> ->
            request(
                Method, Request, Budget, Token, proplists:get_value(grpc_channel, Conf, textguard)
            );
        _ ->
            {error, not_ready}
    end.

request(Method, Request, Budget, Token, Channel) ->
    Metadata = #{
        <<"authorization">> => <<"Bearer ", Token/binary>>,
        <<"x-textguard-request-id">> => maps:get(request_id, Request, <<>>)
    },
    Context = grpcbox_metadata:append_to_outgoing_ctx(
        ctx:with_deadline_after(Budget, millisecond), Metadata
    ),
    {Path, Input, Output} = method(Method),
    Definition = #grpcbox_def{
        service = 'imboy.textguard.v1.TextGuard',
        message_type = atom_to_binary(Input),
        marshal_fun = fun(Value) -> encode(Value, Input) end,
        unmarshal_fun = fun(Bytes) -> decode(Bytes, Output) end
    },
    try
        textguard_grpc_call:call(Context, Path, proto_request(Request), Definition, #{
            channel => Channel
        })
    catch
        throw:text_too_large -> {error, text_too_large};
        exit:{timeout, _} -> {error, timeout};
        exit:timeout -> {error, timeout}
    end.

encode(Value, Type) ->
    Bytes = textguard_pb:encode_msg(Value, Type),
    case byte_size(Bytes) =< ?MAX_BYTES of
        true -> Bytes;
        false -> throw(text_too_large)
    end.

decode(Bytes, Type) when byte_size(Bytes) =< ?MAX_BYTES ->
    textguard_pb:decode_msg(Bytes, Type);
decode(_, _) ->
    error(response_too_large).

method(check_text) ->
    {<<"/imboy.textguard.v1.TextGuard/CheckText">>, 'imboy.textguard.v1.CheckTextRequest',
        'imboy.textguard.v1.CheckTextResponse'};
method(get_info) ->
    {<<"/imboy.textguard.v1.TextGuard/GetInfo">>, 'imboy.textguard.v1.GetInfoRequest',
        'imboy.textguard.v1.GetInfoResponse'}.

proto_request(#{scene := Scene} = Request) ->
    Request#{scene => scene(Scene)};
proto_request(Request) ->
    Request.

scene(<<"channel_message">>) -> 'CHANNEL_MESSAGE';
scene(<<"moment_post">>) -> 'MOMENT_POST';
scene(<<"profile_field">>) -> 'PROFILE_FIELD'.

normalize({ok, Response, _Metadata}) when is_map(Response) ->
    {ok, Response};
normalize({error, {Status, _Message}, Metadata}) when is_binary(Status), is_map(Metadata) ->
    try
        {error, #{
            grpc_status => binary_to_integer(Status),
            metadata => grpcbox_metadata:join([
                maps:get(headers, Metadata, #{}), maps:get(trailers, Metadata, #{})
            ])
        }}
    catch
        _:_ -> {error, malformed_transport_response}
    end;
normalize({error, Reason}) when
    Reason =:= timeout;
    Reason =:= not_ready;
    Reason =:= text_too_large;
    Reason =:= connection_failed;
    Reason =:= overloaded
->
    {error, Reason};
normalize({error, Reason}) when
    Reason =:= econnrefused;
    Reason =:= closed;
    Reason =:= undefined_channel;
    Reason =:= no_endpoints;
    Reason =:= no_available_subchannel
->
    {error, connection_failed};
normalize(_) ->
    {error, malformed_transport_response}.
