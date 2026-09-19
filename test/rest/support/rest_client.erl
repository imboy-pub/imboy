-module(rest_client).

-export([request/5, post/4]).

-define(TIMEOUT, 10000).

-spec post(inet:port_number(), binary(), map() | binary(), map()) -> map().
post(Port, Path, Body, Headers) ->
    request(Port, <<"POST">>, Path, Body, Headers).

-spec request(inet:port_number(), binary(), binary(), map() | binary(), map()) -> map().
request(Port, Method, Path, Body0, Headers0) ->
    Body = encode_body(Body0),
    Headers = maps:merge(
        #{
            <<"accept">> => <<"application/json">>,
            <<"content-type">> => <<"application/json">>
        },
        Headers0
    ),
    Started = erlang:monotonic_time(millisecond),
    {ok, ConnPid} = gun:open("127.0.0.1", Port, #{protocols => [http]}),
    try
        {ok, http} = gun:await_up(ConnPid, ?TIMEOUT),
        StreamRef = gun:request(ConnPid, Method, Path, maps:to_list(Headers), Body),
        {Status, ResponseHeaders, RawBody} = await_response(ConnPid, StreamRef),
        #{
            status => Status,
            headers => maps:from_list(ResponseHeaders),
            body => decode_body(RawBody),
            raw_body => RawBody,
            duration_ms => erlang:monotonic_time(millisecond) - Started
        }
    after
        gun:close(ConnPid)
    end.

await_response(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef, ?TIMEOUT) of
        {response, fin, Status, Headers} ->
            {Status, Headers, <<>>};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef, ?TIMEOUT),
            {Status, Headers, Body};
        Other ->
            erlang:error({http_request_failed, Other})
    end.

encode_body(Body) when is_map(Body) ->
    jsone:encode(Body, [native_utf8]);
encode_body(Body) when is_binary(Body) ->
    Body.

decode_body(<<>>) ->
    #{};
decode_body(Body) ->
    try jsone:decode(Body, [{object_format, map}]) of
        Json -> Json
    catch
        _:_ -> Body
    end.
