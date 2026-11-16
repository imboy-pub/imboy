-module(web_attachment_targets).
-export([bootstrap/0, resolve/1, contract/3, resolve/4, valid_base/1, resolve_ticket/1, resolve_garage/3, matches_garage/3, web_ticket_url/1]).

%% Only explicitly configured public endpoints are shared with a Web client.
bootstrap() ->
    contract(garage_endpoint(), config_ds:env(base_url, <<>>), attachment_edge_redirect:edge_base()).

resolve(Url) ->
    resolve(Url, garage_endpoint(), config_ds:env(base_url, <<>>), attachment_edge_redirect:edge_base()).

garage_endpoint() ->
    Cfg0 = application:get_env(imboy, garage, #{}),
    Cfg = case is_map(Cfg0) of true -> Cfg0; false -> #{} end,
    case maps:find(public_endpoint, Cfg) of
        {ok, {env, Name}} when is_binary(Name) -> env(Name, <<>>);
        {ok, {env, Name, Default}} when is_binary(Name) -> env(Name, Default);
        {ok, Value} -> Value;
        error -> <<>>
    end.

env(Name, Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        [] -> Default;
        Value -> unicode:characters_to_binary(Value)
    end.

contract(Garage, Base, Edge) ->
    #{version => 1, garage => checked(Garage),
      edge => case {valid_base(normalize_api_base(Base)), valid_base(Edge)} of
          {true, true} -> Edge;
          _ -> null
      end}.
checked(Value) -> case valid_base(Value) of true -> Value; false -> null end.

%% Match ticket_view/2: the configured base drops trailing slashes before
%% appending the fixed content route. No other normalization is permitted.
normalize_api_base(Value) when is_binary(Value) -> string:trim(Value, trailing, "/");
normalize_api_base(_) -> <<>>.

%% Conservative canonical URL grammar: HTTPS, or HTTP only on 127.0.0.1.
%% No percent-encoded path components, empty/dot segments, redirects or userinfo.
valid_base(Value) when is_binary(Value), byte_size(Value) > 0 ->
    try uri_string:parse(Value) of
        #{scheme := Scheme, host := Host, path := Path} = U ->
            (Scheme =:= <<"https">> orelse (Scheme =:= <<"http">> andalso Host =:= <<"127.0.0.1">>))
            andalso not maps:is_key(userinfo, U) andalso not maps:is_key(query, U)
            andalso not maps:is_key(fragment, U) andalso safe_path(Path)
            andalso valid_host(Host) andalso valid_port(Scheme, U)
            andalso uri_string:recompose(U) =:= Value;
        _ -> false
    catch _:_ -> false end;
valid_base(_) -> false.

valid_host(Host) when is_binary(Host), byte_size(Host) > 0, byte_size(Host) =< 253 ->
    re:run(Host, <<"^[a-z0-9](?:[a-z0-9.-]*[a-z0-9])?$">>, [{capture, none}]) =:= match
    andalso not lists:any(fun(S) -> S =:= <<>> orelse byte_size(S) > 63 orelse
        binary:first(S) =:= $- orelse binary:last(S) =:= $- end, binary:split(Host, <<".">>, [global]));
valid_host(_) -> false.
valid_port(Scheme, U) ->
    case maps:find(port, U) of
        error -> true;
        {ok, P} when is_integer(P), P > 0, P =< 65535 ->
            not ((Scheme =:= <<"http">> andalso P =:= 80) orelse
                 (Scheme =:= <<"https">> andalso P =:= 443));
        _ -> false
    end.
safe_path(<<>>) -> true;
safe_path(<<"/", Rest/binary>>) when byte_size(Rest) > 0 ->
    lists:all(fun(S) -> S =/= <<>> andalso S =/= <<".">> andalso S =/= <<"..">> andalso
        re:run(S, <<"^[A-Za-z0-9._~-]+$">>, [{capture, none}]) =:= match
    end, binary:split(Rest, <<"/">>, [global]));
safe_path(_) -> false.

resolve(Url, Garage, Base, Edge) when is_binary(Url) ->
    case ticket(Url, Base, Edge) of
        {ok, _} = Result -> Result;
        error -> garage_url(Url, Garage)
    end;
resolve(_, _, _, _) -> {error, forbidden}.

ticket(Url, Base0, Edge) ->
    Base = normalize_api_base(Base0),
    case valid_base(Base) andalso valid_base(Edge) of
        false -> error;
        true ->
            Prefix = <<Base/binary, "/api/v1/attachment/content?ticket=">>,
            case Url of
                <<Prefix:(byte_size(Prefix))/binary, Ticket/binary>> when byte_size(Ticket) > 0 ->
                    case re:run(Ticket, <<"^[A-Za-z0-9._~-]+$">>, [{capture, none}]) of
                        match -> {ok, <<Edge/binary, "?ticket=", Ticket/binary>>};
                        _ -> error
                    end;
                _ -> error
            end
    end.

garage_url(Url, Garage) ->
    case valid_base(Garage) of
        false -> {error, forbidden};
        true ->
            try uri_string:parse(Url) of
                #{scheme := _, host := _, path := Path, query := Query} = U ->
                    Prefix = <<Garage/binary, "/">>,
                    case Url of
                        <<Prefix:(byte_size(Prefix))/binary, _/binary>> ->
                            case safe_object_path(Path) andalso byte_size(Query) > 0 andalso
                                not maps:is_key(userinfo, U) andalso not maps:is_key(fragment, U) of
                                true -> {ok, Url}; false -> {error, forbidden}
                            end;
                        _ -> {error, forbidden}
                    end;
                _ -> {error, forbidden}
            catch _:_ -> {error, forbidden} end
    end.

%% Match SigV4 path-style encoding exactly without rewriting the signed URL.
%% A decoded percent followed by hex is rejected to prevent double-decode drift.
safe_object_path(<<"/", Rest/binary>>) when byte_size(Rest) > 0 ->
    lists:all(fun safe_object_segment/1, binary:split(Rest, <<"/">>, [global]));
safe_object_path(_) -> false.

safe_object_segment(<<>>) -> false;
safe_object_segment(Segment) ->
    try uri_string:percent_decode(Segment) of
        Decoded when is_binary(Decoded) ->
            Decoded =/= <<".">> andalso Decoded =/= <<"..">> andalso
            not lists:any(fun(B) -> B =:= $/ orelse B =:= $\\ orelse B < 32 orelse B =:= 127 end, binary_to_list(Decoded)) andalso
            re:run(Decoded, <<"%[0-9A-Fa-f]{2}">>, [{capture, none}]) =:= nomatch andalso
            encode_segment(Decoded) =:= Segment;
        _ -> false
    catch _:_ -> false end.
encode_segment(Bin) -> iolist_to_binary([encode_byte(B) || <<B>> <= Bin]).
encode_byte(B) when B >= $a, B =< $z; B >= $A, B =< $Z; B >= $0, B =< $9;
                    B =:= $-; B =:= $_; B =:= $.; B =:= $~ -> B;
encode_byte(B) -> io_lib:format("%~2.16.0B", [B]).

%% Only called with container/key obtained from an ACL-authorized frozen Garage
%% location. Compare the exact signer-generated path; never decode it again.
resolve_garage(Url, Bucket, Key) when is_binary(Url), is_binary(Bucket), is_binary(Key) ->
    Endpoint = garage_endpoint(),
    case valid_base(Endpoint) andalso valid_bucket(Bucket) andalso valid_raw_key(Key) of
        false -> {error, forbidden};
        true ->
            Encoded = elib_s3_sign:uri_encode_path(Key),
            Prefix = <<Endpoint/binary, "/", Bucket/binary, "/", Encoded/binary, "?">>,
            case Url of
                <<Prefix:(byte_size(Prefix))/binary, Query/binary>> when byte_size(Query) > 0 ->
                    case binary:match(Query, <<"#">>) of nomatch -> {ok, Url}; _ -> {error, forbidden} end;
                _ -> {error, forbidden}
            end
    end;
resolve_garage(_, _, _) -> {error, forbidden}.

resolve_ticket(Url) ->
    case ticket(Url, config_ds:env(base_url, <<>>), attachment_edge_redirect:edge_base()) of
        {ok, _} = Result -> Result;
        error -> {error, forbidden}
    end.

valid_bucket(B) -> re:run(B, <<"^[a-z0-9][a-z0-9.-]*[a-z0-9]$">>, [{capture, none}]) =:= match.
valid_raw_key(Key) when byte_size(Key) > 0 ->
    lists:all(fun(S) -> S =/= <<>> andalso S =/= <<".">> andalso S =/= <<"..">> andalso
        not lists:any(fun(B) -> B =:= $\\ orelse B < 32 orelse B =:= 127 end, binary_to_list(S))
    end, binary:split(Key, <<"/">>, [global]));
valid_raw_key(_) -> false.

matches_garage(Endpoint, Bucket, Key) ->
    is_binary(Endpoint) andalso Endpoint =:= garage_endpoint() andalso
    valid_base(Endpoint) andalso is_binary(Bucket) andalso valid_bucket(Bucket) andalso
    is_binary(Key) andalso valid_raw_key(Key).

web_ticket_url(Ticket) when is_binary(Ticket), byte_size(Ticket) > 0 ->
    Base = normalize_api_base(config_ds:env(base_url, <<>>)),
    Edge = attachment_edge_redirect:edge_base(),
    case valid_base(Base) andalso valid_base(Edge) andalso
        re:run(Ticket, <<"^[A-Za-z0-9._~-]+$">>, [{capture, none}]) =:= match of
        true -> {ok, <<Edge/binary, "?ticket=web.", Ticket/binary>>};
        false -> {error, forbidden}
    end;
web_ticket_url(_) -> {error, forbidden}.
