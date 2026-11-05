-module(imboy_widget_origin).

-export([base/0]).

-spec base() -> binary().
base() ->
    Base = application:get_env(imboy, widget_base_url, <<>>),
    Domain = application:get_env(imboy, widget_domain, <<>>),
    case is_binary(Base) andalso is_binary(Domain) andalso byte_size(Domain) > 0 of
        true ->
            case Base =:= <<"https://", Domain/binary>> andalso valid_origin(Base) of
                true -> Base;
                false -> <<>>
            end;
        false ->
            <<>>
    end.

valid_origin(Base) ->
    case uri_string:parse(Base) of
        #{scheme := <<"https">>, host := Host} = Uri when is_binary(Host), byte_size(Host) > 0 ->
            Port = maps:get(port, Uri, 443),
            is_integer(Port) andalso Port > 0 andalso Port =< 65535 andalso
                not maps:is_key(userinfo, Uri) andalso not maps:is_key(query, Uri) andalso
                not maps:is_key(fragment, Uri) andalso maps:get(path, Uri, <<>>) =:= <<>> andalso
                binary:match(Host, <<"..">>) =:= nomatch andalso
                re:run(
                    Host,
                    <<"\\A[A-Za-z0-9]([A-Za-z0-9.-]*[A-Za-z0-9])?\\z">>,
                    [{capture, none}]
                ) =:= match;
        _ ->
            false
    end.
