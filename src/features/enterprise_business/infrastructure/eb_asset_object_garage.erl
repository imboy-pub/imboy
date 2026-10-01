%%% Private enterprise objects use the existing Garage bucket and SigV4 signer.
%%% URLs and credentials stay inside this adapter; business callers receive bytes only.
-module(eb_asset_object_garage).
-export([put/3, get/2, delete/2]).

put(Key, Bytes, Meta) when is_binary(Key), is_binary(Bytes), byte_size(Bytes) > 0 ->
    configured(fun() ->
        Mime = maps:get(mime, Meta, <<"application/octet-stream">>),
        case elib_oss:put_object(elib_oss:get_bucket(<<"private">>), Key, Bytes, Mime) of
            ok -> ok;
            {error, _} -> {error, storage_unavailable}
        end
    end);
put(_Key, _Bytes, _Meta) ->
    {error, invalid_payload}.

get(Key, Prefix) ->
    scoped(Key, Prefix, fun() ->
        Url = elib_s3_sign:presign_get(
            elib_oss:endpoint(), elib_oss:get_bucket(<<"private">>), Key, 60
        ),
        case request(get, Url) of
            {ok, 200, Bytes} -> {ok, #{bytes => Bytes, size => byte_size(Bytes)}};
            {ok, 404, _} -> {error, not_found};
            {ok, Code, _} -> {error, {http_status, Code}};
            {error, _} = Error -> Error
        end
    end).

delete(Key, Prefix) ->
    scoped(Key, Prefix, fun() ->
        Url = elib_s3_sign:presign_delete(
            elib_oss:endpoint(), elib_oss:get_bucket(<<"private">>), Key, 60
        ),
        case request(delete, Url) of
            {ok, Code, _} when Code =:= 200; Code =:= 204 -> ok;
            {ok, 404, _} -> {error, not_found};
            {ok, Code, _} -> {error, {http_status, Code}};
            {error, _} = Error -> Error
        end
    end).

scoped(Key, Prefix, Fun) when is_binary(Key), is_binary(Prefix), byte_size(Prefix) > 0 ->
    Size = byte_size(Prefix),
    case byte_size(Key) >= Size andalso binary:part(Key, 0, Size) =:= Prefix of
        true -> configured(Fun);
        false -> {error, out_of_scope}
    end;
scoped(_Key, _Prefix, _Fun) ->
    {error, out_of_scope}.

configured(Fun) ->
    Config = elib_oss:garage_config(),
    case {maps:get(access_key, Config, <<>>), maps:get(secret_key, Config, <<>>)} of
        {Access, Secret} when
            is_binary(Access),
            byte_size(Access) > 0,
            is_binary(Secret),
            byte_size(Secret) > 0
        ->
            try
                Fun()
            catch
                _:_ -> {error, storage_unavailable}
            end;
        _ ->
            {error, storage_not_configured}
    end.

request(Method, Url) ->
    case
        httpc:request(
            Method,
            {binary_to_list(Url), []},
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, Code, _}, _, Bytes}} -> {ok, Code, Bytes};
        {error, _} -> {error, storage_unavailable}
    end.
