-module(enterprise_internal_request).
-moduledoc "HMAC authenticated OA integration with live business attribution checks.".
-export([read_body/2, params/1]).

read_body(#{enterprise_internal_body := Body} = Req, _Options) ->
    {ok, Body, Req};
read_body(Req, Options) ->
    cowboy_req:read_body(Req, Options).

params(#{enterprise_internal_body := <<>>}) ->
    #{};
params(#{enterprise_internal_body := Body}) ->
    try jsone:decode(Body) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
params(Req) ->
    elib_param:post(Req).
