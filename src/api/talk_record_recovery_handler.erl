-module(talk_record_recovery_handler).
-moduledoc "JWT和设备签名沿正式路由链校验的精确恢复入口。".
-export([init/2]).

init(Req, State) ->
    Response =
        case cowboy_req:method(Req) of
            <<"GET">> -> lookup(Req, maps:get(current_uid, State, 0));
            _ -> talk_record_http:method_not_allowed(Req)
        end,
    {ok, Response, State}.

lookup(Req, Uid) ->
    Query = maps:from_list(cowboy_req:parse_qs(Req)),
    case talk_record_recovery_logic:lookup(Uid, Query) of
        {ok, Payload} ->
            Rev = maps:get(<<"revision">>, Payload),
            R = cowboy_req:set_resp_header(<<"etag">>, talk_record_ds:etag(Rev), Req),
            elib_response:success(R, Payload);
        {error, Reason} ->
            talk_record_http:respond_error(Req, Reason)
    end.
