-module(micro_community_content_handler).
-moduledoc "Thin authenticated HTTP adapter for scoped micro-community content.".
-export([init/2]).

init(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    Req1 =
        case is_integer(Uid) andalso Uid > 0 of
            false -> elib_response:error(Req0, <<"请先登录"/utf8>>, 401);
            true -> dispatch(maps:get(action, State), Req0, Uid)
        end,
    {ok, Req1, maps:remove(action, State)}.

dispatch(search, Req, Uid) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Options = maps:from_list(cowboy_req:parse_qs(Req)),
            respond(
                Req,
                micro_community_content_logic:search(
                    Uid, cowboy_req:binding(channel_id, Req), Options
                )
            );
        _ ->
            method_error(Req)
    end;
dispatch(Action, Req, Uid) when Action =:= essence; Action =:= tags ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            Body = elib_param:post(Req),
            ChannelId = cowboy_req:binding(channel_id, Req),
            MessageId = cowboy_req:binding(message_id, Req),
            Result =
                case Action of
                    essence ->
                        micro_community_content_logic:set_essence(
                            Uid,
                            ChannelId,
                            MessageId,
                            maps:get(<<"is_essence">>, Body, undefined)
                        );
                    tags ->
                        micro_community_content_logic:set_tags(
                            Uid,
                            ChannelId,
                            MessageId,
                            maps:get(<<"tags">>, Body, undefined)
                        )
                end,
            respond(Req, Result);
        _ ->
            method_error(Req)
    end;
dispatch(_, Req, _) ->
    method_error(Req).

respond(Req, {ok, Result}) -> elib_response:success(Req, Result);
respond(Req, {error, {Code, Msg}}) -> elib_response:error(Req, Msg, Code).
method_error(Req) -> elib_response:error(Req, <<"请求方法不支持"/utf8>>, 405).
