-module(handler_init).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    Req1 =
        case lists:keyfind(action, 1, State) of
            {action, init} ->
                api_init(Req0);
            {action, help} ->
                get_help(Req0);
            false ->
                Req0
        end,
    {ok, Req1, State}.


api_init(Req0) ->
    Data = aas_api_init:data(),
    dto_resp_json:success(Req0, Data, "操作成功.").


get_help(Req0) ->
    Body = "
        <meta charset=\"utf-8\"/>
        <meta http-equiv=\"Content-Language\" content=\"zh-CN\">
        <h1>API列表</h1>
        <ol>
            <li><a href=\"/init\" target=\"_blank\">/init  GET</a></li>
            <li><a href=\"/conversation/online\" target=\"_blank\">
                /conversation/online  GET</a></li>
        </ol>
    ",
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html">>},
                     unicode:characters_to_binary(Body, utf8),
                     Req0).
