-module(dtl_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

init(Req0, State) ->
    [Op | _] = State,
    case Op of
        login ->
            login(Req0, State)
        % login when guard ->
        %     body
    end.

login(Req, State) ->
    %%     这里的toppage_dtl是我们编译好的模板名
    erlydtl:compile_file("../../priv/templates/login.dtl", toppage_dtl),
    {ok, Body} = toppage_dtl:render([
        {hello, <<"helo imboy">>}
    ]),
    {ok, Req1} = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        unicode:characters_to_binary(Body, utf8),
        Req
    ),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
