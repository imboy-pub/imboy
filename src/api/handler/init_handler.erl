-module(init_handler).
-behavior(cowboy_rest).

-export([init/2]).


init(Req0, State) ->
    [Op | _] = State,
    case Op of
        init ->
            api_init(Req0);
        refreshtoken ->
            refreshtoken(Req0);
        help ->
            get_help(Req0, State)
    end.


api_init(Req0) ->
    Data = api_init_aas:data(),
    resp_json_dto:success(Req0, Data, "操作成功.").

refreshtoken(Req0) ->
    % Token = cowboy_req:header(<<"imboy-token">>, Req0),
    Refreshtoken = cowboy_req:header(<<"imboy-refreshtoken">>, Req0),
    io:format("refreshtoken  ~p~n",[Refreshtoken]),
    case token_ds:decrypt_token(Refreshtoken) of
        {ok, Id, _ExpireAt} ->
            Data = [
                {<<"token">>, token_ds:encrypt_token(Id)}
                , {<<"refreshtoken">>, token_ds:encrypt_refreshtoken(Id)}
            ],
            resp_json_dto:success(Req0, Data, "操作成功.");
        {error, Code, Msg, _Li} ->
            resp_json_dto:error(Req0, Msg, Code)
    end.

get_help(Req0, State) ->
    Body = "
        <meta charset=\"utf-8\"/>
        <meta http-equiv=\"Content-Language\" content=\"zh-CN\">
        <h1>API列表</h1>
        <ol>
            <li><a href=\"/passport/login.html\" target=\"_blank\">/passport/login.html  GET</a></li>
            <li><a href=\"/init\" target=\"_blank\">/init  GET</a></li>

            <li> /passport/login  POST</li>

        </ol>
    ",
    {ok, Req1} = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        unicode:characters_to_binary(Body, utf8),
        Req0
    ),
    {cowboy_rest, Req1, State}.

