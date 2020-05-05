-module(auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% 这个是回调函数
execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    NeedAuth = route_helper:need_auth_paths(),
    Need = lists:member(Path, NeedAuth),
    case Need of
        true ->
            Token = cowboy_req:header(<<"imboy-token">>, Req),
            case token_ds:decrypt_token(Token) of
                {ok, _Id, _ExpireAt} ->
                    {ok, Req, Env};
                {error, Code, Msg, _Li} ->
                    resp_json_dto:error(Req, Msg, Code)
            end;
        false ->
            {ok, Req, Env}
    end.
