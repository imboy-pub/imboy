-module(auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include("imboy.hrl").

%% 这个是回调函数
execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    NeedAuth = route_helper:need_auth_paths(),
    Need = lists:member(Path, NeedAuth),
    case Need of
        true ->
            Token = cowboy_req:header(<<"imboy-token">>, Req),
            case token_ds:decrypt_token(Token) of
                {ok, Id, _ExpireAt, <<"tk">>} ->
                    Uid = list_to_integer(binary_to_list(Id)),
                    #{handler_opts := HandlerOpts} = Env,
                    Env2 = Env#{handler_opts => [{current_uid, Uid}|HandlerOpts]},
                    {ok, Req, Env2};
                {ok, _Id, _ExpireAt, <<"rtk">>} ->
                    Req1 = resp_json_dto:error(Req, "Does not support refreshtoken", 1),
                    {stop, Req1};
                {error, Code, Msg, _Li} ->
                    Req1 = resp_json_dto:error(Req, Msg, Code),
                    {stop, Req1}
            end;
        false ->
            {ok, Req, Env}
    end.
