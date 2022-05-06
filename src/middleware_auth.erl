-module(middleware_auth).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include("common.hrl").


%% 这个是回调函数
execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    NotNeedAuth = route_helper:not_need_auth_paths(),
    Need = lists:member(Path, NotNeedAuth),
    % ?LOG(["Path", Path, "Need", Need]),
    case Need of
        true ->
            {ok, Req, Env};
        false ->
            Authorization = cowboy_req:header(<<"authorization">>, Req),
            % ?LOG(['Authorization', Authorization]),
            case ds_token:decrypt_token(Authorization) of
                {ok, Id, _ExpireAt, <<"tk">>} when is_integer(Id) ->
                    #{handler_opts := HandlerOpts} = Env,
                    Env2 = Env#{handler_opts => [{current_uid, Id} |
                                                 HandlerOpts]},
                    {ok, Req, Env2};
                {ok, Id, _ExpireAt, <<"tk">>} when is_binary(Id) ->
                    #{handler_opts := HandlerOpts} = Env,
                    Env2 = Env#{handler_opts =>
                                    [{current_uid, binary_to_integer(Id)} |
                                     HandlerOpts]},
                    {ok, Req, Env2};
                {ok, _Id, _ExpireAt, <<"rtk">>} ->
                    Req1 = dto_resp_json:error(Req,
                                               "Does not support refreshtoken",
                                               1),
                    {stop, Req1};
                {error, Code, Msg, _Li} ->
                    Req1 = dto_resp_json:error(Req, Msg, Code),
                    {stop, Req1}
            end
    end.
