-module(auth_middleware).
-behaviour(cowboy_middleware).

-include_lib("imboy/include/log.hrl").

-export([execute/2]).

%% 这个是回调函数
execute(Req, Env) ->
    Path = remove_last_forward_slash(cowboy_req:path(Req)),
    case Path of
        <<"/static/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/webrtc/", _Tail/binary>> ->
            {ok, Req, Env};
        _ ->
            OpenLi = imboy_router:open(),
            OptionLi = imboy_router:option(),
            InOpenLi = lists:member(Path, OpenLi),
            InOptionLi = lists:member(Path, OptionLi),

            Authorization = cowboy_req:header(<<"authorization">>, Req),
            % ?LOG(["Path", Path, "InOpenLi", InOpenLi, "InOptionLi", InOptionLi,
            %     "Authorization", Authorization, Authorization == undefined]),
            condition(InOptionLi, InOpenLi, Authorization, Req, Env)
    end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
condition(true, _, undefined, Req, Env) ->
    {ok, Req, Env};
condition(true, _, Authorization, Req, Env) ->
    do_authorization(Authorization, Req, Env);
condition(_, true, _, Req, Env) ->
    {ok, Req, Env};
condition(_, _, Authorization, Req, Env) ->
    do_authorization(Authorization, Req, Env).

do_authorization(undefined, Req, _Env) ->
    {stop, Req};
do_authorization(Authorization, Req, Env) ->
    % ?LOG(['Authorization', Authorization]),
    case token_ds:decrypt_token(Authorization) of
        {ok, Id, _ExpireAt, <<"tk">>} when is_integer(Id) ->
            #{handler_opts := HandlerOpts} = Env,
            Env2 = Env#{handler_opts := HandlerOpts#{current_uid => Id}},
            {ok, Req, Env2};
        {ok, Id, _ExpireAt, <<"tk">>} when is_binary(Id) ->
            #{handler_opts := HandlerOpts} = Env,
            Env2 = Env#{handler_opts := HandlerOpts#{current_uid => Id}},
            {ok, Req, Env2};
        {ok, _Id, _ExpireAt, <<"rtk">>} ->
            Err = "Does not support refreshtoken",
            Req1 = imboy_response:error(Req, Err, 1),
            {stop, Req1};
        {error, Code, Msg, _Li} ->
            Req1 = imboy_response:error(Req, Msg, Code),
            {stop, Req1}
    end.

%% Remove the last forward slash
%% 删除最后一个正斜杠
%% auth_middleware:remove_last_forward_slash(<<"/abc/">>).
%%  will be echo <<"/abc">>
-spec remove_last_forward_slash(binary()) -> binary().
remove_last_forward_slash(<<"">>) ->
    <<"/">>;
remove_last_forward_slash(<<"/">>) ->
    <<"/">>;
remove_last_forward_slash(Path) ->
    case binary:last(Path) of
        47 ->
            binary:part(Path, 0, byte_size(Path)-1);
        _ ->
            Path
    end.

