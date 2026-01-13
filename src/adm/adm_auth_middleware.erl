-module(adm_auth_middleware).
-behaviour(cowboy_middleware).

-include("log.hrl").

-export([execute/2]).
-export([remove_last_forward_slash/1]).


%% 这个是回调函数
%% @doc 执行认证中间件
-spec execute(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    % elib_log:info([is_binary(cowboy_req:path(Req)), cowboy_req:path(Req)]),
    Path = remove_last_forward_slash(cowboy_req:path(Req)),
    % elib_log:info(Path),
    case Path of
        <<"/static/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/adm/passport/", _Tail/binary>> ->
            % elib_log:info("passport xxxxxxxxxxxxxx\n"),
            {ok, Req, Env};
        _ ->
            % {ok, Req, Env} | {stop, Req}
            Method = cowboy_req:method(Req),
            Uid = elib_req:cookie(<<"adm_user_id">>, Req),
            % elib_log:info([Method, Uid]),
            condition(Method, Uid, Req, Env)
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 条件判断处理
-spec condition(binary(), binary() | undefined, cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
condition(_, Uid, Req, Env) when is_binary(Uid) ->
    #{handler_opts := HandlerOpts} = Env,
    Env1 = maps:remove(has_sent_resp, Env),
    Env2 = Env1#{handler_opts := HandlerOpts#{adm_user_id => elib_hashids:decode(Uid)}},
    {ok, Req, Env2};
condition(<<"GET">>, _, Req, _State) ->
    Uri = cowboy_req:uri(Req),
    Req1 = cowboy_req:set_resp_cookie(<<"back_uri">>, Uri, Req, #{path => <<"/">>}),
    Location = "/adm/passport/login",
    Req2 = cowboy_req:reply(302
        , #{<<"Location">> => Location}
        , Req1),
    {stop, Req2};
condition(<<"POST">>, _, Req, _) ->
    Req1 = elib_response:error(
         Req,
         "Need to log in again",
         706),
    {stop, Req1}.
% condition(_, _, Req, _) ->
%     Req1 = elib_response:error(
%          Req,
%          "Need to log in again",
%          706),
%     {stop, Req1}.
% condition(_, _, Req, _) ->
%     {stop, Req}.
    % elib_log:info(['Req1Req1Req1Req1', Req]),
    % {suspend, elib_response, error, [
    %      Req,
    %      "Need to log in again",
    %      706]}.

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
            binary:part(Path, 0, byte_size(Path) - 1);
        _ ->
            Path
    end.
