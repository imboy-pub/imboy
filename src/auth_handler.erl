-module(auth_handler).
-behavior(cowboy_rest).

-include_lib("imboy/include/log.hrl").

-export([init/2]).

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        assets ->
            assets(Req0);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%% for https://sjqzhang.github.io/go-fastdfs/authentication.html#custom
assets(Req0) ->
    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
    % ?LOG(PostVals),
    % AuthToken
    AuthTk = proplists:get_value(<<"a">>, PostVals),
    % Sence
    Scene = proplists:get_value(<<"s">>, PostVals),
    % Val = md5(filepath+filename)
    Val = proplists:get_value(<<"v">>, PostVals),

    % Body is ok or fail
    Body = auth_for_assets(Scene, AuthTk, Val),
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html">>},
                     unicode:characters_to_binary(Body, utf8),
                     Req0).


auth_for_assets(undefined, _AuthTk, _Name) ->
    <<"fail">>;
auth_for_assets(_Scene, undefined, _Name) ->
    <<"fail">>;
auth_for_assets(_Scene, _AuthTk, undefined) ->
    <<"fail">>;
auth_for_assets(Scene, AuthTk, Val) ->
    {ok, AuthKeys} = application:get_env(imboy, auth_keys),
    Key = proplists:get_value(Scene, AuthKeys),
    Str = Key ++ binary_to_list(Val),
    case binary:part(imboy_hasher:md5(Str), {8, 16}) == AuthTk of
        true ->
            <<"ok">>;
        _ ->
            <<"fail">>
    end.
