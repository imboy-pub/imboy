-module(auth_handler).
-behavior(cowboy_rest).

-include_lib("imlib/include/log.hrl").

-export([init/2]).


%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            assets ->
                assets(Method, Req0);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%%% for https://sjqzhang.github.io/go-fastdfs/authentication.html#custom
% Assets服务认证
assets(<<"POST">>, Req0) ->
    try
        PostVals = imboy_req:post_params(Req0),
        Scene = proplists:get_value(<<"s">>, PostVals),
        % AuthToken
        AuthTk = proplists:get_value(<<"a">>, PostVals),
        Val = proplists:get_value(<<"v">>, PostVals),
        Path = proplists:get_value(<<"__path__">>, PostVals),
        {Scene, Path}
    of
        {<<"open">>, Path2} ->
            Body = auth_logic:verify_for_open(Path2, AuthTk, Val),
            cowboy_req:reply(200,
                             #{<<"content-type">> => <<"text/html">>},
                             unicode:characters_to_binary(Body, utf8),
                             Req0);
        {_Scene, _Path} ->
            {V, _} = string:to_integer(Val),
            % Body is <<"ok">> or <<"fail">>
            Body = auth_logic:verify_for_assets(Scene, AuthTk, V, Path),
            cowboy_req:reply(200,
                             #{<<"content-type">> => <<"text/html">>},
                             unicode:characters_to_binary(Body, utf8),
                             Req0)
    catch
        _:_ ->
            cowboy_req:reply(200,
                             #{<<"content-type">> => <<"text/html">>},
                             unicode:characters_to_binary(<<"fail">>, utf8),
                             Req0)
    end;
assets(<<"GET">>, Req0) ->
    % Body is <<"fail">>
    Body = auth_logic:verify_for_assets(undefined, undefined, undefined),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, unicode:characters_to_binary(Body, utf8), Req0).
