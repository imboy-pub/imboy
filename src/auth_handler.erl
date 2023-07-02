-module(auth_handler).
-behavior(cowboy_rest).

-include_lib("imboy/include/log.hrl").

-export([init/2]).

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        assets ->
            assets(Method, Req0);
        get_token ->
            get_token(Method, Req0);
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
            % Body is <<"ok">> or <<"fail">>
            Body = auth_logic:verify_for_assets(Scene, AuthTk, Val, Path),
            cowboy_req:reply(200,
                 #{<<"content-type">> => <<"text/html">>},
                 unicode:characters_to_binary(Body, utf8),
                 Req0)
    catch
        _ ->
            cowboy_req:reply(200,
                 #{<<"content-type">> => <<"text/html">>},
                 unicode:characters_to_binary(<<"fail">>, utf8),
                 Req0)
    end;
assets(<<"GET">>, Req0) ->
    % Body is <<"fail">>
    Body = auth_logic:verify_for_assets(undefined, undefined, undefined),
    cowboy_req:reply(200,
         #{<<"content-type">> => <<"text/html">>},
         unicode:characters_to_binary(Body, utf8),
         Req0).

% Assets服务认证 获取token
get_token(<<"GET">>, Req0) ->
    imboy_response:success(Req0, #{
    }, "success.");
get_token(<<"POST">>, Req0) ->
    PostVals = imboy_req:post_params(Req0),
    S = proplists:get_value(<<"s">>, PostVals, <<"dev">>),
    V = imboy_dt:second(),
    A = auth_ds:get_token(assets, S, integer_to_list(V)),
    Key = imboy_func:env(solidified_key),
    Bin = imboy_cipher:aes_encrypt(jsone:encode(#{
        a => A,
        s => S,
        v => V
    }), Key),
    imboy_response:success(Req0, #{
        res => Bin
    }, "success.").
