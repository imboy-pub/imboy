-module(auth_handler).
-behavior(cowboy_rest).

-include("log.hrl").

-export([init/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化认证处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 参数
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
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

%% @doc Assets服务认证
%% 用于go-fastdfs的自定义认证
%%
%% @param Method HTTP方法（POST或GET）
%% @param Req0 Cowboy请求对象，包含认证参数
%% @return 返回认证结果（ok或fail）
%% @end
-spec assets(binary(), cowboy_req:req()) -> cowboy_req:req().
assets(<<"POST">>, Req0) ->
    try
        PostVals = elib_param:post(Req0),
        Scene = maps:get(<<"s">>, PostVals, undefined),
        % AuthToken
        AuthTk = maps:get(<<"a">>, PostVals, undefined),
        Val = maps:get(<<"v">>, PostVals, undefined),
        Path = maps:get(<<"__path__">>, PostVals, undefined),
        {Scene, Path}
    of
        {<<"open">>, Path2} ->
            % ?DEBUG_LOG([Path2, AuthTk, Val]),
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
    Body = <<"fail">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, unicode:characters_to_binary(Body, utf8), Req0).
