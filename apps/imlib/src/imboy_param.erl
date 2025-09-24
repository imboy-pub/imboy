-module(imboy_param).

-export([page/1]).
-export([int/3]).
-export([get/3]).
-export([post/1, post/3]).
-export([param/3]).

-include_lib("imlib/include/log.hrl").


%% ===================================================================
%% API
%% ===================================================================

% {Page, Size} = imboy_param:page(Req0),
page(Req) ->
    Page = int(page, Req, 1),
    Size = int(size, Req, 20),
    pase_page_size(Page, Size).

% {ok, Ajax} = imboy_param:int(ajax, Req0, -2)
int(Key, Req, Def) ->
    % ?DEBUG_LOG([get_int, Key, Val, Def, Val == Def]),
    Method = cowboy_req:method(Req),
    if
        Method == <<"POST">> ->
            PostVals = post(Req),
            % ?DEBUG_LOG([get_int, PostVals, proplists:get_value(ec_cnv:to_binary(Key), PostVals, Def)]),
            {ok, proplists:get_value(ec_cnv:to_binary(Key), PostVals, Def)};
        true ->
            #{Key := Val} = cowboy_req:match_qs([{Key, [], Def}], Req),
            case string:to_integer(Val) of
                {error, _} ->
                    {ok, Def};
                {Val2, _} ->
                    {ok, Val2}
            end
    end.


% -spec post(Req::cowboy_req:req()) -> proplists().
% imboy_param:post(Req0),
% PostVals = imboy_param:post(Req0),
post(Req) ->
    ContentType = cowboy_req:parse_header(<<"content-type">>, Req),
    % ?DEBUG_LOG([ContentType]),
    % imboy_log:info(io_lib:format("ContentType: ~p ContentType_End~n", [ContentType])),
    % ?DEBUG_LOG(Method = cowboy_req:method(Req)),
    case ContentType of
        % {<<"text">>,<<"plain">>, [{<<"charset">>,<<"utf-8">>}]} ->
        % {<<"text">>,<<"plain">>, _} ->
        %     [];
        {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
            {ok, Params, _Req} = cowboy_req:read_urlencoded_body(Req, #{length => 640000000, period => 50000}),
            % imboy_log:info(io_lib:format("Params: ~p Params_End~n", [Params])),
            Params;
        {<<"application">>, <<"json">>, _} ->
            {ok, PostVals, _Req} = cowboy_req:read_body(Req),
            % ?DEBUG_LOG(PostVals),
            % Params = jsone:decode(PostVals, [{object_format, proplist}]),
            % ?DEBUG_LOG(Params),
            % Params
            jsone:decode(PostVals, [{object_format, proplist}]);
        _ ->
            imboy_log:error(io_lib:format("imboy_req:post error: ContentType ~p; ~p ~n", [ContentType, Req])),
            []
    end.


get(Key, Req, Default) ->
    param(Key, Req, Default).
post(Key, Req, Default) ->
    param(Key, Req, Default).
%% @doc 获取查询参数
%% @param Key 参数名
%% @param Req cowboy请求对象
%% @param Default 默认值
param(Key, Req, Default) ->
    case cowboy_req:match_qs([{Key, [], Default}], Req) of
        #{Key := Val} when Val =/= Default ->
            Val;
        _ ->
            % 如果是默认值，检查POST参数
            Method = cowboy_req:method(Req),
            if
                Method == <<"POST">> ->
                    PostVals = post(Req),
                    proplists:get_value(Key, PostVals, Default);
                true ->
                    Default
            end
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

pase_page_size(error, error) ->
    pase_page_size(1, 20);
pase_page_size(error, Size) ->
    pase_page_size(1, Size);
pase_page_size(Page, error) ->
    pase_page_size(Page, 20);
pase_page_size(Page, Size) when Page < 1 ->
    pase_page_size(1, Size);
pase_page_size(Page, Size) when Size < 1 ->
    pase_page_size(Page, 20);
pase_page_size(Page, Size) when Size > 1000 ->
    pase_page_size(Page, 1000);
pase_page_size(Page, Size) ->
    {Page, Size}.
