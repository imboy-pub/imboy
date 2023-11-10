-module(imboy_req).

-export([get_int/3]).
-export([page_size/1]).
-export([post_params/1]).

-export([get/1, get/2]).
-export([post/2, post/3]).

-include_lib("imlib/include/log.hrl").

-define(ReqHeaders, [{"content-type", "application/json"}, {"client", "imboy-req"}]).


%% ===================================================================
%% API
%% ===================================================================

% {Page, Size} = imboy_req:page_size(Req0),
page_size(Req) ->
    #{page := Page} = cowboy_req:match_qs([{page, [], <<"1">>}], Req),
    #{size := Size} = cowboy_req:match_qs([{size, [], <<"10">>}], Req),
    {Page2, _} = string:to_integer(Page),
    {Size2, _} = string:to_integer(Size),
    pase_page_size(Page2, Size2).


get_int(Key, Req, Def) ->
    #{Key := Val} = cowboy_req:match_qs([{Key, [], Def}], Req),
    if
        Val == Def ->
            {ok, Def};
        true ->
            case string:to_integer(Val) of
                {error, _} ->
                    {ok, Def};
                {Val2, _} ->
                    {ok, Val2}
            end
    end.


% -spec post_params(Req::cowboy_req:req()) -> proplists().
% imboy_req:post_params(Req0),
% PostVals = imboy_req:post_params(Req0),
post_params(Req) ->
    ContentType = cowboy_req:parse_header(<<"content-type">>, Req),
    % ?LOG([ContentType]),
    % lager:info(io_lib:format("ContentType: ~p ContentType_End~n", [ContentType])),
    % ?LOG(Method = cowboy_req:method(Req)),
    case ContentType of
        % {<<"text">>,<<"plain">>, [{<<"charset">>,<<"utf-8">>}]} ->
        % {<<"text">>,<<"plain">>, _} ->
        %     [];
        {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
            {ok, Params, _Req} = cowboy_req:read_urlencoded_body(Req, #{length => 640000000, period => 50000}),
            % lager:info(io_lib:format("Params: ~p Params_End~n", [Params])),
            Params;
        {<<"application">>, <<"json">>, _} ->
            {ok, PostVals, _Req} = cowboy_req:read_body(Req),
            % ?LOG(PostVals),
            % Params = jsone:decode(PostVals, [{object_format, proplist}]),
            % ?LOG(Params),
            % Params
            jsone:decode(PostVals, [{object_format, proplist}]);
        _ ->
            lager:error(io_lib:format("imboy_req:post_params error: ContentType ~p; ~p ~n", [ContentType, Req])),
            []
    end.


get(Url) ->
    req(get, Url, #{}, ?ReqHeaders).


get(Url, Headers) ->
    req(get, Url, #{}, Headers).


post(Url, Params) ->
    req(post, Url, Params, ?ReqHeaders).


post(Url, Params, Headers) ->
    req(post, Url, Params, Headers).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% https://stackoverflow.com/questions/19103694/simple-example-using-erlang-for-https-post
% imboy_req:post("http://127.0.0.1:9800/test/req_post", #{type => 1, b => 2}).
% imboy_req:post("http://127.0.0.1:9800/test/req_post", [1,2,3]).
% imboy_req:get("http://127.0.0.1:9800/test/req_get").
-spec req(atom(), list() | binary(), list() | map(), list()) -> {ok, map()} | {error, any()}.
req(Method, Url, Params, Headers) ->
    application:ensure_started(ssl),
    application:ensure_started(inets),
    % 检查 content-type
    ContentType = proplists:get_value("content-type", Headers, "application/json"),
    Request =
        case Method of
            post ->
                Bin = jsone:encode(Params, [native_utf8]),
                {Url, Headers, ContentType, Bin};
            get ->
                {Url, Headers};
            _ ->
                {Url, Headers}
        end,
    Response = httpc:request(Method, Request, [], []),
    % ?LOG([response, Response]),
    case Response of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, jsone:decode(list_to_binary(Body))};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, StatusCode};
        {error, Reason} ->
            {error, Reason}
    end.


pase_page_size(error, error) ->
    pase_page_size(1, 10);
pase_page_size(error, Size) ->
    pase_page_size(1, Size);
pase_page_size(Page, error) ->
    pase_page_size(Page, 10);
pase_page_size(Page, Size) when Page < 1 ->
    pase_page_size(1, Size);
pase_page_size(Page, Size) when Size < 1 ->
    pase_page_size(Page, 10);
pase_page_size(Page, Size) when Size > 1000 ->
    pase_page_size(Page, 1000);
pase_page_size(Page, Size) ->
    {Page, Size}.