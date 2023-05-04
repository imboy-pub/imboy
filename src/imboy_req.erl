-module(imboy_req).

-include_lib("imboy/include/log.hrl").


-export([post_params/1]).
-export([page_size/1]).

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

% -spec post_params(Req::cowboy_req:req()) -> proplists().
% imboy_req:post_params(Req0),
% PostVals = imboy_req:post_params(Req0),
post_params(Req) ->
    % ?LOG(cowboy_req:parse_header(<<"content-type">>, Req)),
    % ?LOG(Method = cowboy_req:method(Req)),
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        % {<<"text">>,<<"plain">>, [{<<"charset">>,<<"utf-8">>}]} ->
        % {<<"text">>,<<"plain">>, _} ->
        %     [];
        {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
            {ok, Params, _Req} = cowboy_req:read_urlencoded_body(Req),
            Params;
        {<<"application">>, <<"json">>, _} ->
            {ok, PostVals, _Req} = cowboy_req:read_body(Req),
            % ?LOG(PostVals),
            % Params = jsone:decode(PostVals, [{object_format, proplist}]),
            % ?LOG(Params),
            % Params
            jsone:decode(PostVals, [{object_format, proplist}])
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

pase_page_size(error,  error) ->
    pase_page_size(1,  10);
pase_page_size(error,  Size) ->
    pase_page_size(1,  Size);
pase_page_size(Page,  error) ->
    pase_page_size(Page,  10);
pase_page_size(Page,  Size) when Page < 1 ->
    pase_page_size(1,  Size);
pase_page_size(Page,  Size) when Size < 1 ->
    pase_page_size(Page, 10);
pase_page_size(Page,  Size) when Size > 1000 ->
    pase_page_size(Page, 1000);
pase_page_size(Page, Size) ->
    {Page, Size}.
