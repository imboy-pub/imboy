-module(imboy_req).

-include_lib("imboy/include/log.hrl").


-export([post_params/1]).

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
