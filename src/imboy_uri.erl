-module(imboy_uri).

-export([exclusion_param/2]).
-export([get_params/1, get_params/2, get_params/3]).

-export([download/2]).
-export([upload/5]).
-export([check_auth/1]).

-include_lib("imboy/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

% imboy_uri:download("https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375", "./temp_temp.png").
download(Url, FilePath) ->
    application:ensure_started(ssl),
    application:ensure_started(inets),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, File} = file:open(FilePath, [write, binary]),
            file:write(File, Body),
            file:close(File),
            {ok, FilePath};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, StatusCode};
        {error, Reason} ->
            {error, Reason}
    end.

% https://gist.github.com/leeyisoft/4cc8acd930910006b5251092e0013d07

%% Usage:
%% upload(<<"site.com/api/upload">>, <<"path/to/file.png">>, <<"upload">>, <<"image/png">>, [], <<"some-token">>)
%%
%% Usage with RequestData:
%% Payload = [{upload_type, <<"user_picture">>}],
%% PayloadContent = jsx:encode(Payload),
%% RequestData = [
%%     {<<"payload">>, PayloadContent}
%% ]
%% upload(<<"site.com/api/upload">>, <<"path/to/file.png">>, <<"upload">>, <<"image/png">>, RequestData)
-spec upload(URL, FilePath, Name, MimeType, RequestData) -> {ok, binary()} | {error, list()} when
    URL:: binary(),
    FilePath:: binary(),
    Name:: binary(),
    MimeType:: binary(),
    RequestData:: list().
upload(URL, FilePath, Name, MimeType, RequestData) ->
    application:ensure_started(ssl),
    application:ensure_started(inets),
    Filename = filename:basename(FilePath),
    {ok, Data} = file:read_file(FilePath),
    Boundary = imboy_dt:microsecond(),
    RequestBody = format_multipart_formdata(Data, RequestData, Name, [Filename], MimeType, integer_to_binary(Boundary)),
    % ?LOG(['RequestBody', RequestBody]),
    % RequestBody.
    ContentType = "multipart/form-data; boundary=" ++ integer_to_list(Boundary),

    ContentLength = integer_to_list(length(binary_to_list(RequestBody))),
    Headers = [
        {"Content-Length", ContentLength}
    ],
    HTTPOptions = [],
    Options = [{body_format, binary}],
    Response = httpc:request(post
        , {binary_to_list(URL), Headers, ContentType, RequestBody}
        , HTTPOptions
        , Options
    ),
    % ?LOG([response, Response]),
    case Response of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, jsx:decode(Body)};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, StatusCode};
        {error, Reason} ->
            {error, Reason}
    end.


% imboy_uri:exclusion_param("https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375", ["width", "v"]).
% imboy_uri:exclusion_param("https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375", ["width"]).
exclusion_param(Url, Keys) when is_list(Url) ->
    exclusion_param(list_to_binary(Url), Keys);
exclusion_param(Url, Keys) ->
    UrlMap = uri_string:parse(Url),
    Query = maps:get(query, UrlMap),
    Query2 = uri_string:dissect_query(Query),
    Query3 = [[K, "=", V, "&"] || {K, V} <- Query2, lists:member(K, Keys) == false],
    Query4 = iolist_to_binary(Query3),
    uri_string:normalize(UrlMap#{query => Query4}).
    % lists:droplast(uri_string:normalize(UrlMap#{query => Query4})).

% 获取URL中的所有参数
-spec get_params(list() | binary()) -> map().
% imboy_uri:get_params("https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375").
% {#{host => "a.imboy.pub",
%    path => "/img/20235/20_15/chk7ef90poqbagho7410.jpg",
%    query => "s=dev&a=344af61665efff23&v=531378&width=375",
%    scheme => "https"},
%  #{"a" => "344af61665efff23","s" => "dev","v" => "531378",
%    "width" => "375"}}
get_params(Url) ->
    UrlMap = uri_string:parse(Url),
    Query = maps:get(query, UrlMap, ""),
    Query2 = uri_string:dissect_query(Query),
    {UrlMap, maps:from_list(Query2)}.

%% 根据指定参数名获取在URL中对应的值
-spec get_params(atom(), list()) -> binary().
% imboy_uri:get_params("width", "https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375").
get_params(Key, Url) ->
    get_params(Key, Url, <<"">>).

get_params(Key, Url, Def) ->
    Params = get_params(Url),
    maps:get(Key, Params, Def).

-spec check_auth(list() | binary()) -> map().
% imboy_uri:check_auth("https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375").
check_auth(Url) when is_list(Url) ->
    check_auth(list_to_binary(Url));
check_auth(Url) ->
    % Url = <<"https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375">>,

    {UrlMap, QMap} = get_params(Url),

    % % <<"s=dev&a=344af61665efff23&v=531378&width=375">>
    S = maps:get(<<"s">>, QMap, <<"dev">>),
    V = imboy_dt:second(),
    A = auth_ds:get_token(assets, S, integer_to_list(V)),
    V2 = integer_to_binary(V),
    NewQuery = <<"s=", S/binary
        ,"&a=", A/binary,
        "&v=", V2/binary>>,
    % lager:info(Query2),
    uri_string:normalize(UrlMap#{query => NewQuery}).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% https://gist.github.com/leeyisoft/4cc8acd930910006b5251092e0013d07
-spec format_multipart_formdata(Data, Params, Name, FileNames, MimeType, Boundary) -> binary() when
    Data:: binary(),
    Params:: list(),
    Name:: binary(),
    FileNames:: list(),
    MimeType:: binary(),
    Boundary:: binary().
format_multipart_formdata(Data, Params, Name, FileNames, MimeType, Boundary) ->
    StartBoundary = erlang:iolist_to_binary([<<"--">>, Boundary]),
    LineSeparator = <<"\r\n">>,
    % ?LOG(['Params', Params]),
    WithParams = lists:foldl(fun({Key, Value}, Acc) ->
        erlang:iolist_to_binary([
            Acc,
            StartBoundary, LineSeparator,
            <<"Content-Disposition: form-data; name=\"">>, Key, <<"\"">>, LineSeparator, LineSeparator,
            Value, LineSeparator
        ])
    end, <<"">>, Params),
    WithPaths = lists:foldl(fun(FileName, Acc) ->
        erlang:iolist_to_binary([
            Acc,
            StartBoundary, LineSeparator,
            <<"Content-Disposition: form-data; name=\"">>, Name,
            <<"\"; filename=\"">>, FileName, <<"\"">>, LineSeparator,
            <<"Content-Type: application/">>, MimeType, LineSeparator, LineSeparator,
            % <<"Content-Type: application/octet-stream;">>, LineSeparator, LineSeparator,
            Data,
            LineSeparator
        ])
    end, WithParams, FileNames),
    erlang:iolist_to_binary([WithPaths, StartBoundary, <<"--">>, LineSeparator]).
