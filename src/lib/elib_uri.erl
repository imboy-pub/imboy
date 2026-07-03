-module(elib_uri).

%%% @doc URI 和文件处理模块
%%% 提供 URL 构建、参数解析、文件上传下载等功能

-export([build_query/3]).
-export([exclusion_param/2]).
-export([get_params/1, get_params/2, get_params/3]).
-export([query_pairs_to_map/1]).

-export([download/2]).
-export([upload/5]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%

%% @doc 构建带查询参数的完整 URL
%% @param Base 基础 URL
%% @param Path 路径
%% @param Args 查询参数映射
%% @returns 完整的 URL（如 <<"https://example.com/api/path?a=1&b=2">>）
-spec build_query(binary(), binary(), map()) -> binary().
build_query(Base, Path, Args) ->
    Base1 = ec_cnv:to_binary(Base),
    CheckBase = elib_str:endswith(<<"/">>, Base1),
    Base2 =
        if
            CheckBase ->
                binary:part(Base1, 0, byte_size(Base1) - 1);
            true ->
                Base1
        end,
    CheckPath = elib_str:startswith(<<"/">>, Path),
    Path2 =
        if
            CheckPath ->
                Path;
            true ->
                <<"/", Path/binary>>
        end,
    Args2 = elib_cnv:map_to_query(Args),
    case Args2 of
        <<>> -> <<Base2/binary, Path2/binary>>;
        _ -> <<Base2/binary, Path2/binary, "?", Args2/binary>>
    end.

%% @doc 下载文件到本地
%% @param Url 文件 URL
%% @param FilePath 本地保存路径
%% @returns {ok, FilePath} | {error, Reason}
%% @example
%% elib_uri:download(<<"https://example.com/image.jpg">>, "./temp.jpg").
-spec download(binary(), binary()) -> {ok, binary()} | {error, term()}.
download(Url, FilePath) ->
    _ = application:ensure_started(ssl),
    _ = application:ensure_started(inets),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, File} = file:open(FilePath, [write, binary]),
            ok = file:write(File, Body),
            ok = file:close(File),
            {ok, FilePath};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, StatusCode};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 上传文件（multipart/form-data 格式）
%% @param URL 上传目标 URL
%% @param FilePath 本地文件路径
%% @param Name 表单字段名称
%% @param MimeType MIME 类型
%% @param RequestData 额外的表单数据
%% @returns {ok, Response} | {error, Reason}
%% @example
%% elib_uri:upload(
%%     <<"https://example.com/upload">>,
%%     <<"/path/to/file.png">>,
%%     <<"file">>,
%%     <<"image/png">>,
%%     []
%% ).
%% @see https://gist.github.com/leeyisoft/4cc8acd930910006b5251092e0013d07
-spec upload(binary(), binary(), binary(), binary(), list()) ->
    {ok, binary()} | {error, list()}.
upload(URL, FilePath, Name, MimeType, RequestData) ->
    _ = application:ensure_started(ssl),
    _ = application:ensure_started(inets),
    Filename = filename:basename(FilePath),
    {ok, Data} = file:read_file(FilePath),
    Boundary = elib_dt:microsecond(),
    RequestBody = format_multipart_formdata(
        Data, RequestData, Name, [Filename], MimeType, integer_to_binary(Boundary)
    ),
    % ?DEBUG_LOG(['RequestBody', RequestBody]),
    % RequestBody.
    ContentType = "multipart/form-data; boundary=" ++ integer_to_list(Boundary),

    ContentLength = integer_to_list(length(binary_to_list(RequestBody))),
    Headers = [{"Content-Length", ContentLength}],
    HTTPOptions = [],
    Options = [{body_format, binary}],
    Response = httpc:request(
        post, {binary_to_list(URL), Headers, ContentType, RequestBody}, HTTPOptions, Options
    ),
    % ?DEBUG_LOG([response, Response]),
    case Response of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, jsone:decode(Body, [{object_format, map}])};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, StatusCode};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 从 URL 中排除指定的查询参数
%% @param Url URL 字符串
%% @param Keys 要排除的参数名列表
%% @returns 排除指定参数后的 URL
%% @example
%% elib_uri:exclusion_param(
%%     <<"https://example.com/img.jpg?s=dev&a=123&v=531378&width=375">>,
%%     [<<"width">>, <<"v">>]
%% ).
%%   %% <<"https://example.com/img.jpg?s=dev&a=123">>
-spec exclusion_param(binary() | list(), list()) -> binary().
exclusion_param(Url, Keys) when is_list(Url) ->
    exclusion_param(list_to_binary(Url), Keys);
exclusion_param(Url, Keys) ->
    UrlMap = uri_string:parse(Url),
    case maps:find(query, UrlMap) of
        error ->
            Url;
        {ok, <<>>} ->
            Url;
        {ok, Query} ->
            Query2 = uri_string:dissect_query(Query),
            Query3 = [[K, "=", V, "&"] || {K, V} <- Query2, lists:member(K, Keys) == false],
            Query4 = iolist_to_binary(Query3),
            uri_string:normalize(UrlMap#{query => Query4})
    end.

% lists:droplast(uri_string:normalize(UrlMap#{query => Query4})).

% 获取URL中的所有参数
-spec get_params(list() | binary()) -> {map(), map()}.
% elib_uri:get_params("https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375").
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
    {UrlMap, query_pairs_to_map(Query2)}.

query_pairs_to_map(Pairs) when is_list(Pairs) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            Key = ec_cnv:to_binary(K),
            Val = ec_cnv:to_binary(V),
            case maps:get(Key, Acc, undefined) of
                undefined ->
                    maps:put(Key, Val, Acc);
                Existing when is_list(Existing) ->
                    maps:put(Key, Existing ++ [Val], Acc);
                Existing ->
                    maps:put(Key, [Existing, Val], Acc)
            end
        end,
        #{},
        Pairs
    );
query_pairs_to_map(_) ->
    #{}.

%% 根据指定参数名获取在URL中对应的值
-spec get_params(binary(), binary()) -> binary().
% elib_uri:get_params("width", "https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375").
% elib_uri:get_params(<<"width">>, <<"https://a.imboy.pub/img/20235/20_15/chk7ef90poqbagho7410.jpg?s=dev&a=344af61665efff23&v=531378&width=375">>).
get_params(Key, Url) ->
    get_params(Key, Url, <<"">>).

-spec get_params(binary(), binary(), binary()) -> binary().
get_params(Key, Url, Def) ->
    {_, Params} = get_params(Url),
    maps:get(Key, Params, Def).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% https://gist.github.com/leeyisoft/4cc8acd930910006b5251092e0013d07
-spec format_multipart_formdata(Data, Params, Name, FileNames, MimeType, Boundary) ->
    binary()
when
    Data :: binary(),
    Params :: list(),
    Name :: binary(),
    FileNames :: list(),
    MimeType :: binary(),
    Boundary :: binary().
format_multipart_formdata(Data, Params, Name, FileNames, MimeType, Boundary) ->
    StartBoundary = erlang:iolist_to_binary([<<"--">>, Boundary]),
    LineSeparator = <<"\r\n">>,
    % ?DEBUG_LOG(['Params', Params]),
    WithParams = lists:foldl(
        fun({Key, Value}, Acc) ->
            erlang:iolist_to_binary([
                Acc,
                StartBoundary,
                LineSeparator,
                <<"Content-Disposition: form-data; name=\"">>,
                Key,
                <<"\"">>,
                LineSeparator,
                LineSeparator,
                Value,
                LineSeparator
            ])
        end,
        <<"">>,
        Params
    ),
    WithPaths = lists:foldl(
        fun(FileName, Acc) ->
            erlang:iolist_to_binary([
                Acc,
                StartBoundary,
                LineSeparator,
                <<"Content-Disposition: form-data; name=\"">>,
                Name,
                <<"\"; filename=\"">>,
                FileName,
                <<"\"">>,
                LineSeparator,
                <<"Content-Type: application/">>,
                MimeType,
                LineSeparator,
                LineSeparator,
                % <<"Content-Type: application/octet-stream;">>, LineSeparator, LineSeparator,
                Data,
                LineSeparator
            ])
        end,
        WithParams,
        FileNames
    ),
    erlang:iolist_to_binary([WithPaths, StartBoundary, <<"--">>, LineSeparator]).
