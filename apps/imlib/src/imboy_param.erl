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

%% @doc 从请求中提取分页参数
%% @param Req cowboy请求对象
%% @returns {Page, Size} 元组，Page为页码，Size为每页大小
%% 示例: {Page, Size} = imboy_param:page(Req0)
-spec page(cowboy_req:req()) -> {non_neg_integer(), pos_integer()}.
page(Req) ->
    Page = int(page, Req, 1),
    Size = int(size, Req, 20),
    pase_page_size(Page, Size).

%% @doc 从请求中获取整数参数
%% 优先从POST参数获取，如果没有则从GET参数获取
%% @param Key 参数名
%% @param Req cowboy请求对象
%% @param Def 默认值
%% @returns {ok, Value} 整数值
%% 示例: {ok, Ajax} = imboy_param:int(ajax, Req0, -2)
-spec int(atom() | binary() | list(), cowboy_req:req(), integer()) -> {ok, integer()}.
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


%% @doc 从请求体中解析POST参数
%% 支持 application/x-www-form-urlencoded 和 application/json 格式
%% @param Req cowboy请求对象
%% @returns 参数列表（property list格式）
%% 示例: PostVals = imboy_param:post(Req0)
-spec post(cowboy_req:req()) -> proplists:proplist().
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


%% @doc 获取GET参数
%% @param Key 参数名
%% @param Req cowboy请求对象
%% @param Default 默认值
%% @returns 参数值或默认值
-spec get(atom() | binary() | list(), cowboy_req:req(), any()) -> any().
get(Key, Req, Default) ->
    param(Key, Req, Default).

%% @doc 获取POST参数
%% @param Key 参数名
%% @param Req cowboy请求对象
%% @param Default 默认值
%% @returns 参数值或默认值
-spec post(atom() | binary() | list(), cowboy_req:req(), any()) -> any().
post(Key, Req, Default) ->
    param(Key, Req, Default).

%% @doc 获取请求参数（支持GET和POST）
%% 优先获取GET参数，如果是默认值则检查POST参数
%% @param Key 参数名
%% @param Req cowboy请求对象
%% @param Default 默认值
%% @returns 参数值或默认值
-spec param(atom() | binary() | list(), cowboy_req:req(), any()) -> any().
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

%% @doc 解析和规范化分页参数
%% @param Page 页码
%% @param Size 每页大小
%% @returns {Page, Size} 规范化后的分页参数
-spec pase_page_size(any(), any()) -> {non_neg_integer(), pos_integer()}.
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
