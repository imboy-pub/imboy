-module(imboy_param).


-export([page/1]).

-export([int/3]).

-export([get/3]).

-export([post/1, post/3]).

-export([param/3]).


-include("log.hrl").



%% ===================================================================
%% API
%% ===================================================================

%% @doc 从请求中提取分页参数
%% @param Req cowboy请求对象
%% @returns {Page, Size} 元组，Page为页码，Size为每页大小
%% 示例: {Page, Size} = imboy_param:page(Req0)
-spec page(cowboy_req:req()) -> {non_neg_integer(), pos_integer()}.

page(Req) ->
    {ok, Page} = int(page, Req, 1),
    {ok, Size} = int(size, Req, 20),
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
            KeyBin = ec_cnv:to_binary(Key),
            case maps:get(KeyBin, PostVals, undefined) of
                undefined ->
                    {ok, Def};
                Val when is_integer(Val) ->
                    {ok, Val};
                Val ->
                    case string:to_integer(ec_cnv:to_list(Val)) of
                        {error, _} ->
                            {ok, Def};
                        {Val2, _} ->
                            {ok, Val2}
                    end
            end;
        true ->
            KeyBin = ec_cnv:to_binary(Key),
            Val = maps:get(KeyBin, qs_map(Req), Def),
            case Val of
                V when is_integer(V) ->
                    {ok, V};
                V ->
                    case string:to_integer(ec_cnv:to_list(V)) of
                        {error, _} ->
                            {ok, Def};
                        {Val2, _} ->
                            {ok, Val2}
                    end
            end
    end.


%% @doc 从请求体中解析POST参数
%% 支持 application/x-www-form-urlencoded 和 application/json 格式
%% @param Req cowboy请求对象
%% @returns 参数列表（map格式）
%% 示例: PostVals = imboy_param:post(Req0)
-spec post(cowboy_req:req()) -> map().

post(Req) ->
    case erlang:get({?MODULE, post_vals}) of
        PostValsCached when is_map(PostValsCached) ->
            PostValsCached;
        _ ->
            ContentType = cowboy_req:parse_header(<<"content-type">>, Req),
            PostVals =
                case ContentType of
                    {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
                        {ok, Params, _Req} =
                            cowboy_req:read_urlencoded_body(Req, #{length => 640000000, period => 50000}),
                            urlencoded_to_map(Params);
                    {<<"application">>, <<"json">>, _} ->
                        {ok, Body, _Req} = cowboy_req:read_body(Req),
                        try
                            case jsone:decode(Body, [{object_format, map}]) of
                                Map when is_map(Map) -> Map;
                                _ -> #{}
                            end
                        catch
                            _:_ ->
                                #{}
                        end;
                    _ ->
                        ok = imboy_log:error(io_lib:format(
                            "imboy_param:post error: ContentType ~p; ~p ~n",
                            [ContentType, Req]
                        )),
                        #{}
                end,
            erlang:put({?MODULE, post_vals}, PostVals),
            PostVals
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
    KeyBin = ec_cnv:to_binary(Key),
    case maps:get(KeyBin, qs_map(Req), Default) of
        Val when Val =/= Default ->
            Val;
        _ ->
            Method = cowboy_req:method(Req),
            if
                Method == <<"POST">> ->
                    PostVals = post(Req),
                    maps:get(KeyBin, PostVals, Default);
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
-spec pase_page_size(number() | binary(), number() | binary()) -> {non_neg_integer(), pos_integer()}.

pase_page_size(Page, Size) when Page < 1; Page =:= error; Page =:= undefined ->
    pase_page_size(1, Size);
pase_page_size(Page, Size) when Size < 1; Size =:= error; Size =:= undefined ->
    pase_page_size(Page, 20);
pase_page_size(Page, Size) when Size > 1000 ->
    pase_page_size(Page, 1000);
pase_page_size(Page, Size) ->
    {ec_cnv:to_integer(Page), ec_cnv:to_integer(Size)}.

urlencoded_to_map(Params) when is_list(Params) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            KeyBin = ec_cnv:to_binary(Key),
            ValBin = ec_cnv:to_binary(Value),
            case maps:get(KeyBin, Acc, undefined) of
                undefined ->
                    maps:put(KeyBin, ValBin, Acc);
                Existing when is_list(Existing) ->
                    maps:put(KeyBin, Existing ++ [ValBin], Acc);
                Existing ->
                    maps:put(KeyBin, [Existing, ValBin], Acc)
            end
        end,
        #{},
        Params
    ).

qs_map(Req) ->
    case erlang:get({?MODULE, qs_vals}) of
        QsValsCached when is_map(QsValsCached) ->
            QsValsCached;
        _ ->
            QsVals = urlencoded_to_map(cowboy_req:parse_qs(Req)),
            erlang:put({?MODULE, qs_vals}, QsVals),
            QsVals
    end.
