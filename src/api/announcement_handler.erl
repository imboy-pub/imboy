-module(announcement_handler).

%%%
% 用户端全局公告处理器
% User-facing global announcement handler
%%%

-behavior(cowboy_handler).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        list   -> list(Req0, State);
        detail -> detail(Req0, State)
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 获取已发布公告列表（分页）
%% 返回 status=1 且未过期的全局公告，按置顶优先、ID 倒序排列
%%
%% @param Req0 Cowboy 请求对象，支持 query 参数 page / page_size
%% @param _State 状态映射
%% @return 分页公告列表
%% @end
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    {ok, Payload} = announcement_ds:list_active(Page, Size),
    elib_response:success(Req0, Payload).

%% @doc 获取单条公告详情
%% 返回指定 ID 的已发布且未过期公告
%%
%% @param Req0 Cowboy 请求对象，query 参数 id（TSID integer）
%% @param _State 状态映射
%% @return 公告详情或 404
%% @end
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    IdBin = proplists:get_value(<<"id">>, Qs, undefined),
    case parse_id(IdBin) of
        {error, missing} ->
            elib_response:error(Req0, <<"缺少 id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid} ->
            elib_response:error(Req0, <<"id 参数格式错误"/utf8>>, ?ERR_BAD_REQUEST);
        {ok, Id} ->
            Row = announcement_ds:find_published(Id),
            case map_size(Row) of
                0 ->
                    elib_response:error(Req0, <<"公告不存在或已过期"/utf8>>, ?ERR_NOT_FOUND);
                _ ->
                    elib_response:success(Req0, Row)
            end
    end.

%% @doc 解析并校验 id 参数
%% @end
-spec parse_id(binary() | undefined) -> {ok, pos_integer()} | {error, missing | invalid}.
parse_id(undefined) ->
    {error, missing};
parse_id(<<>>) ->
    {error, missing};
parse_id(IdBin) when is_binary(IdBin) ->
    try ec_cnv:to_integer(IdBin) of
        Id when is_integer(Id), Id > 0 -> {ok, Id};
        _ -> {error, invalid}
    catch
        _:_ -> {error, invalid}
    end.
