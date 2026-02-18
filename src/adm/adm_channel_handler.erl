-module(adm_channel_handler).

%%%
% adm_channel 控制器模块
% 频道管理后台 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 REST 处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0);
            detail -> detail(Method, Req0);
            search -> search(Method, Req0);
            delete -> delete_action(Method, Req0);
            false -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 获取频道列表
-spec list(binary(), cowboy_req:req()) -> cowboy_req:req().
list(<<"GET">>, Req0) ->
    {Page, Size} = elib_param:page(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    StatusFilter = proplists:get_value(<<"status">>, Qs, <<"-1">>),

    Tb = channel_repo:tablename(),
    Column = <<"id, name, type, owner_id, custom_id, description, avatar, "
               "subscriber_count, status, created_at, updated_at">>,

    Where = case StatusFilter of
        <<"-1">> -> #{};
        <<"1">> -> #{status => 1};
        <<"0">> -> #{status => 0};
        _ -> #{}
    end,

    {ok, Payload} = elib_pg:page_with_total(Tb, Column, Where, <<"id desc">>, Page, Size),
    elib_response:success(Req0, Payload);
list(_, Req0) ->
    Req0.

%% @doc 获取频道详情
-spec detail(binary(), cowboy_req:req()) -> cowboy_req:req().
detail(<<"GET">>, Req0) ->
    case cowboy_req:binding(channel_id, Req0) of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        ChannelIdBin ->
            ChannelId = ec_cnv:to_integer(ChannelIdBin),
            Column = <<"id, name, type, owner_id, custom_id, description, avatar, "
                      "tags, subscriber_count, status, created_at, updated_at">>,
            case channel_repo:find_by_id(ChannelId, Column) of
                {error, _} ->
                    elib_response:error(Req0, <<"频道不存在"/utf8>>, ?ERR_NOT_FOUND);
                Channel ->
                    elib_response:success(Req0, Channel)
            end
    end;
detail(_, Req0) ->
    Req0.

%% @doc 搜索频道
-spec search(binary(), cowboy_req:req()) -> cowboy_req:req().
search(<<"GET">>, Req0) ->
    Qs = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"keyword">>, Qs, <<>>),
    Limit = case proplists:get_value(<<"limit">>, Qs) of
        undefined -> 20;
        LimitBin -> binary_to_integer(LimitBin)
    end,

    case Keyword of
        <<>> ->
            elib_response:success(Req0, #{items => [], page => 1, size => Limit, total => 0});
        _ ->
            Column = <<"id, name, type, owner_id, custom_id, description, "
                      "subscriber_count, status, created_at">>,
            case channel_repo:search(Keyword, Limit, Column) of
                {ok, Channels} ->
                    elib_response:success(Req0, #{items => Channels, page => 1, size => Limit, total => length(Channels)});
                {error, _} ->
                    elib_response:success(Req0, #{items => [], page => 1, size => Limit, total => 0})
            end
    end;
search(_, Req0) ->
    Req0.

%% @doc 删除频道（软删除）
-spec delete_action(binary(), cowboy_req:req()) -> cowboy_req:req().
delete_action(<<"DELETE">>, Req0) ->
    PostVals = elib_param:post(Req0),
    ChannelId = maps:get(<<"id">>, PostVals, undefined),

    case ChannelId of
        undefined ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            ChannelIdInt = ec_cnv:to_integer(ChannelId),
            case channel_repo:delete(ChannelIdInt) of
                {ok, _} ->
                    elib_response:success(Req0, #{}, <<"频道已删除"/utf8>>);
                {error, Reason} ->
                    ?DEBUG_LOG("删除频道失败: ~p", [Reason]),
                    elib_response:error(Req0, <<"删除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end;
delete_action(_, Req0) ->
    Req0.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
