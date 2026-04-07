-module(fts_handler).

%%%
% fts 控制器模块
% fts controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            user_search ->
                user_search(Req0, State);
            recently_user ->
                recently_user(Req0, State);
            msg ->
                msg(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 解析搜索选项
%% 从查询字符串中解析出高级搜索选项
%% @param QueryList 查询参数列表
%% @return 选项 map
-spec parse_search_options(proplists:proplist()) -> map().
parse_search_options(QueryList) ->
    Options0 = #{},

    % 解析日期范围
    Options1 = case proplists:get_value(<<"start_date">>, QueryList, undefined) of
        undefined -> Options0;
        StartDate -> maps:put(<<"start_date">>, StartDate, Options0)
    end,

    Options2 = case proplists:get_value(<<"end_date">>, QueryList, undefined) of
        undefined -> Options1;
        EndDate -> maps:put(<<"end_date">>, EndDate, Options1)
    end,

    % 解析消息类型
    Options3 = case proplists:get_value(<<"msg_type">>, QueryList, undefined) of
        undefined -> Options2;
        MsgType -> maps:put(<<"msg_type">>, MsgType, Options2)
    end,

    % 解析发送者ID（需要解码）
    Options4 = case proplists:get_value(<<"from_uid">>, QueryList, undefined) of
        undefined -> Options3;
        FromUid ->
            case ec_cnv:to_integer(FromUid) of
                FromUidInt when is_integer(FromUidInt), FromUidInt > 0 -> maps:put(<<"from_uid">>, FromUidInt, Options3);
                _ -> Options3
            end
    end,

    % 解析会话ID（需要解码）
    Options5 = case proplists:get_value(<<"conversation_id">>, QueryList, undefined) of
        undefined -> Options4;
        ConversationId ->
            case ec_cnv:to_integer(ConversationId) of
                ConversationIdInt when is_integer(ConversationIdInt), ConversationIdInt > 0 -> maps:put(<<"conversation_id">>, ConversationIdInt, Options4);
                _ -> Options4
            end
    end,

    % 解析排序方式
    Options6 = case proplists:get_value(<<"sort_by">>, QueryList, undefined) of
        undefined -> Options5;
        SortBy -> maps:put(<<"sort_by">>, SortBy, Options5)
    end,

    Options6.

%% @doc 搜索用户
%% 搜索允许被搜索的用户
%%
%% @param Req0 Cowboy请求对象，包含搜索关键词和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含搜索结果的响应
%% @end
-spec user_search(cowboy_req:req(), map()) -> cowboy_req:req().
user_search(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    Qs1 = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"keyword">>, Qs1, <<"">>),
    Payload = fts_logic:user_search_page(CurrentUid, Page, Size, Keyword),
    elib_response:success(Req0, Payload).

%% @doc 最近新注册的用户
%% 获取最近新注册的并且允许被搜索到的用户
%%
%% @param Req0 Cowboy请求对象，包含搜索关键词和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含搜索结果的响应
%% @end
-spec recently_user(cowboy_req:req(), map()) -> cowboy_req:req().
recently_user(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    Qs5 = cowboy_req:parse_qs(Req0),
    Keyword = proplists:get_value(<<"keyword">>, Qs5, <<"">>),
    Payload = fts_logic:recently_user_page(CurrentUid, Page, Size, Keyword),
    elib_response:success(Req0, Payload).

%% @doc 消息全文搜索
%% 搜索私聊和群聊消息内容，支持多种筛选条件
%%
%% @param Req0 Cowboy请求对象，包含搜索关键词、消息类型和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含搜索结果的响应
%% @end
-spec msg(cowboy_req:req(), map()) -> cowboy_req:req().
msg(Req0, State) ->
    case imboy_policy:message_search_enabled() of
        false ->
            elib_response:error(
                Req0,
                imboy_error:error_msg(?ERR_FEATURE_DISABLED),
                ?ERR_FEATURE_DISABLED
            );
        true ->
            CurrentUid = auth_ds:current_uid(State),
            {Page, Size} = elib_param:page(Req0),
            Qs1 = cowboy_req:parse_qs(Req0),
            Keyword = proplists:get_value(<<"keyword">>, Qs1, <<"">>),
            Type = proplists:get_value(<<"type">>, Qs1, <<"C2C">>),

            % 解析增强搜索选项
            Options = parse_search_options(Qs1),

            Payload = fts_logic:search_msg(CurrentUid, Page, Size, Keyword, Type, Options),
            elib_response:success(Req0, Payload)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
