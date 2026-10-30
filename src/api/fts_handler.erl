-module(fts_handler).

-moduledoc "全文搜索（FTS）控制器 —— 用户搜索等检索端点。".
%%%
% fts 控制器模块
% fts controller module
%%%
-behaviour(cowboy_rest).

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
    Options1 =
        case proplists:get_value(<<"start_date">>, QueryList, undefined) of
            undefined -> Options0;
            StartDate -> maps:put(<<"start_date">>, StartDate, Options0)
        end,

    Options2 =
        case proplists:get_value(<<"end_date">>, QueryList, undefined) of
            undefined -> Options1;
            EndDate -> maps:put(<<"end_date">>, EndDate, Options1)
        end,

    % 解析消息类型
    Options3 =
        case proplists:get_value(<<"msg_type">>, QueryList, undefined) of
            undefined -> Options2;
            MsgType -> maps:put(<<"msg_type">>, MsgType, Options2)
        end,

    % 解析发送者ID（需要解码）
    Options4 =
        case proplists:get_value(<<"from_uid">>, QueryList, undefined) of
            undefined ->
                Options3;
            FromUid ->
                case ec_cnv:to_integer(FromUid) of
                    FromUidInt when is_integer(FromUidInt), FromUidInt > 0 ->
                        maps:put(<<"from_uid">>, FromUidInt, Options3);
                    _ ->
                        Options3
                end
        end,

    % 解析排序方式
    Options6 =
        case proplists:get_value(<<"sort_by">>, QueryList, undefined) of
            undefined -> Options4;
            SortBy -> maps:put(<<"sort_by">>, SortBy, Options4)
        end,

    Options6.

conversation_scope(QueryList, Type, Uid) ->
    case proplists:get_all_values(<<"conversation_id">>, QueryList) of
        [] -> {ok, undefined};
        [Value] -> decode_scope(Value, string:lowercase(Type), Uid);
        _ -> error
    end.

decode_scope(Value, Type, Uid) when is_binary(Value) ->
    case binary:split(Value, <<":">>, [global]) of
        [Id] when Type =:= <<"c2c">>; Type =:= <<"c2g">> -> positive_id(Id);
        [<<"c2g">>, Id] when Type =:= <<"c2g">> -> positive_id(Id);
        [<<"c2c">>, A, B] when Type =:= <<"c2c">> ->
            case {positive_id(A), positive_id(B)} of
                {{ok, X}, {ok, Y}} when X =< Y, Uid =:= X -> {ok, Y};
                {{ok, X}, {ok, Y}} when X =< Y, Uid =:= Y -> {ok, X};
                _ -> error
            end;
        _ ->
            error
    end;
decode_scope(_, _, _) ->
    error.

positive_id(Id) when byte_size(Id) > 0, byte_size(Id) =< 19 ->
    case re:run(Id, <<"^[0-9]+$">>, [{capture, none}]) of
        match ->
            N = binary_to_integer(Id),
            case N > 0 andalso N =< 9223372036854775807 of
                true -> {ok, N};
                false -> error
            end;
        nomatch ->
            error
    end;
positive_id(_) ->
    error.

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
            case conversation_scope(Qs1, Type, CurrentUid) of
                error ->
                    elib_response:error(Req0, <<"invalid conversation_id">>, ?ERR_INVALID_PARAM);
                {ok, Scope} ->
                    Options0 = parse_search_options(Qs1),
                    Options =
                        case Scope of
                            undefined -> Options0;
                            _ -> maps:put(<<"conversation_id">>, Scope, Options0)
                        end,
                    case fts_logic:search_msg(CurrentUid, Page, Size, Keyword, Type, Options) of
                        {error, _} ->
                            elib_response:error(
                                Req0, <<"search unavailable">>, ?ERR_SERVICE_UNAVAILABLE
                            );
                        Payload ->
                            elib_response:success(Req0, Payload)
                    end
            end
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

conversation_scope_test() ->
    Scope = fun(V, T) -> conversation_scope([{<<"conversation_id">>, V}], T, 42) end,
    ?assertEqual({ok, 17}, Scope(<<"17">>, <<"C2C">>)),
    ?assertEqual({ok, 17}, Scope(<<"c2c:17:42">>, <<"C2C">>)),
    ?assertEqual({ok, 42}, Scope(<<"c2c:42:42">>, <<"C2C">>)),
    ?assertEqual({ok, 9007199254740993}, Scope(<<"c2g:9007199254740993">>, <<"C2G">>)),
    ?assertEqual(error, Scope(<<"c2c:17:18">>, <<"C2C">>)),
    ?assertEqual(error, Scope(<<"c2c:42:17">>, <<"C2C">>)),
    ?assertEqual(error, Scope(<<"c2g:17">>, <<"C2C">>)),
    ?assertEqual(error, Scope(<<"9223372036854775808">>, <<"C2C">>)),
    ?assertEqual(error, Scope(<<"17x">>, <<"C2C">>)),
    ?assertEqual(
        error,
        conversation_scope(
            [
                {<<"conversation_id">>, <<"17">>},
                {<<"conversation_id">>, <<"18">>}
            ],
            <<"C2C">>,
            42
        )
    ).
