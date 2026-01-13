-module(user_collect_handler).

%%%
% collect 控制器模块
% collect controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

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
            page ->
                page(Req0, State);
            add ->
                add(Req0, State);
            remove ->
                remove(Req0, State);
            change ->
                change(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 收藏分页列表
%% 获取用户的收藏列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页和筛选参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含收藏列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    {ok, Kind} = elib_param:int(kind, Req0, 0),
    #{order := OrderBy} = cowboy_req:match_qs([{order, [], <<>>}], Req0),
    #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
    #{tag := Tag} = cowboy_req:match_qs([{tag, [], <<>>}], Req0),

    Where0 = #{user_id => CurrentUid, status => 1},
    Where1 =
        case Kind of
            0 -> Where0;
            _ -> Where0#{kind => Kind}
        end,
    Where2 =
        case byte_size(Tag) > 0 of
            true ->
                Where1#{tag => {op, <<"LIKE">>, <<"%", Tag/binary, ",%">>}};
            false ->
                Where1
        end,
    WhereMap =
        case byte_size(Kwd) > 0 of
            true ->
                Like = <<"%", Kwd/binary, "%">>,
                Where2#{
                    <<"__or">> => [
                        #{source => {op, <<"LIKE">>, Like}},
                        #{remark => {op, <<"LIKE">>, Like}},
                        #{info => {op, <<"LIKE">>, Like}}
                    ]
                };
            false ->
                Where2
        end,

    % Determine order
    Order =
        case OrderBy of
            <<"recent_use">> ->
                <<"updated_at desc, id desc">>;
            _ ->
                <<"id desc">>
        end,

    % Build column list
    Info = elib_hasher:decoded_field(<<"info">>),
    Column = <<"kind, kind_id, source, created_at, updated_at, tag, ", Info/binary>>,
    Tb = user_collect_repo:tablename(),

    {ok, Payload} = elib_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size),
    % Parse info field JSON string to structured data
    List = maps:get(list, Payload, []),
    List2 = elib_response:json_decode_list_field(List, <<"info">>),
    Payload2 = maps:put(list, List2, Payload),
    elib_response:success(Req0, Payload2).

%% @doc 添加收藏
%% 添加新的收藏项
%%
%% @param Req0 Cowboy请求对象，包含收藏信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    % Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
    Kind = maps:get(<<"kind">>, PostVals, <<"">>),
    KindId = maps:get(<<"kind_id">>, PostVals, <<"">>),
    Source = maps:get(<<"source">>, PostVals, <<"">>),
    Remark = maps:get(<<"remark">>, PostVals, <<"">>),
    Info = maps:get(<<"info">>, PostVals, #{}),
    case user_collect_logic:add(CurrentUid, Kind, KindId, Info, Source, Remark) of
        {ok, _Msg} ->
            elib_response:success(Req0);
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 删除收藏
%% 删除指定的收藏项
%%
%% @param Req0 Cowboy请求对象，包含收藏项ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    KindId = maps:get(<<"kind_id">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    _ = user_collect_logic:remove(CurrentUid, KindId),
    elib_response:success(Req0, #{}, "success.").

%% @doc 修改收藏
%% 修改收藏项的信息
%%
%% @param Req0 Cowboy请求对象，包含收藏项ID和修改内容
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec change(cowboy_req:req(), map()) -> cowboy_req:req().
change(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Action = maps:get(<<"action">>, PostVals, <<>>),
    KindId = maps:get(<<"kind_id">>, PostVals, <<>>),
    user_collect_logic:change(CurrentUid, Action, KindId, PostVals),
    elib_response:success(Req0, #{}, "success.").

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).

%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
