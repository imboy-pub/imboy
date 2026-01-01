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
-include("include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec init(any(), any()) -> {ok, any(), any()}.
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


page(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_param:page(Req0),
    {ok, Kind} = imboy_param:int(kind, Req0, 0),
    #{order := OrderBy} = cowboy_req:match_qs([{order, [], <<>>}], Req0),
    #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
    #{tag := Tag} = cowboy_req:match_qs([{tag, [], <<>>}], Req0),

    % Build WHERE clause
    WhereParts = [
        <<"user_id = ", (integer_to_binary(CurrentUid))/binary>>,
        <<"status = 1">>,
        case Kind of
            0 -> <<>>;
            _ -> <<"kind = ", (integer_to_binary(Kind))/binary>>
        end,
        case byte_size(Kwd) > 0 of
            true ->
                EscapedKwd = imboy_str:replace_single_quote(Kwd),
                <<"(source like '%", EscapedKwd/binary, "%' or remark like '%", EscapedKwd/binary,
                  "%' or info like '%", EscapedKwd/binary, "%')">>;
            false ->
                <<>>
        end,
        case byte_size(Tag) > 0 of
            true ->
                EscapedTag = imboy_str:replace_single_quote(Tag),
                <<"tag like '%", EscapedTag/binary, ",%'">>;
            false ->
                <<>>
        end
    ],

    % Filter out empty parts and join with ' AND '
    NonEmptyParts = [P || P <- WhereParts, byte_size(P) > 0],
    WhereSql = imboy_cnv:implode(<<" AND ">>, NonEmptyParts),
    WhereMap = #{<<"__raw">> => WhereSql},

    % Determine order
    Order = case OrderBy of
        <<"recent_use">> -> <<"updated_at desc, id desc">>;
        _ -> <<"id desc">>
    end,

    % Build column list
    Info = imboy_hasher:decoded_field(<<"info">>),
    Column = <<"kind, kind_id, source, created_at, updated_at, tag, ", Info/binary>>,
    Tb = user_collect_repo:tablename(),

    {ok, Payload} = imboy_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size),
    % Parse info field JSON string to structured data
    List = maps:get(list, Payload, []),
    List2 = imboy_response:json_decode_list_field(List, <<"info">>),
    Payload2 = maps:put(list, List2, Payload),
    imboy_response:success(Req0, Payload2).


add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    % Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
    Kind = proplists:get_value(<<"kind">>, PostVals, <<"">>),
    KindId = proplists:get_value(<<"kind_id">>, PostVals, <<"">>),
    Source = proplists:get_value(<<"source">>, PostVals, <<"">>),
    Remark = proplists:get_value(<<"remark">>, PostVals, <<"">>),
    Info = proplists:get_value(<<"info">>, PostVals, []),
    case user_collect_logic:add(CurrentUid, Kind, KindId, Info, Source, Remark) of
        {ok, _Msg} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.


remove(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    KindId = proplists:get_value(<<"kind_id">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    user_collect_logic:remove(CurrentUid, KindId),
    imboy_response:success(Req0, #{}, "success.").


change(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Action = proplists:get_value(<<"action">>, PostVals, <<>>),
    KindId = proplists:get_value(<<"kind_id">>, PostVals, <<>>),
    user_collect_logic:change(CurrentUid, Action, KindId, PostVals),
    imboy_response:success(Req0, #{}, "success.").


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
