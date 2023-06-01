-module(collect_handler).
%%%
% collect 控制器模块
% collect controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
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
    {Page, Size} = imboy_req:page_size(Req0),
    Payload = collect_logic:page(CurrentUid, Page, Size),
    imboy_response:success(Req0, Payload).

add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    % 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件
    Kind = proplists:get_value(<<"kind">>, PostVals, <<"">>),
    KindId = proplists:get_value(<<"kind_id">>, PostVals, <<"">>),
    Remark = proplists:get_value(<<"remark">>, PostVals, <<"">>),
    Info = proplists:get_value(<<"info">>, PostVals, []),
    case collect_logic:add(CurrentUid, Kind, KindId, Info, Remark) of
        {ok, _Msg} ->
            imboy_response:success(Req0);
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.

remove(Req0, State) ->
    _CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    % collect_logic:remove(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, PostVals, "success.").

change(Req0, State) ->
    _CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    % collect_logic:change(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, PostVals, "success.").

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
