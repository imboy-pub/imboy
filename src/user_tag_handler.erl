-module(user_tag_handler).
%%%
% user_tag 控制器模块
% user_tag controller module
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
        add ->
            add(Req0, State);
        remove ->
            remove(Req0, State);
        % page ->
        %     page(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, []),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = proplists:get_value(<<"objectId">>, PostVals, <<>>),
    % user_tag_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    user_tag_logic:add(CurrentUid, Scene, ObjectId, Tag),
    imboy_response:success(Req0, #{}, "success.").

remove(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, []),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = proplists:get_value(<<"objectId">>, PostVals, <<>>),
    % user_tag_logic:add(1, <<"friend">>, <<"18aw3p">>, [<<"a">>, <<"b">>]).
    user_tag_logic:remove(CurrentUid, Scene, ObjectId, Tag),
    imboy_response:success(Req0, #{}, "success.").

% page(Req0, State) ->
%     CurrentUid = maps:get(current_uid, State),
%     {Page, Size} = imboy_req:page_size(Req0),
%     Kind = imboy_req:get_int(kind, Req0, 0),
%     #{order := OrderBy} = cowboy_req:match_qs([{order, [], <<>>}], Req0),
%     #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
%     % #{kind := Kind} = cowboy_req:match_qs([{kind, [], 0}], Req0),
%     UidBin = integer_to_binary(CurrentUid),
%     ?LOG([page, Kind]),

    % KwdWhere = if
    %     byte_size(Kwd) > 0 ->
    %         <<" and (source like '%", Kwd/binary, "%' or remark like '%", Kwd/binary, "%' or info like '%", Kwd/binary, "%')">>;
    %     true ->
    %         <<>>
    % end,

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
