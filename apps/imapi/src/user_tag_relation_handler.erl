-module(user_tag_relation_handler).
%%%
% user_tag_relation 控制器模块
% user_tag_relation controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            add ->
                add(Req0, State);
            set ->
                set(Req0, State);
            remove ->
                remove(Req0, State);
            collect_page ->
                page(<<"collect">>, Req0, State);
            friend_page ->
                page(<<"friend">>, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% 用户标签_给特定对象打标签
add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    ?LOG(["CurrentUid ", CurrentUid]),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, []),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = proplists:get_value(<<"objectId">>, PostVals, <<>>),
    % user_tag_relation_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    {Scene2, IsFriend} =
        case Scene of
            <<"collect">> ->
                {<<"1">>, false};
            <<"friend">> ->
                {<<"2">>, friend_ds:is_friend(CurrentUid, imboy_hashids:uid_decode(ObjectId))};
            _ ->
                <<>>
        end,
    Tag2 = [ Name || Name <- Tag, string:length(Name) > 14 ],
    ObjectId2 =
        if
            Scene2 == <<"2">>, IsFriend == false ->
                <<>>;
            true ->
                ObjectId
        end,
    if
        bit_size(Scene2) == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        length(Tag2) > 0 ->
            imboy_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        length(Tag) == 0, bit_size(ObjectId) == 0 ->
            imboy_response:error(Req0, <<"ObjectId Tag 不能同时为空"/utf8>>);
        length(Tag) > 1, bit_size(ObjectId) == 0 ->
            imboy_response:error(Req0, <<"ObjectId 不能为空"/utf8>>);
        true ->
            ?LOG(["before logic CurrentUid ", CurrentUid]),
            case user_tag_relation_logic:add(CurrentUid, Scene2, ObjectId2, Tag) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.


% 用户标签_联系人标签设置标签
set(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    TagName = proplists:get_value(<<"tagName">>, PostVals, <<>>),
    TagId = proplists:get_value(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectIds = proplists:get_value(<<"objectIds">>, PostVals, []),
    % user_tag_relation_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    Scene2 =
        case Scene of
            <<"collect">> ->
                <<"1">>;
            <<"friend">> ->
                <<"2">>;
            _ ->
                <<>>
        end,
    TagLen = string:length(TagName),
    if
        bit_size(Scene2) == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        TagLen > 14 ->
            imboy_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        TagId < 1 ->
            imboy_response:error(Req0, <<"TagId 不能同时为空"/utf8>>);
        true ->
            case user_tag_relation_logic:set(CurrentUid, Scene2, ObjectIds, TagId, TagName) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.


%% 用户标签_标签详情-标签联系人列表-移除标签里的联系人
remove(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    TagId = proplists:get_value(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = proplists:get_value(<<"objectId">>, PostVals, <<>>),
    % user_tag_relation_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    % INSERT INTO user_tag_relation (id, scene, user_id, tag_id, object_id, created_at) VALUES(43, 2, 109, 56, 108, 1688916074887);
    % aaa30,aaa1,你好你好你好你好你好你好你1,abc1,端订单1,
    % user_tag_relation_logic:remove(109, <<"2">>, 108, 56).
    Scene2 =
        case Scene of
            <<"collect">> ->
                <<"1">>;
            <<"friend">> ->
                <<"2">>;
            _ ->
                <<>>
        end,
    if
        bit_size(Scene2) == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        bit_size(ObjectId) == 0 ->
            imboy_response:error(Req0, <<"ObjectId 不能同时为空"/utf8>>);
        TagId < 1 ->
            imboy_response:error(Req0, <<"TagId 不能同时为空"/utf8>>);
        true ->
            case user_tag_relation_logic:remove(CurrentUid, Scene2, imboy_hashids:uid_decode(ObjectId), TagId) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.


%% 用户标签_标签详情-标签联系人列表 / 标签收藏列表
page(Scene, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_req:page_size(Req0),

    #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
    {ok, TagId} = imboy_req:get_int(tag_id, Req0, 0),
    % imboy_log:info(io_lib:format("user_tag_relation_handler:page/2 TagId: ~p; ~n", [TagId])),
    if
        CurrentUid == 0 ->
            imboy_response:error(Req0, <<"token无效"/utf8>>, 706);
        TagId == 0 ->
            imboy_response:error(Req0, <<"tag_id 格式有误"/utf8>>);
        Scene == <<"collect">> ->
            imboy_response:success(Req0, #{});
        Scene == <<"friend">> ->
            Payload = friend_ds:page_by_tag(CurrentUid, Page, Size, TagId, Kwd),
            imboy_response:success(Req0, Payload);
        true ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>)
    end.


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
