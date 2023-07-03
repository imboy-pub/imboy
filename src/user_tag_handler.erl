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
        delete ->
            delete(Req0, State);
        set ->
            set(Req0, State);
        page ->
            page(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新。
delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, <<>>),

    Scene2 = case Scene of
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
        true ->
            user_tag_logic:delete(CurrentUid, Scene2, Tag),
            imboy_response:success(Req0, #{}, "success.")
    end.

add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, []),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = proplists:get_value(<<"objectId">>, PostVals, <<>>),
    % user_tag_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    Scene2 = case Scene of
        <<"collect">> ->
            <<"1">>;
        <<"friend">> ->
            <<"2">>;
        _ ->
            <<>>
    end,
    Tag2 = [Name || Name <- Tag, string:length(Name) > 14],
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
            case user_tag_logic:add(CurrentUid, Scene2, ObjectId, Tag) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.

set(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    TagName = proplists:get_value(<<"tagName">>, PostVals, <<>>),
    TagId = proplists:get_value(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectIds = proplists:get_value(<<"objectIds">>, PostVals, []),
    % user_tag_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    Scene2 = case Scene of
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
            case user_tag_logic:set(CurrentUid, Scene2, ObjectIds, TagId, TagName) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.

page(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_req:page_size(Req0),

    #{scene := Scene} = cowboy_req:match_qs([{scene, [], <<>>}], Req0),
    OrderBy = <<"id desc">>,
    UidBin = integer_to_binary(CurrentUid),
    {Scene2, Where} = case Scene of
        <<"collect">> ->
            {<<"1">>, <<"creator_user_id = ", UidBin/binary, " and scene = 1">>};
        <<"friend">> ->
            {<<"2">>, <<"creator_user_id = ", UidBin/binary, " and scene = 2">>};
        _ ->
            {<<>>, <<>>}
    end,
    if
        CurrentUid == 0 ->
            imboy_response:error(Req0, <<"token无效"/utf8>>, 706);
        bit_size(Scene2) == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        true ->
            Payload = user_tag_logic:tag_page(Scene2, Page, Size, Where, OrderBy) ,
            imboy_response:success(Req0, Payload)
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
