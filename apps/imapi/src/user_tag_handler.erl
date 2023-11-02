-module(user_tag_handler).
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
            page ->
                page(Req0, State);
            change_name ->
                change_name(Req0, State);
            add ->
                add(Req0, State);
            delete ->
                delete(Req0, State);
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

    #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
    #{scene := Scene} = cowboy_req:match_qs([{scene, [], <<>>}], Req0),
    OrderBy = <<"id desc">>,
    UidBin = integer_to_binary(CurrentUid),
    {Scene2, Where} =
        case Scene of
            <<"collect">> ->
                {<<"1">>, <<"creator_user_id = ", UidBin/binary, " and scene = 1">>};
            <<"friend">> ->
                {<<"2">>, <<"creator_user_id = ", UidBin/binary, " and scene = 2">>};
            _ ->
                {<<>>, <<>>}
        end,
    Where2 =
        if
            byte_size(Kwd) > 0 ->
                <<Where/binary, " and name like '%", Kwd/binary, "%'">>;
            true ->
                Where
        end,

    if
        CurrentUid == 0 ->
            imboy_response:error(Req0, <<"token无效"/utf8>>, 706);
        bit_size(Scene2) == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        true ->
            Payload = user_tag_logic:page(Scene2, Page, Size, Where2, OrderBy),
            imboy_response:success(Req0, Payload)
    end.


%% 修改标签名称
change_name(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    TagName = proplists:get_value(<<"tagName">>, PostVals, <<>>),
    TagId = proplists:get_value(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    % user_tag_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
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
            Uid2 = integer_to_binary(CurrentUid),
            TagId2 = integer_to_binary(TagId),
            Count = imboy_db:pluck(<<"user_tag">>,
                                   <<"scene = ", Scene2/binary, " AND creator_user_id = ", Uid2/binary, " AND name = '",
                                     TagName/binary, "' AND id != ", TagId2/binary>>,
                                   <<"count(*)">>,
                                   0),
            case user_tag_logic:change_name(Count, Uid2, Scene2, TagId2, TagName) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.


%% 新建标签
add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, <<>>),

    Scene2 =
        case Scene of
            <<"collect">> ->
                <<"1">>;
            <<"friend">> ->
                <<"2">>;
            _ ->
                <<>>
        end,
    TagLen = string:length(Tag),
    if
        bit_size(Scene2) == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        TagLen > 14 ->
            imboy_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        true ->
            case user_tag_logic:add(CurrentUid, Scene2, Tag) of
                {ok, _, [{ID}]} ->
                    imboy_response:success(Req0, #{tagId => ID}, "success.");
                {Code, Err} ->
                    imboy_response:error(Req0, Err, Code);
                Err when is_binary(Err) ->
                    imboy_response:error(Req0, Err);
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.


% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新。
delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, <<>>),

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
        true ->
            user_tag_logic:delete(CurrentUid, Scene2, Tag),
            imboy_response:success(Req0, #{}, "success.")
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
