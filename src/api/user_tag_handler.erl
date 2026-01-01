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
    {Page, Size} = imboy_param:page(Req0),

    #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
    #{scene := Scene} = cowboy_req:match_qs([{scene, [], <<>>}], Req0),
    OrderBy = <<"id desc">>,
    UidBin = integer_to_binary(CurrentUid),
    {Scene2, Where} =
        case Scene of
            <<"collect">> ->
                {1, <<"creator_user_id = ", UidBin/binary, " and scene = 1">>};
            <<"friend">> ->
                {2, <<"creator_user_id = ", UidBin/binary, " and scene = 2">>};
            _ ->
                {0, <<>>}
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
        Scene2 /= 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        true ->
            WhereMap = #{<<"__raw">> => Where2},
            Payload = user_tag_logic:page(Scene2, Page, Size, WhereMap, OrderBy),
            imboy_response:success(Req0, Payload)
    end.


%% 修改标签名称
change_name(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:encode(CurrentUid),

    PostVals = imboy_param:post(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    TagName = proplists:get_value(<<"tagName">>, PostVals, <<>>),
    TagId = proplists:get_value(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    % user_tag_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).

    Scene2 = case Scene of
        <<"collect">> -> 1;
        <<"friend">>  -> 2;
        _ -> 0
    end,

    case {Scene2, string:length(TagName), TagId} of
        {0, _, _} ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        {_, Len, _} when Len > 14 ->
            imboy_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        {_, _, Id} when Id < 1 ->
            imboy_response:error(Req0, <<"TagId 不能同时为空"/utf8>>);
        {S2, _, Id} ->
            Count = imboy_pg:pluck_value(
                <<"public.user_tag">>,
                #{<<"scene">> => S2,
                  <<"creator_user_id">> => CurrentUid,
                  <<"name">> => TagName,
                  <<"id">> => {neq, Id}},
                #{},
                <<"count(*)">>,
                0
            ),
            case user_tag_logic:change_name(Count, CurrentUid, S2, Id, TagName) of
                ok ->
                    imboy_response:success(Req0, #{}, "success.");
                Err ->
                    imboy_response:error(Req0, Err)
            end
    end.


%% 新建标签
add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:encode(CurrentUid),

    PostVals = imboy_param:post(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, <<>>),

    Scene2 = case Scene of
        <<"collect">> ->
            1;
        <<"friend">> ->
            2;
        _ ->
            0
    end,
    TagLen = string:length(Tag),
    if
        Scene2 == 0 ->
            imboy_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        TagLen > 14 ->
            imboy_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        true ->
            case user_tag_logic:add(CurrentUid, Scene2, Tag) of
                {ok, TagId} ->
                    imboy_response:success(Req0, #{<<"tagId">>=>TagId}, "success.");
                {error, Err} ->
                    imboy_response:error(Req0, Err)
            end
    end.


% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新。
delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Scene = proplists:get_value(<<"scene">>, PostVals, <<>>),
    Tag = proplists:get_value(<<"tag">>, PostVals, <<>>),

    Scene2 =
        case Scene of
            <<"collect">> ->
                1;
            <<"friend">> ->
                2;
            _ ->
                2
        end,
    if
        Scene2 == 0 ->
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
