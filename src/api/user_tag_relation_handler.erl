-module(user_tag_relation_handler).

%%%
% user_tag_relation 控制器模块
% user_tag_relation controller module
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

%% @doc 添加标签关联
%% 给特定对象（用户或收藏）打标签
%%
%% @param Req0 Cowboy请求对象，包含对象ID和标签列表
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    PostVals = elib_param:post(Req0),
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    Tag = maps:get(<<"tag">>, PostVals, []),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = maps:get(<<"objectId">>, PostVals, <<>>),
    % ?DEBUG_LOG(["CurrentUid add ", CurrentUid, "; Scene ", Scene, "; Tag: ", Tag, "; ObjectId: ", ObjectId]),
    % user_tag_relation_logic:add(1, 2, <<"2">>, [<<"a">>, <<"b">>]).
    {Scene2, IsFriend} =
        case Scene of
            <<"collect">> ->
                {1, false};
            <<"friend">> ->
                {2, friend_ds:is_friend(CurrentUid, elib_hashids:decode(ObjectId))};
            _ ->
                0
        end,
    Tag2 = [Name || Name <- Tag, string:length(Name) > 14],
    ObjectId2 =
        if Scene2 == 2, IsFriend == false ->
               <<>>;
           true ->
               ObjectId
        end,
    if Scene2 == 0 ->
           elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
       length(Tag2) > 0 ->
           elib_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
       length(Tag) == 0, bit_size(ObjectId) == 0 ->
           elib_response:error(Req0, <<"ObjectId Tag 不能同时为空"/utf8>>);
       length(Tag) > 1, bit_size(ObjectId) == 0 ->
           elib_response:error(Req0, <<"ObjectId 不能为空"/utf8>>);
       true ->
           % ?DEBUG_LOG(["before logic CurrentUid ", CurrentUid]),
           case user_tag_relation_logic:add(CurrentUid, Scene2, ObjectId2, Tag) of
               ok ->
                   elib_response:success(Req0, #{}, "success.");
               Err ->
                   elib_response:error(Req0, Err)
           end
    end.

%% @doc 设置标签关联
%% 批量设置对象的标签
%%
%% @param Req0 Cowboy请求对象，包含对象ID列表和标签信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec set(cowboy_req:req(), map()) -> cowboy_req:req().
set(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    PostVals = elib_param:post(Req0),
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    TagName = maps:get(<<"tagName">>, PostVals, <<>>),
    TagId = maps:get(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectIds = maps:get(<<"objectIds">>, PostVals, []),
    % user_tag_relation_logic:add(1, 2, <<"2">>, [<<"a">>, <<"b">>]).
    Scene2 =
        case Scene of
            <<"collect">> ->
                1;
            <<"friend">> ->
                2;
            _ ->
                0
        end,

    case {Scene2, string:length(TagName), ec_cnv:to_integer(TagId)} of
        {0, _, _} ->
            elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        {_, Len, _} when Len > 14 ->
            elib_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        {_, _, Id} when Id < 1 ->
            elib_response:error(Req0, <<"TagId 不能同时为空"/utf8>>);
        {S2, _, Id} ->
            case user_tag_relation_logic:set(CurrentUid, S2, ObjectIds, Id, TagName) of
                ok ->
                    elib_response:success(Req0, #{}, "success.");
                Err ->
                    elib_response:error(Req0, Err)
            end
    end.

%% @doc 移除标签关联
%% 从标签中移除指定的对象
%%
%% @param Req0 Cowboy请求对象，包含对象ID和标签ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    PostVals = elib_param:post(Req0),
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    TagId = maps:get(<<"tagId">>, PostVals, 0),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    ObjectId = maps:get(<<"objectId">>, PostVals, <<>>),
    % user_tag_relation_logic:add(1, 2, <<"2">>, [<<"a">>, <<"b">>]).
    % INSERT INTO user_tag_relation (id, scene, user_id, tag_id, object_id, created_at) VALUES(43, 2, 109, 56, 108, 1688916074887);
    % aaa30,aaa1,你好你好你好你好你好你好你1,abc1,端订单1,
    % user_tag_relation_logic:remove(109, <<"2">>, 108, 56).
    Scene2 =
        case Scene of
            <<"collect">> ->
                1;
            <<"friend">> ->
                2;
            _ ->
                0
        end,

    case {Scene2, ObjectId, ec_cnv:to_integer(TagId)} of
        {0, _, _} ->
            elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        {_, <<>>, _} ->
            elib_response:error(Req0, <<"ObjectId 不能同时为空"/utf8>>);
        {_, _, Id} when Id < 1 ->
            elib_response:error(Req0, <<"TagId 不能同时为空"/utf8>>);
        {S2, Obj, Id} ->
            user_tag_relation_logic:remove(CurrentUid, integer_to_binary(S2), elib_hashids:decode(Obj), Id),
            elib_response:success(Req0, #{}, "success.")
    end.

%% @doc 标签关联分页列表
%% 获取标签下的对象列表（分页）
%%
%% @param Scene 场景类型（collect或friend）
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含对象列表的响应
%% @end
-spec page(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
page(Scene, Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),

    #{kwd := Kwd} = cowboy_req:match_qs([{kwd, [], <<>>}], Req0),
    {ok, TagId} = elib_param:int(tag_id, Req0, 0),
    % elib_log:info(io_lib:format("user_tag_relation_handler:page/2 TagId: ~p; ~n", [TagId])),
    if CurrentUid == 0 ->
           elib_response:error(Req0, <<"token无效"/utf8>>, ?ERR_TOKEN_INVALID);
       TagId == 0 ->
           elib_response:error(Req0, <<"tag_id 格式有误"/utf8>>);
       Scene == <<"collect">> ->
           elib_response:success(Req0, #{});
       Scene == <<"friend">> ->
           Payload = friend_ds:page_by_tag(CurrentUid, Page, Size, TagId, Kwd),
           elib_response:success(Req0, Payload);
       true ->
           elib_response:error(Req0, <<"不支持的 Scene"/utf8>>)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

