-module(user_tag_handler).

%%%
% user_tag_relation 控制器模块
% user_tag_relation controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").
-include("error_code.hrl").

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

%% @doc 标签分页列表
%% 获取用户的标签列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页和筛选参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含标签列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),

    Qs = cowboy_req:parse_qs(Req0),
    Kwd = proplists:get_value(<<"kwd">>, Qs, <<>>),
    Scene = proplists:get_value(<<"scene">>, Qs, <<>>),
    OrderBy = <<"id desc">>,
    {Scene2, Where0} =
        case Scene of
            <<"collect">> ->
                {1, #{creator_user_id => CurrentUid, scene => 1}};
            <<"friend">> ->
                {2, #{creator_user_id => CurrentUid, scene => 2}};
            _ ->
                {0, #{}}
        end,
    WhereMap =
        case byte_size(Kwd) > 0 of
            true ->
                Where0#{name => {op, <<"LIKE">>, <<"%", Kwd/binary, "%">>}};
            false ->
                Where0
        end,

    if CurrentUid == 0 ->
           elib_response:error(Req0, <<"token无效"/utf8>>, ?ERR_TOKEN_INVALID);
       Scene2 == 0 ->
           elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
       true ->
           Payload = user_tag_logic:page(Scene2, Page, Size, WhereMap, OrderBy),
           elib_response:success(Req0, Payload)
    end.

%% @doc 修改标签名称
%% 修改标签的名称
%%
%% @param Req0 Cowboy请求对象，包含标签ID和新名称
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec change_name(cowboy_req:req(), map()) -> cowboy_req:req().
change_name(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    PostVals = elib_param:post(Req0),
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    TagName = maps:get(<<"tagName">>, PostVals, <<>>),
    TagId = ec_cnv:to_integer(maps:get(<<"tagId">>, PostVals, 0)),
    % 被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)
    % user_tag_logic:add(1, <<"friend">>, <<"2">>, [<<"a">>, <<"b">>]).
    Scene2 =
        case Scene of
            <<"collect">> ->
                1;
            <<"friend">> ->
                2;
            _ ->
                0
        end,

    case {Scene2, string:length(TagName), TagId} of
        {0, _, _} ->
            elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
        {_, Len, _} when Len > 14 ->
            elib_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
        {_, _, Id} when Id < 1 ->
            elib_response:error(Req0, <<"TagId 不能同时为空"/utf8>>);
        {S2, _, Id} ->
            Count =
                elib_pg:pluck_value(<<"public.user_tag">>,
                                     <<"count(*)">>,
                                     #{<<"scene">> => S2,
                                       <<"creator_user_id">> => CurrentUid,
                                       <<"name">> => TagName,
                                       <<"id">> => {op, <<"<>">>, Id}},
                                     #{},
                                     0),
            case user_tag_logic:change_name(Count, CurrentUid, S2, Id, TagName) of
                ok ->
                    elib_response:success(Req0, #{}, "success.");
                Err ->
                    elib_response:error(Req0, Err)
            end
    end.

%% @doc 新建标签
%% 创建一个新的标签
%%
%% @param Req0 Cowboy请求对象，包含标签名称和场景
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    PostVals = elib_param:post(Req0),
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    Tag = maps:get(<<"tag">>, PostVals, <<>>),

    Scene2 =
        case Scene of
            <<"collect">> ->
                1;
            <<"friend">> ->
                2;
            _ ->
                0
        end,
    TagLen = string:length(Tag),
    if Scene2 == 0 ->
           elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
       TagLen > 14 ->
           elib_response:error(Req0, <<"Tag 最多14个字"/utf8>>);
       true ->
           case user_tag_logic:add(CurrentUid, Scene2, Tag) of
               {ok, TagId} ->
                   elib_response:success(Req0, #{<<"tagId">> => TagId}, "success.");
               {error, Err} ->
                   elib_response:error(Req0, Err)
           end
    end.

%% @doc 删除标签
%% 删除指定的标签（标签中的联系人不会被删除）
%%
%% @param Req0 Cowboy请求对象，包含标签名称和场景
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Scene = maps:get(<<"scene">>, PostVals, <<>>),
    Tag = maps:get(<<"tag">>, PostVals, <<>>),

    Scene2 =
        case Scene of
            <<"collect">> ->
                1;
            <<"friend">> ->
                2;
            _ ->
                0
        end,
    if Scene2 == 0 ->
           elib_response:error(Req0, <<"不支持的 Scene"/utf8>>);
       true ->
           user_tag_logic:delete(CurrentUid, Scene2, Tag),
           elib_response:success(Req0, #{}, "success.")
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
