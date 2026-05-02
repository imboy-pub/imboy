-module(group_category_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API Functions
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            create ->
                create(Req0, State);
            list ->
                list(Req0, State);
            rename ->
                rename(Req0, State);
            delete ->
                delete(Req0, State);
            move_group ->
                move_group(Req0, State);
            sort ->
                sort(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% @doc 创建群组分类
%% 为当前用户创建一个新的群组分类
%%
%% @param Req0 Cowboy请求对象，包含分类名称
%% @param State 状态映射，包含 current_uid
%% @return 返回包含新分类ID的响应或错误响应
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    CategoryName = maps:get(<<"category_name">>, PostVals, <<>>),

    case group_category_logic:create(CurrentUid, CategoryName) of
        {error, ErrorMsg} when is_binary(ErrorMsg) ->
            elib_response:error(Req0, ErrorMsg);
        {ok, CategoryId} ->
            Data = #{<<"id">> => CategoryId,
                    <<"category_name">> => CategoryName},
            elib_response:success(Req0, Data, <<"创建分类成功"/utf8>>)
    end.

%% @doc 查询用户的群组分类列表
%% 获取当前用户的所有群组分类
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回包含分类列表的响应或错误响应
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    case group_category_logic:list(CurrentUid) of
        {error, ErrorMsg} when is_binary(ErrorMsg) ->
            elib_response:error(Req0, ErrorMsg);
        {ok, Categories} when is_list(Categories) ->
            elib_response:success(Req0, #{<<"categories">> => Categories}, <<"success."/utf8>>)
    end.

%% @doc 重命名群组分类
%% 修改当前用户的指定群组分类名称
%%
%% @param Req0 Cowboy请求对象，包含分类ID和新名称
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
-spec rename(cowboy_req:req(), map()) -> cowboy_req:req().
rename(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    CategoryId = normalize_id(maps:get(<<"id">>, PostVals, 0)),
    NewName = maps:get(<<"category_name">>, PostVals, <<>>),

    case group_category_logic:rename(CurrentUid, CategoryId, NewName) of
        {error, ErrorMsg} when is_binary(ErrorMsg) ->
            elib_response:error(Req0, ErrorMsg);
        ok ->
            elib_response:success(Req0, #{}, <<"重命名成功"/utf8>>)
    end.

%% @doc 删除群组分类
%% 删除当前用户的指定群组分类
%% 删除分类时会将该分类下的群组移到默认分类
%%
%% @param Req0 Cowboy请求对象，包含分组ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    CategoryId = normalize_id(maps:get(<<"id">>, PostVals, 0)),

    case group_category_logic:delete(CurrentUid, CategoryId) of
        {error, ErrorMsg} when is_binary(ErrorMsg) ->
            elib_response:error(Req0, ErrorMsg);
        ok ->
            elib_response:success(Req0, #{}, <<"删除成功"/utf8>>)
    end.

%% @doc 移动群组到指定分类
%% 将群组移动到指定的分类
%%
%% @param Req0 Cowboy请求对象，包含群组ID和目标分类ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
-spec move_group(cowboy_req:req(), map()) -> cowboy_req:req().
move_group(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Gid = normalize_id(maps:get(<<"gid">>, PostVals, 0)),
    CategoryId = normalize_id(maps:get(<<"category_id">>, PostVals, 0)),

    case group_category_logic:move_group(CurrentUid, Gid, CategoryId) of
        {error, ErrorMsg} when is_binary(ErrorMsg) ->
            elib_response:error(Req0, ErrorMsg);
        ok ->
            elib_response:success(Req0, #{}, <<"移动成功"/utf8>>)
    end.

%% @doc 更新分类排序
%% 批量更新多个分类的排序顺序
%%
%% @param Req0 Cowboy请求对象，包含排序列表
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
-spec sort(cowboy_req:req(), map()) -> cowboy_req:req().
sort(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    SortOrders = maps:get(<<"sort_orders">>, PostVals, []),

    %% 解析排序列表
    %% 格式: [{id, sort_order}, ...]
    SortOrders2 = case SortOrders of
        List when is_list(List) ->
            [{normalize_id(maps:get(<<"id">>, Item, 0)),
              maps:get(<<"sort_order">>, Item, 0)}
             || Item <- List];
        _ ->
            []
    end,

    case group_category_logic:update_sort_order(CurrentUid, SortOrders2) of
        {error, ErrorMsg} when is_binary(ErrorMsg) ->
            elib_response:error(Req0, ErrorMsg);
        ok ->
            elib_response:success(Req0, #{}, <<"更新排序成功"/utf8>>)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 兼容 ID 参数:
%% - int
%% - 数字字符串
-spec normalize_id(term()) -> integer().
normalize_id(Value) when is_integer(Value), Value >= 0 ->
    Value;
normalize_id(Value) when is_list(Value) ->
    normalize_id(ec_cnv:to_binary(Value));
normalize_id(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    elib_cnv:safe_to_integer(Value);
                false ->
                    elib_cnv:safe_to_integer(Value)
            end
    end;
normalize_id(_Value) ->
    0.
