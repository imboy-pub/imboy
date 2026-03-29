-module(user_denylist_logic).

%%%
% user_denylist 业务逻辑模块
% user_denylist business logic module
%%%

-export([add/2,
         add/3,
         remove/2]).
-export([page/3]).
-export([in_denylist/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 黑名单分页列表
%% 获取用户黑名单的分页数据
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页大小
%% @return map() 包含 total、page、size、list 的分页结果
-spec page(integer(), integer(), integer()) -> map().
page(Uid, Page, Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_denylist_ds:count_for_uid(Uid),
    case user_denylist_ds:page_for_uid(Uid, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            Items2 = [#{<<"denied_user_id">> => elib_hashids:encode(maps:get(<<"denied_user_id">>, Row)),
                        <<"created_at">> => maps:get(<<"created_at">>, Row, <<>>),
                        <<"nickname">> => maps:get(<<"nickname">>, Row, <<>>),
                        <<"avatar">> => maps:get(<<"avatar">>, Row, <<>>),
                        <<"account">> => maps:get(<<"account">>, Row, <<>>),
                        <<"sign">> => maps:get(<<"sign">>, Row, <<>>),
                        <<"remark">> => maps:get(<<"remark">>, Row, <<>>),
                        <<"tag">> => maps:get(<<"tag">>, Row, <<>>),
                        <<"gender">> => maps:get(<<"gender">>, Row, 0),
                        <<"region">> => maps:get(<<"region">>, Row, <<>>),
                        <<"source">> => maps:get(<<"source">>, Row, <<>>)} || Row <- Items0],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


%% @doc 添加黑名单
%% 将用户添加到黑名单
%% @param Uid 当前用户ID
%% @param DeniedUserId 被拉黑的用户ID
%% @return binary() 操作时间戳
-spec add(integer(), integer()) -> binary().
add(Uid, DeniedUserId) ->
    Now = elib_dt:now(),
    _ = user_denylist_ds:add(Uid, DeniedUserId, Now),
    Key = {in_denylist, Uid, DeniedUserId},
    imboy_cache:flush(Key),
    Now.

%% @doc 兼容旧入口：保留 remark 参数并返回 ok
-spec add(integer(), integer(), binary()) -> ok.
add(Uid, DeniedUserId, _Remark) ->
    _ = add(Uid, DeniedUserId),
    ok.


%% @doc 移除黑名单
%% 将用户从黑名单中移除
%% @param Uid 当前用户ID
%% @param DeniedUserId 被移除的用户ID
%% @return ok
-spec remove(integer(), integer()) -> ok.
remove(Uid, DeniedUserId) ->
    _ = user_denylist_ds:remove(Uid, DeniedUserId),
    Key = {in_denylist, Uid, DeniedUserId},
    imboy_cache:flush(Key),
    ok.


%% @doc 检查用户是否在黑名单中
%% 检查指定用户是否在当前用户的黑名单中
%% @param Uid 当前用户ID
%% @param DeniedUserId 待检查的用户ID
%% @return integer() 1 表示在黑名单中，0 表示不在
-spec in_denylist(integer(), integer()) -> integer().
in_denylist(Uid, DeniedUserId) ->
    Key = {in_denylist, Uid, DeniedUserId},
    Fun = fun() -> user_denylist_ds:in_denylist(Uid, DeniedUserId) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
