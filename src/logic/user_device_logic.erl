-module(user_device_logic).
%%%
% user_device 业务逻辑模块
% user_device business logic module
%%%

-export([device_name/2,
         change_name/3,
         delete/2]).
-export([page/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 获取设备名称
%% 获取设备的显示名称，支持缓存
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return binary() 设备名称
-spec device_name(integer(), binary()) -> binary().
% DID = <<"3f039a2b4724a5b7">>.
% Key = {user_device_name, Uid, DID}.
% user_device_logic:device_name(1, <<"HUAWEIMRD-AL00">>).
% user_device_ds:device_name(1, <<"HUAWEIMRD-AL00">>).
%  imboy_cache:get(Key).
device_name(Uid, DID) ->
    Key = {user_device_name, 2, Uid, DID},
    Fun = fun() -> user_device_ds:device_name(Uid, DID) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


%% @doc 修改设备名称
%% 修改设备的显示名称
%% @param Uid 用户ID
%% @param DID 设备ID
%% @param Name 新的设备名称
%% @return ok
-spec change_name(integer(), binary(), binary()) -> ok.
change_name(Uid, DID, Name) ->
    Set = <<"device_name = $1">>,
    SetArgs = [Name],
    _ = user_device_ds:update_by_did(Uid, DID, Set, SetArgs),

    Key = {user_device_name, 2, Uid, DID},
    imboy_cache:flush(Key),
    ok.


%% @doc 删除设备
%% 删除指定设备记录
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return ok
-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    user_device_ds:delete(Uid, DID),
    Key = {user_device_name, 2, Uid, DID},
    imboy_cache:flush(Key),
    ok.


%% @doc 获取用户设备列表（分页）
%% 获取用户的所有设备，包含在线状态
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页大小
%% @return map() 包含 total、page、size、list 的分页结果
-spec page(integer(), integer(), integer()) -> map().
page(Uid, Page, Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_device_ds:count_by_uid(Uid),
    case user_device_ds:page(Uid, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            OnlineDids = imboy_syn:online_dids(Uid),
            Items2 = [elib_response:json_decode_field(
                         #{<<"online">> => lists:member(maps:get(<<"device_id">>, Row), OnlineDids),
                           <<"device_id">> => maps:get(<<"device_id">>, Row),
                           <<"device_name">> => maps:get(<<"device_name">>, Row, <<>>),
                           <<"device_type">> => maps:get(<<"device_type">>, Row, <<>>),
                           <<"last_active_at">> => maps:get(<<"last_active_at">>, Row, <<>>),
                           <<"device_vsn">> => maps:get(<<"device_vsn">>, Row, <<>>)},
                         <<"device_vsn">>)
                      || Row <- Items0],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

