-module(user_device_repo).
%%%
% user_device_repo 是 user_device repository 缩写
% 用户设备数据仓库层，提供用户设备信息的基础数据库操作
%%%

-export([tablename/0]).
-export([save/4]).
-export([login_count/2]).
-export([device_name/2]).
-export([delete/2]).
-export([update_by_did/4]).
-export([count_by_uid/1,
         page/3]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取用户设备表的表名
%% @return 返回用户设备表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_pg_sql:public_tablename(<<"user_device">>).


% user_device_repo:page(1, 10, 0).
-spec page(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page(Uid, Limit, Offset) ->
    Tb = tablename(),
    Column = <<"device_id, device_name, device_type, last_active_at,device_vsn">>,
    Where = <<" WHERE status = $1 and user_id = $2">>,

    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary,
            " ORDER BY last_active_at desc LIMIT $3 OFFSET $4">>,
    % ?DEBUG_LOG([Sql, Uid, Limit, Offset]),
    imboy_pg:query(Sql, [1, Uid, Limit, Offset]).


% user_device_repo:count_by_uid(1).
count_by_uid(Uid) ->
    % 使用安全的参数化查询，避免SQL注入
    case imboy_pg:pluck(tablename(), <<"count(*) as count">>, #{status => 1, user_id => Uid}, #{}) of
        {ok, Count} when is_integer(Count) -> Count;
        _ -> 0
    end.


% user_device_repo:device_name(1, <<"3f039a2b4724a5b7">>).
% user_device_repo:device_name(1, <<"HUAWEIMRD-AL00">>).
-spec device_name(integer(), binary()) -> binary().
device_name(Uid, DID) ->
    % 使用安全的参数化查询，避免SQL注入
    case imboy_pg:pluck(tablename(), <<"device_name">>, #{status => 1, user_id => Uid, device_id => DID}, #{}) of
        {ok, DeviceName} when is_binary(DeviceName) -> DeviceName;
        _ -> <<>>
    end.


% user_device_repo:login_count(1, <<"872619BD-8FCD-45AF-B255-406D70C4D9C9">>).
-spec login_count(Uid :: binary(), DID :: binary()) -> integer().
login_count(Uid, DID) ->
    % 使用安全的参数化查询，避免SQL注入
    case imboy_pg:pluck(tablename(), <<"login_count">>, #{status => 1, user_id => Uid, device_id => DID}, #{}) of
        {ok, LoginCount} when is_integer(LoginCount) -> LoginCount;
        _ -> 0
    end.


-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 1 AND user_id = $1 AND device_id = $2">>,
    imboy_pg:execute(Sql, [Uid, DID]),
    ok.


% user_device_repo:save(1, 1, <<"3f039a2b4724a5b7">>, [{<<"ip">>, <<"127.0.0.1">>}]).
-spec save(binary(), integer(), binary(), list()) -> ok.
save(Now, Uid, DID, PostVals) when is_binary(DID), bit_size(DID) > 0 ->
    % 调用之前判断一次 DID不为空，可以减少一个数据库count查询
    LoginCount = user_device_repo:login_count(Uid, DID),
    % ?DEBUG_LOG(["login save ", Now, Uid, DID, LoginCount]),
    save(Now, Uid, PostVals, DID, LoginCount);
save(_Now, _Uid, _DID, _PostVals) ->
    % 无设备ID登录，无需记录设备信息
    ok.


% user_device_repo:update_by_did(1, <<"3f039a2b4724a5b7">>, <<"device_name = $1">>, [<<"CLT-AL00 1">>]).
-spec update_by_did(integer(), binary(), binary(), list()) -> {ok, integer()} | {error, any()}.
update_by_did(Uid, DID, Set, SetArgs) ->
    Tb = tablename(),
    SetArgsLen = length(SetArgs),
    SetArgsLen2 = integer_to_binary(SetArgsLen + 1),
    SetArgsLen3 = integer_to_binary(SetArgsLen + 2),
    % 更新登录次数，最近登录时间、IP
    Sql = <<"UPDATE ", Tb/binary, " SET ", Set/binary, " WHERE status = 1 AND user_id = $", SetArgsLen2/binary,
            " AND device_id = $", SetArgsLen3/binary>>,
    SetArgs2 = SetArgs ++ [Uid, DID],
    imboy_pg:execute(Sql, SetArgs2).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


-spec save(binary(), integer(), list(), binary(), integer()) -> ok.
save(Now, Uid, PostVals, DID, LoginCount) when bit_size(DID) > 0, LoginCount > 0 ->
    % 更新登录次数，最近登录时间、IP
    Ip = proplists:get_value(<<"ip">>, PostVals, <<>>),
    PublicKey = proplists:get_value(<<"public_key">>, PostVals, <<>>),
    Tb = tablename(),
    Ip2 = case Ip of
        undefined ->
            <<>>;
        _ ->
            Ip
    end,
    % 使用安全的参数化查询，避免SQL注入
    imboy_pg:update(Tb, #{
        login_count => LoginCount + 1,
        last_login_ip => Ip2,
        last_login_at => Now,
        public_key => PublicKey
    }, <<"status = 1 AND user_id = $1 AND device_id = $2">>, [Uid, DID]);
save(Now, Uid, PostVals, DID, _LoginCount) when bit_size(DID) > 0 ->
    % 第一次登陆记录设备信息
    DeviceType = proplists:get_value(<<"cos">>, PostVals, <<>>),
    DeviceVsn = proplists:get_value(<<"dvsn">>, PostVals, <<>>),
    DeviceName = proplists:get_value(<<"dname">>, PostVals, <<>>),
    PublicKey = proplists:get_value(<<"public_key">>, PostVals, <<>>),
    Ip = proplists:get_value(<<"ip">>, PostVals, <<>>),

    imboy_pg:insert(tablename(), #{
        %% 用户ID (字符串类型)
        <<"user_id">> => Uid,
        %% 设备类型 (字符串，如"ios"/"android")
        <<"device_type">> => DeviceType,
        %% 设备唯一标识 (字符串)
        <<"device_id">> => DID,
        %% 设备版本号 (字符串)
        <<"device_vsn">> => DeviceVsn,
        %% 设备名称 (字符串)
        <<"device_name">> => DeviceName,
        %% 登录次数 (整型)
        <<"login_count">> => 1,
        %% 最后登录IP (字符串)
        <<"last_login_ip">> => Ip,
        %% 最后登录时间
        <<"last_login_at">> => Now,
        %% 状态
        <<"status">> => 1,
        %% 公钥 (字符串，特殊字符需要处理)
        <<"public_key">> => PublicKey,
        %% 创建时间
        <<"created_at">> => Now
    }).
