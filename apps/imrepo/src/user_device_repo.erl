-module(user_device_repo).
%%%
% user_device_repo 是 user_device repository 缩写
%%%

-export([tablename/0]).
-export([save/4]).
-export([login_count/2]).
-export([device_name/2]).
-export([delete/2]).
-export([update_by_did/4]).
-export([count_by_uid/1,
         page/3]).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================
tablename() ->
    imboy_db:public_tablename(<<"user_device">>).


% user_device_repo:page(1, 10, 0).
-spec page(integer(), integer(), integer()) -> {ok, list(), list()} | {error, any()}.
page(Uid, Limit, Offset) ->
    Tb = tablename(),
    Column = <<"device_id, device_name, device_type, last_active_at,device_vsn">>,
    Where = <<" WHERE status = $1 and user_id = $2">>,

    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary,
            " ORDER BY last_active_at desc LIMIT $3 OFFSET $4">>,
    % ?LOG([Sql, Uid, Limit, Offset]),
    imboy_db:query(Sql, [1, Uid, Limit, Offset]).


% user_device_repo:count_by_uid(1).
count_by_uid(Uid) ->
    Uid2 = integer_to_binary(Uid),
    % use index uk_UserId_DeniedUserId
    imboy_db:pluck(tablename(), <<"status = 1 and user_id = ", Uid2/binary>>, <<"count(*) as count">>, 0).


-spec device_name(integer(), binary()) -> binary().
% user_device_repo:device_name(1, <<"3f039a2b4724a5b7">>).
device_name(Uid, DID) ->
    Uid2 = integer_to_binary(Uid),
    Where = <<"status = 1 AND user_id = ", Uid2/binary, " AND device_id = '", DID/binary, "'">>,
    imboy_db:pluck(tablename(), Where, <<"device_name">>, <<>>).


% user_device_repo:login_count(1, <<"3f039a2b4724a5b7">>).
-spec login_count(Uid :: binary(), DID :: binary()) -> integer().
login_count(Uid, DID) ->
    Uid2 = integer_to_binary(Uid),
    imboy_db:pluck(tablename(),
                   <<"status = 1 AND user_id = ", Uid2/binary, " AND device_id = '", DID/binary, "'">>,
                   <<"login_count">>,
                   0).


-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 1 AND user_id = $1 AND device_id = $2">>,
    imboy_db:execute(Sql, [Uid, DID]),
    ok.


% user_device_repo:save(1, 1, <<"3f039a2b4724a5b7">>, [{<<"ip">>, <<"127.0.0.1">>}]).
-spec save(integer(), integer(), binary(), list()) -> ok.
save(Now, Uid, DID, PostVals) when is_binary(DID), bit_size(DID) > 0 ->
    % 调用之前判断一次 DID不为空，可以减少一个数据库count查询
    LoginCount = user_device_repo:login_count(Uid, DID),
    ?LOG(["login save ", Now, Uid, DID, LoginCount]),
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
    imboy_db:execute(Sql, SetArgs2).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


-spec save(integer(), integer(), list(), binary(), integer()) -> ok.
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
    Where = <<"status = 1 AND user_id = ", (ec_cnv:to_binary(Uid))/binary," AND device_id = '", DID/binary,"'">>,
    imboy_db:update(Tb, Where, #{
        login_count => LoginCount + 1,
        last_login_ip => Ip2,
        last_login_at => Now,
        public_key => PublicKey
    });
save(Now, Uid, PostVals, DID, _LoginCount) when bit_size(DID) > 0 ->
    % 第一次登陆记录设备信息
    DeviceType = proplists:get_value(<<"cos">>, PostVals, <<>>),
    DeviceVsn = proplists:get_value(<<"dvsn">>, PostVals, <<>>),
    DeviceName = proplists:get_value(<<"dname">>, PostVals, <<>>),
    PublicKey = proplists:get_value(<<"public_key">>, PostVals, <<>>),
    Ip = proplists:get_value(<<"ip">>, PostVals, <<>>),
    Ip2 = case Ip of
        undefined ->
            <<>>;
        _ ->
            Ip
    end,
    Uid2 = integer_to_binary(Uid),
    Status = <<"1">>,
    LoginCount2 = <<"1">>,

    Tb = tablename(),
    Column = <<"(user_id,device_type,device_id,device_vsn,device_name,
        login_count,last_login_ip,last_login_at,status,public_key,created_at)">>,
    Value = <<"('", Uid2/binary, "', '", DeviceType/binary, "', '", DID/binary, "', '", DeviceVsn/binary, "', '",
              DeviceName/binary, "', '", LoginCount2/binary, "', '", Ip2/binary, "', '", Now/binary, "', '"
              , Status/binary, "', '"
              , (ec_cnv:to_binary(PublicKey))/binary, "', '"
              , Now/binary, "')">>,
    imboy_db:insert_into(Tb, Column, Value).
