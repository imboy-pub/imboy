-module(repo_user_device).
%%%
% repo_user_device 是 user_device repository 缩写
%%%

-export([save/4]).
-export([login_count/2]).


-spec login_count(UID :: binary(), DID :: binary()) -> integer().
% repo_user_device:login_count(1, <<"3f039a2b4724a5b7">>).
login_count(UID, DID) ->
    Sql = <<"SELECT `login_count` FROM `user_device`
        WHERE `status` = ? AND `user_id` = ? AND `device_id` = ?">>,
    case mysql_pool:query(Sql, [1, UID, DID]) of
        {ok, _FieldList, [[LoginCount]]}
          when is_integer(LoginCount), LoginCount > 0 ->
            LoginCount;
        _ ->
            0
    end.


-spec save(Now :: integer(),
           UID :: integer(),
           DID :: binary(),
           PostVals :: list()) -> ok.
save(Now, UID, DID, PostVals) when is_binary(DID), bit_size(DID) > 0 ->
    % 调用之前判断一次 DID不为空，可以减少一个数据库count查询
    LoginCount = repo_user_device:login_count(UID, DID),
    save(Now, UID, PostVals, DID, LoginCount);
save(_Now, _UID, _DID, _PostVals) ->
    % 无设备ID登录，无需记录设备信息
    ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec save(Now :: integer(),
           UID :: integer(),
           PostVals :: list(),
           DID :: binary(),
           LoginCount :: integer()) -> ok.
save(Now, UID, PostVals, DID, LoginCount)
  when bit_size(DID) > 0, LoginCount > 0 ->
    % 更新登录次数，最近登录时间、IP
    Ip = proplists:get_value(<<"ip">>, PostVals, <<"">>),
    Sql = <<"UPDATE `user_device` SET `login_count` = ?,
        `last_login_ip` = ?,`last_login_at` = ?
        WHERE `status` = ? AND `user_id` = ? AND `device_id` = ?">>,
    mysql_pool:execute(Sql,
                       [LoginCount + 1, Ip, Now, <<"1">>, UID, DID]);
save(Now, UID, PostVals, DID, _LoginCount) when bit_size(DID) > 0 ->
    % 第一次登陆记录设备信息
    DeviceType = proplists:get_value(<<"cos">>, PostVals, <<"">>),
    DeviceVsn = proplists:get_value(<<"dvsn">>, PostVals, <<"">>),
    DeviceName = proplists:get_value(<<"dname">>, PostVals, <<"">>),
    Ip = proplists:get_value(<<"ip">>, PostVals, <<"">>),

    UID2 = integer_to_binary(UID),
    Status = <<"1">>,
    LoginCount2 = <<"1">>,
    Now2 = integer_to_binary(Now),

    Table = <<"`user_device`">>,
    Column = <<"(`user_id`,`device_type`,`device_id`,`device_vsn`,`device_name`,
        `login_count`,`last_login_ip`,`last_login_at`,`status`,`created_at`)">>,
    Value = <<"('", UID2/binary, "', '", DeviceType/binary, "', '",
              DID/binary, "', '", DeviceVsn/binary, "', '",
              DeviceName/binary, "', '", LoginCount2/binary, "', '",
              Ip/binary, "', '", Now2/binary, "', '", Status/binary,
              "', '", Now2/binary, "')">>,
    mysql_pool:replace_into(Table, Column, Value).
