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
-export([
    count_by_uid/1,
    page/3
]).
-export([list_public_keys/1]).
-export([list_public_keys_by_uids/1]).
-export([count_other_device_keys/2]).
-export([get_public_by_uid/1]).

%% 设备会话管理使用 imboy_syn，无需数据库扩展

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取用户设备表的表名
%% @return 返回用户设备表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_device">>).

% user_device_repo:page(1, 10, 0).
-spec page(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page(Uid, Limit, Offset) ->
    Tb = tablename(),
    Column = <<"device_id, device_name, device_type, last_active_at,device_vsn">>,
    Where = <<" WHERE status = $1 and user_id = $2">>,

    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary,
            " ORDER BY last_active_at desc LIMIT $3 OFFSET $4">>,
    % ?DEBUG_LOG([Sql, Uid, Limit, Offset]),
    elib_pg:query(Sql, [1, Uid, Limit, Offset]).

-spec list_public_keys(integer()) -> {ok, list(map())} | {error, any()}.
list_public_keys(Uid) ->
    Tb = tablename(),
    Column = <<"device_id, device_type, public_key, key_id, last_active_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE status = 1 AND user_id = $1 AND public_key IS NOT NULL AND public_key <> ''",
            " ORDER BY last_active_at desc">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 统计当前用户除指定设备外、已上报有效公钥的活跃设备数量
%% 用于区分"换设备/重装"与"全新注册首次登录"两种场景：
%% 仅当存在其他活跃设备（说明历史消息可能由其他设备加密）时，
%% 客户端才需要显示 E2EE 恢复横幅。
%% @param Uid 用户ID
%% @param DeviceId 当前上报的设备ID（排除自身）
%% @return non_neg_integer()
-spec count_other_device_keys(integer(), binary()) -> non_neg_integer().
count_other_device_keys(Uid, DeviceId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT count(*) AS count FROM ", Tb/binary,
            " WHERE status = 1 AND user_id = $1 AND device_id <> $2",
            " AND public_key IS NOT NULL AND public_key <> ''">>,
    %% elib_pg:query/2 返回二元组 {ok, [map()]}（已 rows_to_maps），
    %% 此前用三元组 {ok, _, [...]} 匹配恒失败 → 静默返回 0，
    %% 导致"换设备/重装"检测永久失效（E2EE 恢复横幅永不显示）。
    case elib_pg:query(Sql, [Uid, DeviceId]) of
        {ok, [#{<<"count">> := Count}]} when is_integer(Count) ->
            Count;
        _ ->
            0
    end.

-spec list_public_keys_by_uids([integer()]) -> {ok, list(map())} | {error, any()}.
list_public_keys_by_uids([]) ->
    {ok, []};
list_public_keys_by_uids(Uids) when is_list(Uids) ->
    Tb = tablename(),
    Column = <<"user_id, device_id, device_type, public_key, key_id, last_active_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE status = 1 AND user_id = ANY($1)",
            " AND public_key IS NOT NULL AND public_key <> ''",
            " ORDER BY user_id asc, last_active_at desc">>,
    elib_pg:query(Sql, [Uids]).

% user_device_repo:count_by_uid(1).
-spec count_by_uid(integer()) -> non_neg_integer().
count_by_uid(Uid) ->
    % 使用安全的参数化查询，避免SQL注入
    case elib_pg:pluck(tablename(), <<"count(*) as count">>, #{status => 1, user_id => Uid}, #{}) of
        {ok, Count} when is_integer(Count) -> Count;
        _ -> 0
    end.

% user_device_repo:device_name(1, <<"3f039a2b4724a5b7">>).
% user_device_repo:device_name(1, <<"HUAWEIMRD-AL00">>).
-spec device_name(integer(), binary()) -> binary().
device_name(Uid, DID) ->
    % 使用安全的参数化查询，避免SQL注入
    case
        elib_pg:pluck(
            tablename(), <<"device_name">>, #{status => 1, user_id => Uid, device_id => DID}, #{}
        )
    of
        {ok, DeviceName} when is_binary(DeviceName) -> DeviceName;
        _ -> <<>>
    end.

% user_device_repo:login_count(1, <<"872619BD-8FCD-45AF-B255-406D70C4D9C9">>).
-spec login_count(Uid :: integer(), DID :: binary()) -> integer().
login_count(Uid, DID) ->
    % 使用安全的参数化查询，避免SQL注入
    case
        elib_pg:pluck(
            tablename(), <<"login_count">>, #{status => 1, user_id => Uid, device_id => DID}, #{}
        )
    of
        {ok, LoginCount} when is_integer(LoginCount) -> LoginCount;
        _ -> 0
    end.

-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 1 AND user_id = $1 AND device_id = $2">>,
    _ = elib_pg:execute(Sql, [Uid, DID]),
    ok.

% user_device_repo:save(1, 1, <<"3f039a2b4724a5b7">>, [{<<"ip">>, <<"127.0.0.1">>}]).
-spec save(binary() | integer(), integer(), binary(), map()) -> {ok, term()} | {error, term()}.
save(Now, Uid, DID, PostVals) when is_binary(DID), bit_size(DID) > 0 ->
    % 调用之前判断一次 DID不为空，可以减少一个数据库count查询
    LoginCount = user_device_repo:login_count(Uid, DID),
    % ?DEBUG_LOG(["login save ", Now, Uid, DID, LoginCount]),
    save(Now, Uid, PostVals, DID, LoginCount);
save(_Now, _Uid, _DID, _PostVals) ->
    % 无设备ID登录，无需记录设备信息
    {ok, 0}.

% user_device_repo:update_by_did(1, <<"3f039a2b4724a5b7">>, <<"device_name = $1">>, [<<"CLT-AL00 1">>]).
-spec update_by_did(integer(), binary(), binary(), list()) -> {ok, integer()} | {error, any()}.
update_by_did(Uid, DID, Set, SetArgs) ->
    Tb = tablename(),
    SetArgsLen = length(SetArgs),
    SetArgsLen2 = integer_to_binary(SetArgsLen + 1),
    SetArgsLen3 = integer_to_binary(SetArgsLen + 2),
    % 更新登录次数，最近登录时间、IP
    Sql =
        <<"UPDATE ", Tb/binary, " SET ", Set/binary, " WHERE status = 1 AND user_id = $",
            SetArgsLen2/binary, " AND device_id = $", SetArgsLen3/binary>>,
    SetArgs2 = SetArgs ++ [Uid, DID],
    elib_pg:execute(Sql, SetArgs2).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 获取用户的所有设备（包含公钥和私钥）
%% @param Uid 用户ID
%% @return {ok, [map()]} 设备列表
-spec get_public_by_uid(integer()) -> {ok, [map()]} | {error, term()}.
get_public_by_uid(Uid) ->
    Tb = tablename(),
    %% 私钥永不落库（user_device 表无 private_key 列）：仅返回公钥相关字段。
    Column = <<"device_id, device_type, public_key, key_id, last_active_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE status = 1 AND user_id = $1 "
            " ORDER BY last_active_at desc">>,
    elib_pg:query(Sql, [Uid]).

-spec save(binary() | integer(), integer(), map(), binary(), integer()) ->
    {ok, term()} | {error, term()}.
save(Now, Uid, PostVals, DID, LoginCount) when bit_size(DID) > 0, LoginCount > 0 ->
    % 更新登录次数，最近登录时间、IP
    Ip = maps:get(<<"ip">>, PostVals, <<>>),
    PublicKey = maps:get(<<"public_key">>, PostVals, <<>>),
    Tb = tablename(),
    Ip2 =
        case Ip of
            undefined ->
                <<>>;
            _ ->
                Ip
        end,
    % 使用安全的参数化查询，避免SQL注入
    elib_pg:update(
        Tb,
        #{
            login_count => LoginCount + 1,
            last_login_ip => Ip2,
            last_login_at => Now,
            public_key => PublicKey
        },
        <<"status = 1 AND user_id = $1 AND device_id = $2">>,
        [Uid, DID]
    );
save(Now, Uid, PostVals, DID, _LoginCount) when bit_size(DID) > 0 ->
    % 第一次登陆记录设备信息
    DeviceType = maps:get(<<"cos">>, PostVals, <<>>),
    DeviceVsn = maps:get(<<"dvsn">>, PostVals, <<>>),
    DeviceName = maps:get(<<"dname">>, PostVals, <<>>),
    PublicKey = maps:get(<<"public_key">>, PostVals, <<>>),
    Ip = maps:get(<<"ip">>, PostVals, <<>>),

    GenId = elib_tsid:generate(user_device),
    DevData = #{
        %% 预生成 TSID
        <<"id">> => GenId,
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
    },
    {Sql, Params} = elib_pg_sql:insert(tablename(), DevData),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, GenId};
        {error, _} = Err -> Err
    end.
