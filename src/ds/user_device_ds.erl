-module(user_device_ds).
%%%
% user_device_ds 是用户设备数据服务层
% 封装用户设备的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([save/4]).
-export([list/1]).
-export([login_count/2]).
-export([device_name/2]).
-export([delete/2]).
-export([is_active/2]).
-export([update_by_did/4]).
-export([count_by_uid/1, page/3]).
-export([list_public_keys/1]).
-export([list_public_keys_by_uids/1]).
-export([count_other_device_keys/2]).
-export([get_default_device/1]).
-export([update_public_key/5]).
%% G3 thin wrappers: e2ee handlers 不应直调 user_device_repo
-export([get_public_by_uid/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 获取用户设备列表
%% @param Uid 用户ID
%% @return {ok, list(map())} | {error, any()}
-spec list(integer()) -> {ok, list(map())} | {error, any()}.
list(Uid) ->
    Limit = erlang:max(count_by_uid(Uid), 1) + 10,
    case page(Uid, Limit, 0) of
        {ok, Rows} ->
            {ok, [normalize_compat_device(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取用户设备列表
%% @param Uid 用户ID
%% @param Limit 每页数量
%% @param Offset 偏移量
%% @return {ok, list(map())} | {error, any()}
-spec page(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page(Uid, Limit, Offset) ->
    user_device_repo:page(Uid, Limit, Offset).

%% @doc 获取用户公钥列表
%% @param Uid 用户ID
%% @return {ok, list(map())} | {error, any()}
-spec list_public_keys(integer()) -> {ok, list(map())} | {error, any()}.
list_public_keys(Uid) ->
    user_device_repo:list_public_keys(Uid).

%% @doc 批量获取用户公钥列表
%% @param Uids 用户ID列表
%% @return {ok, list(map())} | {error, any()}
-spec list_public_keys_by_uids([integer()]) -> {ok, list(map())} | {error, any()}.
list_public_keys_by_uids(Uids) ->
    user_device_repo:list_public_keys_by_uids(Uids).

%% @doc 统计当前用户除指定设备外、已上报有效公钥的活跃设备数量
%% @param Uid 用户ID
%% @param DeviceId 当前设备ID（排除自身）
%% @return non_neg_integer()
-spec count_other_device_keys(integer(), binary()) -> non_neg_integer().
count_other_device_keys(Uid, DeviceId) ->
    user_device_repo:count_other_device_keys(Uid, DeviceId).

%% @doc 统计用户设备数量
%% @param Uid 用户ID
%% @return non_neg_integer()
-spec count_by_uid(integer()) -> non_neg_integer().
count_by_uid(Uid) ->
    user_device_repo:count_by_uid(Uid).

%% @doc 获取设备名称
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return binary()
-spec device_name(integer(), binary()) -> binary().
device_name(Uid, DID) ->
    user_device_repo:device_name(Uid, DID).

%% @doc 获取登录次数
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return integer()
-spec login_count(integer(), binary()) -> integer().
login_count(Uid, DID) ->
    user_device_repo:login_count(Uid, DID).

%% @doc 保存设备信息
%% @param Now 当前时间
%% @param Uid 用户ID
%% @param DID 设备ID
%% @param PostVals 请求参数
%% @return {ok, term()} | {error, any()}
-spec save(binary() | integer(), integer(), binary(), map()) -> {ok, term()} | {error, any()}.
save(Now, Uid, DID, PostVals) ->
    user_device_repo:save(Now, Uid, DID, PostVals).

%% @doc 删除设备
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return ok
-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    %% 顺序不可颠倒：先删设备行（= token 吊销，安全关键），再清 Olm 材料。
    %% 反过来若删行失败，会变成"密钥没了但 token 还有效"的最坏组合。
    ok = user_device_repo:delete(Uid, DID),
    ok = cleanup_olm_material(Uid, DID),
    ok.

%% @doc 吊销级联：清掉该设备的 Olm 身份键/一次性键/fallback 键。
%%
%% 不清的后果是**吊销对 E2EE 不生效**——已吊销设备仍会被 list_devices_with_identity
%% 列为收件人（扇出继续向死设备加密），其 one-time key 仍可被 claim。
%%
%% 清理失败**不回滚、不阻断**：设备行已删 = token 已吊销，这是安全关键部分且已完成。
%% 此时中止只会让调用方以为吊销失败而重试，反而更糟。但必须 ERROR 级别记下来——
%% 残留的 Olm 材料会持续造成不可解密的密文，是需要人工介入的状态，不能静默。
-spec cleanup_olm_material(integer(), binary()) -> ok.
cleanup_olm_material(Uid, DID) ->
    try olm_identity_repo:delete_by_device(Uid, DID) of
        {ok, 0} ->
            ok;
        {ok, Cnt} ->
            ok = ?INFO_LOG({device_olm_material_purged, Uid, DID, Cnt}),
            ok;
        {error, Reason} ->
            ok = ?ERROR_LOG({device_olm_cleanup_failed, Uid, DID, Reason}),
            ok
    catch
        Class:CatchReason ->
            ok = ?ERROR_LOG({device_olm_cleanup_crashed, Uid, DID, Class, CatchReason}),
            ok
    end.

%% @doc 设备是否仍活跃（未被移除）——token 吊销的唯一判据
%% 缓存 60s：吊销延迟上限 60s；删除设备时 user_device_logic:delete/2 会
%% 主动 flush 并跨节点广播，正常路径是即时生效。
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return boolean()
-spec is_active(integer(), binary()) -> boolean().
is_active(Uid, DID) ->
    Key = {user_device_active, Uid, DID},
    Fun = fun() -> user_device_repo:is_active(Uid, DID) end,
    case imboy_cache:memo(Fun, Key, 60) of
        {ok, Active} when is_boolean(Active) ->
            Active;
        Other ->
            %% ponytail: DB 查不出来时 fail-open（放行），只有"确定查无此行"才判吊销。
            %% 否则一次 DB 抖动会把所有 did 绑定 token 的用户全端踢下线。
            %% 上限：DB 故障期间已被移除的设备仍能访问（最长 60s 缓存 TTL）。
            %% 升级触发：需要强一致吊销时改 fail-closed + 熔断降级。
            ok = ?WARN_LOG({user_device_is_active_unavailable, Uid, DID, Other}),
            true
    end.

%% @doc 根据设备ID更新设备信息
%% @param Uid 用户ID
%% @param DID 设备ID
%% @param Set SET 子句
%% @param SetArgs 参数
%% @return {ok, integer()} | {error, any()}
-spec update_by_did(integer(), binary(), binary(), list()) -> {ok, integer()} | {error, any()}.
update_by_did(Uid, DID, Set, SetArgs) ->
    user_device_repo:update_by_did(Uid, DID, Set, SetArgs).

%% @doc 获取用户的默认设备（最近活跃的设备）
%% @param Uid 用户ID
%% @return {ok, DeviceMap} | {error, Reason}
-spec get_default_device(integer()) -> {ok, map()} | {error, term()}.
get_default_device(Uid) ->
    case user_device_repo:list_public_keys(Uid) of
        {ok, [Device | _]} ->
            {ok, Device};
        {ok, []} ->
            {error, no_device_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新设备的 E2EE 公钥和密钥ID
%% 当用户重新生成密钥对时调用此函数更新数据库记录
%% @param Uid 用户ID
%% @param DeviceId 设备ID
%% @param PublicKey PEM格式的公钥
%% @param KeyId 密钥ID
%% @param Now 当前时间戳
%% @return {ok, UpdatedCount} | {error, Reason}
-spec update_public_key(integer(), binary(), binary(), binary(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
update_public_key(Uid, DeviceId, PublicKey, KeyId, Now) ->
    Tb = user_device_repo:tablename(),
    % 同时更新 public_key, key_id, last_active_at
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET public_key = $1, key_id = $2, last_active_at = $3"
            " WHERE status = 1 AND user_id = $4 AND device_id = $5">>,
    elib_pg:execute(Sql, [PublicKey, KeyId, Now, Uid, DeviceId]).

%% ===================================================================
%% Internal Functions
%% ===================================================================

normalize_compat_device(Row) ->
    Row#{
        <<"did">> => maps:get(<<"device_id">>, Row, <<>>),
        <<"dtype">> => maps:get(<<"device_type">>, Row, <<>>),
        <<"dname">> => maps:get(<<"device_name">>, Row, <<>>)
    }.

%% G3 thin wrappers: e2ee handlers 不应直调 user_device_repo
-spec get_public_by_uid(integer()) -> {ok, [map()]} | {error, term()}.
get_public_by_uid(Uid) -> user_device_repo:get_public_by_uid(Uid).
