-module(push_token_repo).
%%%
% push_token_repo 是 push_token repository 缩写
% 推送 Token 数据仓库层，提供推送 token 的基础数据库操作
%%%

-export([tablename/0]).
-export([upsert/5]).
-export([deactivate/2]).
-export([deactivate_by_token/1]).
-export([deactivate_inactive/1]).
-export([list_by_uid/1]).
-export([list_by_uids/1]).
-export([list_page/2]).
-export([delete_by_uid/1]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"push_token">>).

%% @doc 注册或更新推送 token（upsert）
%%
%% 接管式绑定（FULL-06，plan-full §7「token 跨用户/设备不可复用」）：
%% 一个推送 token（FCM token / JPush RegistrationID）在生产上唯一对应一台
%% 物理设备，因此**同一 token 同一时刻只能有一个活跃主人**。旧实现在断电
%% 条件里只用了 (user_id, device_id)，于是「同一个 token 换了主人」会漏网：
%%   * 换主人：user B 在同一台机器上登录（device_id 与 A 不同），A 的旧行仍
%%     是 status=1 → 此后投给 A 的推送被投到「已登成 B」的设备上（跨用户投递）；
%%   * 换设备：重装后 device_id 变了而 RegistrationID 未变，同样留下两条活跃
%%     同 token 行（同一设备重复推送）。
%% 修法：断电条件加上 token 维度——先按 token 把**任何**旧活跃行置为无效
%% （含换主人/换设备），再插入新行。DB 层由迁移 00000142 的
%% uq_push_token_active_token（部分唯一索引）兜底，防并发竞态与新写入方绕过。
-spec upsert(integer(), binary(), binary(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
upsert(Uid, DeviceId, DeviceType, Platform, Token) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    %% 先将该设备旧 token，以及该 token 在任何用户/设备上的旧活跃行置为无效
    DeactivateSql =
        <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE status = 1"
            " AND (token = $2 OR (user_id = $3 AND device_id = $4))">>,
    _ = elib_pg:execute(DeactivateSql, [Now, Token, Uid, DeviceId]),
    %% 插入新 token
    Id = elib_tsid:generate(push_token),
    Data = #{
        <<"id">> => Id,
        <<"user_id">> => Uid,
        <<"device_id">> => DeviceId,
        <<"device_type">> => DeviceType,
        <<"platform">> => Platform,
        <<"token">> => Token,
        <<"status">> => 1,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    {Sql, Params} = elib_pg_sql:insert(Tb, Data),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 使指定用户设备的 token 失效
-spec deactivate(integer(), binary()) -> {ok, integer()} | {error, term()}.
deactivate(Uid, DeviceId) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE user_id = $2 AND device_id = $3 AND status = 1">>,
    elib_pg:execute(Sql, [Now, Uid, DeviceId]).

%% @doc 按 token 值使 token 失效（token 过期/无效时调用）
-spec deactivate_by_token(binary()) -> {ok, integer()} | {error, term()}.
deactivate_by_token(Token) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE token = $2 AND status = 1">>,
    elib_pg:execute(Sql, [Now, Token]).

%% @doc 将超过指定天数未更新的活跃 Token 置为失效
%% 用于定期清理不活跃的推送 Token，避免向失效设备发送推送
-spec deactivate_inactive(pos_integer()) -> {ok, integer()} | {error, term()}.
deactivate_inactive(InactiveDays) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Cutoff = elib_dt:minus(Now, {InactiveDays * 86400000, millisecond}),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE status = 1 AND updated_at < $2">>,
    elib_pg:execute(Sql, [Now, Cutoff]).

%% @doc 查询用户所有活跃推送 token
-spec list_by_uid(integer()) -> {ok, list()} | {error, term()}.
list_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<
        "SELECT device_id, device_type, platform, token"
        " FROM ",
        Tb/binary,
        " WHERE user_id = $1 AND status = 1"
    >>,
    elib_pg:query(Sql, [Uid]).

%% @doc 批量查询多个用户的活跃推送 token
-spec list_by_uids([integer()]) -> {ok, list()} | {error, term()}.
list_by_uids([]) ->
    {ok, []};
list_by_uids(Uids) when is_list(Uids) ->
    Tb = tablename(),
    Sql = <<
        "SELECT user_id, device_id, device_type, platform, token"
        " FROM ",
        Tb/binary,
        " WHERE user_id = ANY($1) AND status = 1"
    >>,
    elib_pg:query(Sql, [Uids]).

%% @doc 分页查询推送 token（Admin 管理用）
%%
%% 隐私不变量（plan-full §7「无 secret hydration」）：**投影里不出现 token 明文**。
%% 推送 token 是设备凭据——拿到即可向该设备推任意通知，因此 Admin 读面只给
%% **不可逆指纹**：`md5(token)` 十六进制前 8 位 + `length(token)`（原字节长度）。
%% 指纹在 **SQL 侧**计算：明文根本不进入应用进程，更不进响应体——不是
%% 「取回来再删字段」那种一改响应序列化就漏的写法。口径与前端 PushTokenView
%% 一致（同 md5 前 8 位 + 原长度），不另立一套。
%%
%% 边界：本函数只服务 Admin 读面。推送**执行链**走 list_by_uid/1、list_by_uids/1
%% 与 deactivate_by_token/1（各自独立 SQL，仍取 token 明文），三者不受本次改动影响
%% ——已用 `grep -rn "push_token_repo:list_page" src/` 核实只有一个调用方
%% （adm_admin_handler:push_token_list_action/3，权限 settings:view）。
-spec list_page(pos_integer(), pos_integer()) ->
    {ok, #{list := list(), total := integer()}} | {error, term()}.
list_page(Page, Size) ->
    Tb = tablename(),
    CountSql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE status = 1">>,
    case elib_pg:query(CountSql, []) of
        {ok, [#{<<"count">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            DataSql = <<
                "SELECT user_id, device_id, device_type, platform,"
                " lower(substring(md5(token) for 8)) AS token_fingerprint,"
                " length(token) AS token_length,"
                " created_at, updated_at"
                " FROM ",
                Tb/binary,
                " WHERE status = 1"
                " ORDER BY updated_at DESC"
                " LIMIT $1 OFFSET $2"
            >>,
            case elib_pg:query(DataSql, [Size, Offset]) of
                {ok, Rows} ->
                    {ok, #{list => Rows, total => Total}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _} ->
            {ok, #{list => [], total => 0}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除用户所有推送 token（账号注销时使用）
-spec delete_by_uid(integer()) -> {ok, integer()} | {error, term()}.
delete_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1">>,
    elib_pg:execute(Sql, [Uid]).
