-module(e2ee_local_backup_repo).
%%%===================================================================
%%% @doc
%%% e2ee_local_backup_repo - E2EE 本地备份数据仓库层
%%%
%%% 功能：
%%% - 备份元数据记录（不包含私钥）
%%% - 备份历史查询
%%% - 备份版本管理
%%%
%%% 安全说明：
%%% - 服务器仅记录备份元数据（checksum, device_id, created_at）
%%% - 实际备份文件（.enc）完全由客户端控制
%%% - 服务器永远不接触用户的私钥或备份密码
%%%
%%% 使用示例：
%%% ```
%%% % 创建备份元数据记录
%%% {ok, Backup} = e2ee_local_backup_repo:create(#{
%%%     uid => 10001,
%%%     device_id => <<"device-001">>,
%%%     backup_version => 1,
%%%     key_checksum => <<"sha256:abc123...">>
%%% }).
%%% ```
%%%===================================================================

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([create/1]).
-export([find_latest/1]).
-export([list_by_uid/1]).
-export([delete/1]).
-export([delete_by_id_and_uid/2]).

%% 类型定义
-type backup() :: #{
    id => integer(),
    uid => integer(),
    device_id => binary(),
    backup_version => integer(),
    key_checksum => binary(),
    file_size => integer(),
    user_notes => binary(),
    created_at => binary()
}.
-type repo_result() :: {ok, backup()} | {error, term()}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 创建备份元数据记录
%%
%% @param BackupMap 备份元数据 Map
%%   - uid: 用户 ID（必需）
%%   - device_id: 设备 ID（必需）
%%   - backup_version: 备份版本号（必需）
%%   - key_checksum: SHA-256 校验和（必需）
%%   - file_size: 文件大小（可选，bytes）
%%   - user_notes: 用户备注（可选）
%%
%% @returns {ok, Backup} | {error, Reason}
%%
%% @example
%%% ```
%%% {ok, Backup} = e2ee_local_backup_repo:create(#{
%%%     uid => 10001,
%%%     device_id => <<"device-001">>,
%%%     backup_version => 1,
%%%     key_checksum => <<"sha256:abc123...">>,
%%%     file_size => 2048,
%%%     user_notes => <<"主手机备份 - 2026年1月"/utf8>>
%%% }).
%%% ```
-spec create(map()) -> repo_result().
create(BackupMap) ->
    Uid = maps:get(<<"uid">>, BackupMap),
    _DeviceId = maps:get(<<"device_id">>, BackupMap),
    BackupVersion = maps:get(<<"backup_version">>, BackupMap),
    _KeyChecksum = maps:get(<<"key_checksum">>, BackupMap),
    _FileSize = maps:get(<<"file_size">>, BackupMap, 0),
    _UserNotes = maps:get(<<"user_notes">>, BackupMap, <<>>),

    % 检查是否已存在相同版本的备份
    case find_by_version(Uid, BackupVersion) of
        {ok, _Existing} ->
            % 版本已存在，直接返回已有记录
            find_by_version(Uid, BackupVersion);
        {error, not_found} ->
            % 创建新记录
            insert_new(BackupMap)
    end.

%% @doc 查找用户的最新备份记录
%% @param Uid 用户 ID
%% @returns {ok, Backup} | {error, not_found}
-spec find_latest(integer()) -> repo_result().
find_latest(Uid) ->
    Sql = <<
        "SELECT id, uid, device_id, backup_version, key_checksum,\n"
        "                    file_size, user_notes, created_at\n"
        "             FROM e2ee_local_backups\n"
        "             WHERE uid = $1\n"
        "             ORDER BY backup_version DESC, created_at DESC\n"
        "             LIMIT 1"
    >>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, [Result | _]} ->
            {ok, row_to_map(Result)};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_local_backup_repo, find_latest_failed, Reason]),
            {error, Reason}
    end.

%% @doc 根据版本号查找备份记录
%% @param Uid 用户 ID
%% @param Version 备份版本号
%% @returns {ok, Backup} | {error, not_found}
-spec find_by_version(integer(), integer()) -> repo_result().
find_by_version(Uid, Version) ->
    Sql = <<
        "SELECT id, uid, device_id, backup_version, key_checksum,\n"
        "                    file_size, user_notes, created_at\n"
        "             FROM e2ee_local_backups\n"
        "             WHERE uid = $1 AND backup_version = $2"
    >>,

    case elib_pg:query(Sql, [Uid, Version]) of
        {ok, [Result | _]} ->
            {ok, row_to_map(Result)};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出用户的所有备份记录（按版本倒序）
%% @param Uid 用户 ID
%% @returns {ok, [Backup]} | {error, Reason}
-spec list_by_uid(integer()) -> {ok, [backup()]} | {error, term()}.
list_by_uid(Uid) ->
    Sql = <<
        "SELECT id, uid, device_id, backup_version, key_checksum,\n"
        "                    file_size, user_notes, created_at\n"
        "             FROM e2ee_local_backups\n"
        "             WHERE uid = $1\n"
        "             ORDER BY backup_version DESC, created_at DESC"
    >>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, Results} ->
            {ok, lists:map(fun row_to_map/1, Results)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除备份记录
%% @param BackupId 备份记录 ID
%% @returns ok | {error, Reason}
-spec delete(integer()) -> ok | {error, term()}.
delete(BackupId) ->
    Sql = <<"DELETE FROM e2ee_local_backups WHERE id = $1">>,

    case elib_pg:execute(Sql, [BackupId]) of
        {ok, _} ->
            ?INFO_LOG([e2ee_local_backup_repo, backup_deleted, BackupId]),
            ok;
        {error, Reason} ->
            ?ERROR_LOG([e2ee_local_backup_repo, delete_failed, Reason]),
            {error, Reason}
    end.

%% @doc 安全删除备份记录（同时验证所有权）
%% @param BackupId 备份记录 ID
%% @param Uid 当前用户 ID（只能删除自己的备份）
%% @returns ok | {error, not_found} | {error, Reason}
-spec delete_by_id_and_uid(integer(), integer()) -> ok | {error, term()}.
delete_by_id_and_uid(BackupId, Uid) ->
    Sql = <<"DELETE FROM e2ee_local_backups WHERE id = $1 AND uid = $2">>,

    case elib_pg:execute(Sql, [BackupId, Uid]) of
        {ok, 1} ->
            ?INFO_LOG([e2ee_local_backup_repo, backup_deleted_by_owner, BackupId, Uid]),
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_local_backup_repo, delete_failed, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
insert_new(BackupMap) ->
    Uid = maps:get(<<"uid">>, BackupMap),
    DeviceId = maps:get(<<"device_id">>, BackupMap),
    BackupVersion = maps:get(<<"backup_version">>, BackupMap),
    KeyChecksum = maps:get(<<"key_checksum">>, BackupMap),
    FileSize = maps:get(<<"file_size">>, BackupMap, 0),
    UserNotes = maps:get(<<"user_notes">>, BackupMap, <<>>),

    Id = elib_tsid:generate(e2ee_local_backup),
    Sql = <<
        "INSERT INTO e2ee_local_backups (\n"
        "                id, uid, device_id, backup_version, key_checksum,\n"
        "                file_size, user_notes\n"
        "            ) VALUES ($1, $2, $3, $4, $5, $6, $7)\n"
        "            RETURNING id, uid, device_id, backup_version, key_checksum,\n"
        "                      file_size, user_notes, created_at"
    >>,

    case
        elib_pg:query(Sql, [
            Id,
            Uid,
            DeviceId,
            BackupVersion,
            KeyChecksum,
            FileSize,
            UserNotes
        ])
    of
        {ok, [Result | _]} ->
            ?INFO_LOG([e2ee_local_backup_repo, backup_created, Uid, DeviceId, BackupVersion]),
            {ok, row_to_map(Result)};
        {ok, []} ->
            ?ERROR_LOG([e2ee_local_backup_repo, create_failed, empty_result]),
            {error, empty_result};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_local_backup_repo, create_failed, Reason]),
            {error, Reason}
    end.

%% @private
row_to_map(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        uid => maps:get(<<"uid">>, Row),
        device_id => maps:get(<<"device_id">>, Row),
        backup_version => maps:get(<<"backup_version">>, Row),
        key_checksum => maps:get(<<"key_checksum">>, Row),
        file_size => maps:get(<<"file_size">>, Row, 0),
        user_notes => maps:get(<<"user_notes">>, Row, <<>>),
        created_at => maps:get(<<"created_at">>, Row)
    }.
