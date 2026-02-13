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
-export([find_by_version/2]).
-export([list_by_uid/1]).
-export([count_by_uid/1]).
-export([update_file_size/2]).
-export([delete/1]).

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
            % 更新现有记录
            update_existing(BackupMap);
        {error, not_found} ->
            % 创建新记录
            insert_new(BackupMap)
    end.

%% @doc 查找用户的最新备份记录
%% @param Uid 用户 ID
%% @returns {ok, Backup} | {error, not_found}
-spec find_latest(integer()) -> repo_result().
find_latest(Uid) ->
    Sql = <<"SELECT id, uid, device_id, backup_version, key_checksum,
                    file_size, user_notes, created_at
             FROM e2ee_local_backups
             WHERE uid = $1
             ORDER BY backup_version DESC, created_at DESC
             LIMIT 1">>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, _, [Result]} ->
            {ok, row_to_map(Result)};
        {ok, _, []} ->
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
    Sql = <<"SELECT id, uid, device_id, backup_version, key_checksum,
                    file_size, user_notes, created_at
             FROM e2ee_local_backups
             WHERE uid = $1 AND backup_version = $2">>,

    case elib_pg:query(Sql, [Uid, Version]) of
        {ok, _, [Result]} ->
            {ok, row_to_map(Result)};
        {ok, _, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出用户的所有备份记录（按版本倒序）
%% @param Uid 用户 ID
%% @returns {ok, [Backup]} | {error, Reason}
-spec list_by_uid(integer()) -> {ok, [backup()]} | {error, term()}.
list_by_uid(Uid) ->
    Sql = <<"SELECT id, uid, device_id, backup_version, key_checksum,
                    file_size, user_notes, created_at
             FROM e2ee_local_backups
             WHERE uid = $1
             ORDER BY backup_version DESC, created_at DESC">>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, _, Results} ->
            {ok, lists:map(fun row_to_map/1, Results)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 统计用户的备份数量
%% @param Uid 用户 ID
%% @returns {ok, Count} | {error, Reason}
-spec count_by_uid(integer()) -> {ok, non_neg_integer()} | {error, term()}.
count_by_uid(Uid) ->
    Sql = <<"SELECT COUNT(*) as count
             FROM e2ee_local_backups
             WHERE uid = $1">>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, _, [{Result}]} ->
            {ok, maps:get(<<"count">>, Result)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新备份文件大小
%% @param Uid 用户 ID
%% @param FileSize 文件大小（bytes）
%% @returns ok | {error, Reason}
-spec update_file_size(integer(), integer()) -> ok | {error, term()}.
update_file_size(Uid, FileSize) ->
    Sql = <<"UPDATE e2ee_local_backups
             SET file_size = $1
             WHERE uid = $2">>,

    case elib_pg:query(Sql, [FileSize, Uid]) of
        {ok, _, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除备份记录
%% @param BackupId 备份记录 ID
%% @returns ok | {error, Reason}
-spec delete(integer()) -> ok | {error, term()}.
delete(BackupId) ->
    Sql = <<"DELETE FROM e2ee_local_backups WHERE id = $1">>,

    case elib_pg:query(Sql, [BackupId]) of
        {ok, _, _} ->
            ?INFO_LOG([e2ee_local_backup_repo, backup_deleted, BackupId]),
            ok;
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

    Sql = <<"INSERT INTO e2ee_local_backups (
                uid, device_id, backup_version, key_checksum,
                file_size, user_notes
            ) VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING id, uid, device_id, backup_version, key_checksum,
                      file_size, user_notes, created_at">>,

    case elib_pg:query(Sql, [Uid, DeviceId, BackupVersion, KeyChecksum,
                            FileSize, UserNotes]) of
        {ok, _, [{Result}]} ->
            ?INFO_LOG([e2ee_local_backup_repo, backup_created, Uid, DeviceId, BackupVersion]),
            {ok, row_to_map(Result)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_local_backup_repo, create_failed, Reason]),
            {error, Reason}
    end.

%% @private
update_existing(BackupMap) ->
    Uid = maps:get(<<"uid">>, BackupMap),
    BackupVersion = maps:get(<<"backup_version">>, BackupMap),
    KeyChecksum = maps:get(<<"key_checksum">>, BackupMap),
    FileSize = maps:get(<<"file_size">>, BackupMap, 0),
    UserNotes = maps:get(<<"user_notes">>, BackupMap, <<>>),

    Sql = <<"UPDATE e2ee_local_backups
             SET key_checksum = $1, file_size = $2, user_notes = $3
             WHERE uid = $4 AND backup_version = $5
             RETURNING id, uid, device_id, backup_version, key_checksum,
                      file_size, user_notes, created_at">>,

    case elib_pg:query(Sql, [KeyChecksum, FileSize, UserNotes, Uid, BackupVersion]) of
        {ok, _, [{Result}]} ->
            ?INFO_LOG([e2ee_local_backup_repo, backup_updated, Uid, BackupVersion]),
            {ok, row_to_map(Result)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_local_backup_repo, update_failed, Reason]),
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
