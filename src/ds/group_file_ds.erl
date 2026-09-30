-module(group_file_ds).
-dialyzer({nowarn_function, [upload_file/5]}).
%%%
% group_file_ds 是 group file domain service 缩写
% 群文件数据服务层，封装群文件业务逻辑和数据访问
%
% T7 归档写守卫（P0 后续批）：上传/删除为内容写——upload_file 的 OSS 上传
% 是对外副作用，无法进 DB 事务，故在动 Garage 之前 ensure_writable 预检
% （归档后零上传），落库在 write_tx 守卫事务内；delete_file 同事务守卫；
% download_file 的下载计数是派生读写（freeze：归档跳过计数，下载读取
% 永不 403）；soft_delete（adm 治理 G3 入口）同事务守卫（{group_file,PK}）。
%%%

-export([upload_file/5]).
-export([download_file/2]).
-export([delete_file/2]).
-export([list_files/4]).
-export([list_files/5]).
-export([search_files/5]).
-export([get_file_categories/1]).
-export([count_by_group/1]).
%% G3 thin wrappers for adm_group_handler
-export([find_by_id/1]).
-export([find_by_file_id/1]).
-export([soft_delete/1]).
-export([search_by_name/4]).
-export([list_by_category/4]).
-export([list_by_group/4]).

-include("cache.hrl").
-include("log.hrl").

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 上传文件到群文件
%% @param Gid 群组ID
%% @param UploaderId 上传者ID
%% @param FileName 文件名
%% @param FileBinary 文件二进制数据
%% @param FileType MIME类型
%% @return {ok, FileId} | {error, Reason}
-spec upload_file(integer(), integer(), binary(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
upload_file(Gid, UploaderId, FileName, FileBinary, FileType) ->
    % 1. 验证群成员身份
    case group_ds:is_member(UploaderId, Gid) of
        false ->
            {error, not_member};
        true ->
            % 2. 验证文件类型
            case elib_oss:validate_file_type(FileType) of
                false ->
                    {error, invalid_file_type};
                true ->
                    % 3. T7 归档写守卫（OSS 前预检）：OSS 上传是对外副作用，无法进
                    %    DB 事务——归档群在动 Garage 之前稳定拒绝（980），
                    %    确保归档后零上传副作用。
                    case workspace_guard:ensure_writable({group, Gid}) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            do_upload_file(
                                Gid, UploaderId, FileName, FileBinary, FileType
                            )
                    end
            end
    end.

%% @doc OSS 上传 + 落库（T7：预检通过后执行；落库在 {group, Gid} 守卫事务内，
%% 预检与落库之间存在归档竞态窗口——落库被拒时 Garage 残留孤儿对象，
%% 无 DB 不一致，可接受）。
-spec do_upload_file(integer(), integer(), binary(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
do_upload_file(Gid, UploaderId, FileName, FileBinary, FileType) ->
    case elib_oss:upload(FileBinary, FileName, #{mime_type => FileType}) of
        {error, file_too_large} ->
            {error, file_too_large};
        {error, invalid_file_type} ->
            {error, invalid_file_type};
        {ok, FileUrl, FileId} ->
            FileHash = erlang:md5(FileBinary),
            FileHashHex = binary:encode_hex(FileHash),
            Category = elib_oss:get_file_category(FileType),
            CategoryBin = atom_to_binary(Category, utf8),
            Now = elib_dt:now(),
            Data = #{
                group_id => Gid,
                file_id => FileId,
                file_name => FileName,
                file_size => byte_size(FileBinary),
                file_type => FileType,
                file_category => CategoryBin,
                file_url => FileUrl,
                file_hash => FileHashHex,
                uploader_id => UploaderId,
                download_count => 0,
                status => 1,
                created_at => Now,
                updated_at => Now
            },
            %% 群文件与附件授权记录必须一起提交；任一失败不报告成功。
            elib_pg:with_tx(fun(Conn) ->
                ok = workspace_guard:abort_on_error(
                    attachment_ds:ensure_upload_scope_tx(Conn, {group, Gid}, UploaderId)
                ),
                case group_file_repo:insert_tx(Conn, Data) of
                    {ok, GroupFileId} ->
                        ok = write_attachment_tx(Conn, Data, GroupFileId),
                        {ok, FileId};
                    {error, Reason} ->
                        throw({abort_tx, Reason})
                end
            end);
        {error, UploadErr} ->
            {error, UploadErr}
    end.

%% @doc 下载文件
%% @param FileId 文件ID（主键）
%% @param CurrentUid 当前用户ID
%% @return {ok, FileUrl} | {error, Reason}
-spec download_file(integer(), integer()) -> {ok, binary()} | {error, term()}.
download_file(FileId, CurrentUid) ->
    % 1. 查询文件信息
    case group_file_repo:find_by_id(FileId) of
        #{<<"id">> := _, <<"group_id">> := Gid, <<"file_url">> := FileUrl} ->
            % 2. 验证群成员身份
            case group_ds:is_member(CurrentUid, Gid) of
                false ->
                    {error, not_member};
                true ->
                    % 3. 增加下载计数（T7 派生读写 freeze：归档时跳过计数，
                    %    下载读取永不 403）
                    spawn(fun() ->
                        _ =
                            workspace_guard:write_tx_or_skip({group, Gid}, fun(Conn) ->
                                group_file_repo:increment_download_tx(Conn, FileId)
                            end),
                        ok
                    end),
                    {ok, FileUrl}
            end;
        _ ->
            {error, not_found}
    end.

%% @doc 删除文件（软删除）
%% @param FileId 文件ID（主键）
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec delete_file(integer(), integer()) -> ok | {error, term()}.
delete_file(FileId, CurrentUid) ->
    % 1. 查询文件信息
    case group_file_repo:find_by_id(FileId) of
        #{<<"id">> := _, <<"group_id">> := Gid, <<"uploader_id">> := UploaderId} ->
            % 2. 验证权限（上传者或管理员）
            case check_delete_permission(CurrentUid, UploaderId, Gid) of
                {ok, true} ->
                    % 3. 软删除文件（T7：{group, Gid} 行锁与写入同事务）
                    case
                        workspace_guard:write_tx({group, Gid}, fun(Conn) ->
                            group_file_repo:soft_delete_tx(Conn, FileId)
                        end)
                    of
                        {ok, _AffectedRows} ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, not_found}
    end.

%% @doc 查询群文件列表
%% @param Gid 群组ID
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, [FileMap]} | {error, Reason}
-spec list_files(integer(), integer(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_files(Gid, CurrentUid, Page, Size) ->
    list_files(Gid, CurrentUid, Page, Size, #{}).

%% @doc 查询群文件列表（带选项）
%% @param Gid 群组ID
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @param Options 选项 #{category => binary()}
%% @return {ok, [FileMap]} | {error, Reason}
-spec list_files(integer(), integer(), integer(), integer(), map()) ->
    {ok, list(map())} | {error, term()}.
list_files(Gid, CurrentUid, Page, Size, Options) ->
    % 1. 验证群成员身份
    case group_ds:is_member(CurrentUid, Gid) of
        false ->
            {error, not_member};
        true ->
            % 2. 查询文件列表
            group_file_repo:list_by_group(Gid, Page, Size, Options)
    end.

%% @doc 搜索群文件
%% @param Gid 群组ID
%% @param Keyword 关键词
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, [FileMap]} | {error, Reason}
-spec search_files(integer(), binary(), integer(), integer(), integer()) ->
    {ok, list(map())} | {error, term()}.
search_files(Gid, Keyword, Page, Size, CurrentUid) ->
    % 1. 验证群成员身份
    case group_ds:is_member(CurrentUid, Gid) of
        false ->
            {error, not_member};
        true ->
            % 2. 搜索文件
            group_file_repo:search_by_name(Gid, Keyword, Page, Size)
    end.

%% @doc 获取群文件分类统计
%% @param Gid 群组ID
%% @return {ok, [{Category, Count, TotalSize}]} | {error, Reason}
-spec get_file_categories(integer()) ->
    {ok, list({binary(), integer(), integer()})} | {error, term()}.
get_file_categories(Gid) ->
    group_file_repo:category_stats(Gid).

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc BUG#137：群文件上传成功后补写 attachment 记录（scope=group）。
%% ObjectKey 与 elib_oss:upload_to_storage/4 完全一致（FileId/basename），
%% attachment.path 即该 key；view_url 读鉴权按 scope=group + scope_ref=Gid
%% 校验群成员后签发 presign GET。
%% 同群文件记录一个事务；失败回滚两条记录，不留下无法授权下载的成功文件。
-spec write_attachment_tx(any(), map(), integer()) -> ok.
write_attachment_tx(
    Conn,
    #{
        group_id := Gid,
        uploader_id := UploaderId,
        file_name := FileName,
        file_id := FileId,
        file_url := FileUrl,
        file_type := FileType,
        file_size := FileSize,
        file_hash := FileHashHex
    },
    GroupFileId
) ->
    SafeName = filename:basename(FileName),
    ObjectKey = <<FileId/binary, "/", SafeName/binary>>,
    Attach = #{
        <<"file_hash256">> => FileHashHex,
        <<"mime_type">> => FileType,
        <<"name">> => SafeName,
        <<"path">> => ObjectKey,
        <<"url">> => FileUrl,
        <<"size">> => FileSize,
        <<"scope">> => <<"group">>,
        <<"scope_ref">> => integer_to_binary(Gid),
        <<"group_file_id">> => GroupFileId
    },
    attachment_ds:save(Conn, elib_dt:now(), UploaderId, [Attach]).

%% @doc 检查删除权限
%% @param CurrentUid 当前用户ID
%% @param UploaderId 上传者ID
%% @param Gid 群组ID
%% @return {ok, true} | {error, Reason}
-spec check_delete_permission(integer(), integer(), integer()) -> {ok, true} | {error, term()}.
check_delete_permission(CurrentUid, UploaderId, _Gid) when CurrentUid =:= UploaderId ->
    {ok, true};
check_delete_permission(CurrentUid, _UploaderId, Gid) ->
    % 检查是否为群主或管理员
    case group_member_repo:find(Gid, CurrentUid, <<"role">>) of
        % 管理员或群主
        #{<<"role">> := Role} when Role >= 3 ->
            {ok, true};
        _ ->
            {error, permission_denied}
    end.

%% G3: group_file_logic 不应直调 group_file_repo
-spec count_by_group(integer()) -> {ok, integer()} | {error, term()}.
count_by_group(Gid) -> group_file_repo:count_by_group(Gid).

%% G3 thin wrappers for adm_group_handler
-spec find_by_id(integer()) -> map().
find_by_id(FileId) -> group_file_repo:find_by_id(FileId).

-spec find_by_file_id(binary()) -> map().
find_by_file_id(FileId) -> group_file_repo:find_by_file_id(FileId).

-spec soft_delete(integer()) -> {ok, integer()} | {error, term()}.
soft_delete(FileId) ->
    %% T7 归档写守卫：{group_file, PK} → group → workspace（adm 治理 G3 入口）
    workspace_guard:write_tx({group_file, FileId}, fun(Conn) ->
        group_file_repo:soft_delete_tx(Conn, FileId)
    end).

-spec search_by_name(integer(), binary(), pos_integer(), pos_integer()) ->
    {ok, list(map())} | {error, term()}.
search_by_name(Gid, Keyword, Page, Size) ->
    group_file_repo:search_by_name(Gid, Keyword, Page, Size).

-spec list_by_category(integer(), binary(), pos_integer(), pos_integer()) ->
    {ok, list(map())} | {error, term()}.
list_by_category(Gid, Category, Page, Size) ->
    group_file_repo:list_by_category(Gid, Category, Page, Size).

-spec list_by_group(integer(), pos_integer(), pos_integer(), map()) ->
    {ok, list(map())} | {error, term()}.
list_by_group(Gid, Page, Size, Options) ->
    group_file_repo:list_by_group(Gid, Page, Size, Options).
