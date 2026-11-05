-module(group_file_ds).
-moduledoc "群文件领域服务 / group file domain service。".
%%%
% group_file_ds 是 group file domain service 缩写
% 群文件数据服务层，封装群文件业务逻辑和数据访问
%
% T7 归档写守卫（P0 后续批）：内容写入口经 workspace_guard 同事务守卫；
% delete_file 同事务守卫；download_file 的下载计数是派生读写（freeze：
% 归档跳过计数，下载读取在有效成员授权后允许归档工作区只读）；
% soft_delete（adm 治理 G3 入口）同事务守卫。
%
% C08（V3-12）：multipart 正文上传（upload_file，"登记即绑定"旧链）已
% 退役；群文件唯一引用入口为 bind_file（direct-v1 confirmed 逻辑引用绑定，
% T27 资格硬校验）。
%%%

-export([bind_file/7]).
-export([download_file/2]).
-export([delete_file/2]).
-export([list_files/4]).
-export([list_files/5]).
-export([search_files/5]).
-export([get_file_categories/2]).
-export([count_by_group/1]).
%% G3 thin wrappers for adm_group_handler
-export([find_by_id/1]).
-export([find_by_file_id/1]).
-export([soft_delete/1]).
-export([search_by_name/4]).
-export([list_by_category/4]).
-export([list_by_group/4]).
-ifdef(TEST).
-export([location_columns/1]).
-endif.

-include("cache.hrl").
-include("log.hrl").

%% ===================================================================
%% API 函数
%% ===================================================================

%% ===================================================================
%% direct-v1 确认引用绑定（step-11，协议 §2/§6）：正文经 direct-v1 已
%% confirm 落位，本入口只收逻辑引用——不再接收正文、无 pending/put/pending。
%% ===================================================================

%% @doc 把已 confirmed 的 direct 附件绑定为群文件。
%% 资格硬校验（T27）：attachment 行必须存在（已 confirm）、creator=上传者、
%% scope=group 且 scope_ref=Gid——未 confirm 或跨主体一律拒绝。
%% 幂等：同 ObjectKey 已绑定时返回既有 file_id（不重复引用）。
-spec bind_file(
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    non_neg_integer(),
    binary()
) ->
    {ok, binary()}
    | {error,
        not_confirmed
        | cross_subject
        | not_member
        | invalid_file_type
        | file_too_large
        | term()}.
bind_file(Gid, UploaderId, FileName, FileType, ObjectKey, FileSize, FileHashHex) ->
    case has_file_access(Gid, UploaderId) of
        false ->
            {error, not_member};
        true ->
            case elib_oss:validate_file_type(FileType) of
                false ->
                    {error, invalid_file_type};
                true ->
                    case workspace_guard:ensure_writable({group, Gid}) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            do_bind_file(
                                Gid,
                                UploaderId,
                                FileName,
                                FileType,
                                ObjectKey,
                                FileSize,
                                FileHashHex
                            )
                    end
            end
    end.

%% 资格硬校验：direct confirm 后的 attachment 行是唯一资格来源。
ensure_bindable_reference(Conn, ObjectKey, UploaderId, Gid) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT creator_user_id, scope, scope_ref, lifecycle_state,"
                " anchor_msg_id, anchor_conv_seq, group_file_id, size, mime_type, file_hash256"
                " FROM attachment"
                " WHERE path = $1 AND status >= 0 FOR UPDATE"
            >>,
            [ObjectKey]
        )
    of
        {ok, [Row]} ->
            Creator = maps:get(<<"creator_user_id">>, Row),
            case maps:get(<<"lifecycle_state">>, Row, <<"live">>) of
                <<"live">> ->
                    case
                        Creator =:= UploaderId andalso
                            maps:get(<<"scope">>, Row) =:= <<"group">> andalso
                            maps:get(<<"scope_ref">>, Row) =:= integer_to_binary(Gid)
                    of
                        true -> {ok, Row};
                        false -> {error, cross_subject}
                    end;
                _ ->
                    {error, not_confirmed}
            end;
        {ok, []} ->
            %% 未 confirm：attachment 无行 → 无可绑定对象（T27）
            {error, not_confirmed};
        {error, R} ->
            %% DB 故障原样上抛（吞成 not_confirmed 会把基础设施故障伪装
            %% 成业务态，误导排障与客户端重试）
            {error, R}
    end.

do_bind_file(Gid, UploaderId, FileName, FileType, ObjectKey, FileSize, FileHashHex) ->
    elib_pg:with_deadlock_retry_tx(fun(Conn) ->
        ok = workspace_guard:abort_on_error(
            attachment_ds:ensure_upload_scope_tx_retryable(Conn, {group, Gid}, UploaderId)
        ),
        ok = workspace_guard:abort_on_error(
            attachment_repo:bind_reference_lock_tx(Conn, ObjectKey)
        ),
        ok = workspace_guard:abort_on_error(
            attachment_repo:verify_direct_winner_tx(Conn, ObjectKey)
        ),
        case ensure_bindable_reference(Conn, ObjectKey, UploaderId, Gid) of
            {ok, Row} ->
                ok = validate_bind_metadata(Row, FileType, FileSize, FileHashHex),
                bind_locked(
                    Conn,
                    Row,
                    Gid,
                    UploaderId,
                    FileName,
                    FileType,
                    ObjectKey,
                    FileSize,
                    <<>>
                );
            {error, Reason} ->
                throw({abort_tx, Reason})
        end
    end).

%% The confirmed attachment stores SHA-256, not the legacy group-file MD5.
%% A submitted MD5 cannot be authenticated from that digest.
validate_bind_metadata(Row, FileType, FileSize, FileMd5) ->
    case
        is_integer(FileSize) andalso FileSize >= 0 andalso
            FileSize =:= confirmed_size(maps:get(<<"size">>, Row, undefined))
    of
        true -> ok;
        false -> throw({abort_tx, file_size_mismatch})
    end,
    case maps:get(<<"mime_type">>, Row, undefined) of
        FileType -> ok;
        _ -> throw({abort_tx, file_type_mismatch})
    end,
    case FileMd5 of
        <<>> -> ok;
        _ -> throw({abort_tx, unverifiable_file_md5})
    end.

confirmed_size(Size) when is_integer(Size), Size >= 0 -> Size;
confirmed_size(Size) when is_binary(Size) ->
    try
        binary_to_integer(Size)
    catch
        error:badarg -> invalid
    end;
confirmed_size(_) ->
    invalid.

bind_locked(
    Conn,
    #{<<"group_file_id">> := ExistingId},
    _Gid,
    _Uid,
    _Name,
    _Type,
    _Key,
    _Size,
    _Hash
) when ExistingId =/= null ->
    case
        elib_pg:query(Conn, <<"SELECT file_id FROM group_file WHERE id=$1 AND status=1">>, [
            ExistingId
        ])
    of
        {ok, [#{<<"file_id">> := ExistingBizId}]} ->
            {ok, ExistingBizId};
        {ok, []} ->
            throw({abort_tx, group_file_binding_missing});
        {error, Reason} ->
            throw({abort_tx, Reason})
    end;
bind_locked(
    _Conn,
    #{<<"anchor_msg_id">> := Anchor},
    _Gid,
    _Uid,
    _Name,
    _Type,
    _Key,
    _Size,
    _Hash
) when Anchor =/= null ->
    throw({abort_tx, attachment_anchor_conflict});
bind_locked(
    _Conn,
    #{<<"anchor_conv_seq">> := Anchor},
    _Gid,
    _Uid,
    _Name,
    _Type,
    _Key,
    _Size,
    _Hash
) when Anchor =/= null ->
    throw({abort_tx, attachment_anchor_conflict});
bind_locked(Conn, _Row, Gid, UploaderId, FileName, FileType, ObjectKey, FileSize, FileHashHex) ->
    Data = #{
        group_id => Gid,
        file_id => elib_oss:generate_file_id(),
        file_name => FileName,
        file_size => FileSize,
        file_type => FileType,
        file_category => atom_to_binary(elib_oss:get_file_category(FileType), utf8),
        file_url => ObjectKey,
        file_hash => FileHashHex,
        uploader_id => UploaderId,
        download_count => 0,
        status => 1,
        created_at => elib_dt:now(),
        updated_at => elib_dt:now()
    },
    case group_file_repo:insert_tx(Conn, Data) of
        {ok, GroupFileId} ->
            ok = write_attachment_tx(Conn, Data, GroupFileId, ObjectKey),
            %% direct 行在 confirm 时已存在：save 的 ON CONFLICT
            %% 刻意不覆盖既有列（收藏幂等语义）——bind 的本质是
            %% 引用回填，这里显式补 group_file_id（幂等：仅 NULL 时）。
            case
                elib_pg:query(
                    Conn,
                    <<
                        "UPDATE attachment SET group_file_id = $2"
                        " WHERE path = $1 AND group_file_id IS NULL RETURNING id"
                    >>,
                    [ObjectKey, GroupFileId]
                )
            of
                {ok, [_]} -> ok;
                {ok, []} -> throw({abort_tx, group_file_binding_conflict});
                {error, UpdateReason} -> throw({abort_tx, UpdateReason})
            end,
            {ok, _} = attachment_repo:refresh_gc_verified_tx(Conn, ObjectKey),
            ok = audit_file_tx(Conn, Gid, UploaderId, GroupFileId, <<"file.uploaded">>),
            {ok, maps:get(file_id, Data)};
        {error, Reason} ->
            throw({abort_tx, Reason})
    end.

%% @doc 下载文件
%% @param FileId 文件ID（主键）
%% @param CurrentUid 当前用户ID
%% @return {ok, ObjectKey} | {error, Reason}，Logic 继续调用附件读授权签发 URL。
-spec download_file(integer(), integer()) -> {ok, binary()} | {error, term()}.
download_file(FileId, CurrentUid) ->
    % 1. 查询文件信息
    case group_file_repo:find_by_id(FileId) of
        #{<<"id">> := _, <<"group_id">> := Gid, <<"status">> := 1} ->
            % 2. 验证群成员身份
            case has_file_access(Gid, CurrentUid) of
                false ->
                    {error, not_member};
                true ->
                    % 3. 增加下载计数（T7 派生读写 freeze：归档时跳过计数，
                    %    归档只读仍需群和父级资格）
                    spawn(fun() ->
                        _ =
                            workspace_guard:write_tx_or_skip({group, Gid}, fun(Conn) ->
                                group_file_repo:increment_download_tx(Conn, FileId)
                            end),
                        ok
                    end),
                    attachment_ds:find_path_by_group_file(Gid, FileId)
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
        #{
            <<"id">> := StoredFileId,
            <<"group_id">> := Gid,
            <<"status">> := 1,
            <<"uploader_id">> := UploaderId
        } ->
            % 2. 验证权限（上传者或管理员）
            case authorize_delete(CurrentUid, UploaderId, Gid) of
                {ok, true} ->
                    % 3. 软删除文件（T7：{group, Gid} 行锁与写入同事务）
                    case
                        elib_pg:with_tx(fun(Conn) ->
                            ok = workspace_guard:abort_on_error(
                                attachment_ds:ensure_upload_scope_tx(Conn, {group, Gid}, CurrentUid)
                            ),
                            case group_file_repo:soft_delete_tx(Conn, StoredFileId) of
                                {ok, Count} = Deleted when Count > 0 ->
                                    ok = audit_file_tx(
                                        Conn, Gid, CurrentUid, StoredFileId, <<"file.deleted">>
                                    ),
                                    Deleted;
                                {ok, 0} = Unchanged ->
                                    Unchanged;
                                {error, Reason0} ->
                                    throw({abort_tx, Reason0})
                            end
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
    case has_file_access(Gid, CurrentUid) of
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
    case has_file_access(Gid, CurrentUid) of
        false ->
            {error, not_member};
        true ->
            % 2. 搜索文件
            group_file_repo:search_by_name(Gid, Keyword, Page, Size)
    end.

%% @doc 获取分类统计，必须携带当前用户并重验群与父级资格。
-spec get_file_categories(integer(), integer()) -> {ok, list()} | {error, term()}.
get_file_categories(Gid, CurrentUid) ->
    case attachment_ds:authorize_group_scope(Gid, CurrentUid) of
        true -> group_file_repo:category_stats(Gid);
        false -> {error, not_member}
    end.

has_file_access(Gid, Uid) ->
    group_ds:is_member(Uid, Gid) andalso attachment_ds:authorize_group_scope(Gid, Uid).

authorize_delete(Uid, UploaderId, Gid) ->
    case attachment_ds:authorize_group_scope(Gid, Uid) of
        true -> check_delete_permission(Uid, UploaderId, Gid);
        false -> {error, not_member}
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 同事务写入群文件的真实存储路径（含命名空间前缀），不由业务 ID 重建。
-spec write_attachment_tx(any(), map(), integer(), binary()) -> ok.
write_attachment_tx(Conn, Data, GroupFileId, ObjectKey) ->
    write_attachment_tx(Conn, Data, GroupFileId, ObjectKey, #{}).

-spec write_attachment_tx(any(), map(), integer(), binary(), map()) -> ok.
write_attachment_tx(
    Conn,
    #{
        group_id := Gid,
        uploader_id := UploaderId,
        file_name := FileName,
        file_url := FileUrl,
        file_type := FileType,
        file_size := FileSize,
        file_hash := FileHashHex
    },
    GroupFileId,
    ObjectKey,
    Location
) ->
    SafeName = filename:basename(FileName),
    Attach = maps:merge(
        #{
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
        location_columns(Location)
    ),
    %% W3-01：首绑与 intent/GC 同对象锁（§4 锁序），锁内复核非墓碑后落绑
    ok = attachment_repo:bind_reference_lock_tx(Conn, ObjectKey),
    attachment_ds:save(Conn, elib_dt:now(), UploaderId, [Attach]),
    %% gc_verified 覆盖证明：仅群唯一绑定+明文+完整位置的可证明新对象
    {ok, _} = attachment_repo:refresh_gc_verified_tx(Conn, ObjectKey),
    ok.

location_columns(Location) ->
    %% 运行时校验而非模式匹配：当前仓内唯一调用值域恒为 #{}（bind 路径），
    %% 完整四元组分支是 direct-v1 预留接口；若用 case 模式匹配，Dialyzer
    %% 会按值域收窄把完整分支判为不可达（pattern_match 棘轮变红）。
    %% 语义与旧实现一致：空 map → 不写位置列（NULL，绝不推断默认桶），
    %% 不完整位置 → fail-closed throw。
    case storage_identity(Location) of
        {ok, Driver, Namespace, Container, Digest} ->
            #{
                <<"storage_driver">> => atom_to_binary(Driver),
                <<"storage_namespace">> => Namespace,
                <<"storage_container">> => Container,
                <<"storage_locator_sha256">> => Digest
            };
        empty ->
            #{};
        incomplete ->
            throw({abort_tx, incomplete_storage_identity})
    end.

storage_identity(Location) when is_map(Location), map_size(Location) =:= 0 ->
    empty;
storage_identity(Location) when is_map(Location) ->
    Driver = maps:get(driver, Location, undefined),
    Namespace = maps:get(namespace, Location, undefined),
    Container = maps:get(container, Location, undefined),
    Digest = maps:get(locator_digest, Location, undefined),
    case
        (Driver =:= disk orelse Driver =:= garage) andalso
            is_binary(Namespace) andalso Namespace =/= <<>> andalso
            is_binary(Container) andalso Container =/= <<>> andalso
            is_binary(Digest) andalso Digest =/= <<>>
    of
        true -> {ok, Driver, Namespace, Container, Digest};
        false -> incomplete
    end.

audit_file_tx(Conn, Gid, Uid, FileId, Action) ->
    workspace_guard:abort_on_error(
        enterprise_audit_event_repo:append_scope_file_tx(
            Conn,
            {group, Gid},
            Uid,
            #{resource_type => <<"group_file">>, resource_id => FileId, action => Action}
        )
    ).

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
