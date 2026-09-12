-module(attachment_repo).
%%%
% attachment 相关操作都放到该模块，存储库模块
% attachment related operations are put in this module, repository module
%%%

%% @doc 获取附件表名
%% @returns binary() 表名
-export([tablename/0]).

%% @doc 保存附件信息
%% 保存附件信息，如果 MD5 已存在则更新引用次数
%% @param Conn 数据库连接
%% @param CreatedAt 创建时间
%% @param Uid 用户ID
%% @param Attach 附件信息列表
%% @returns ok
-export([save/4]).

%% @doc 查询附件统计信息（管理后台用）
-export([stats/0]).

%% @doc 分页查询附件列表（管理后台用）
%% @param Page 页码（从1开始）
%% @param Size 每页大小
%% @param Opts 筛选选项 #{mime_type => binary(), keyword => binary()}
-export([page/3]).

%% @doc 更新附件状态（禁用/启用/软删除）
%% Status: 1=正常 0=禁用 -1=软删除
-export([update_status/2]).

%% @doc 把 scope='moment' 且尚未绑定(scope_ref IS NULL)的附件回填 scope_ref=MomentId
%% （两阶段绑定：媒体在发帖前上传时 scope_ref 未知，发帖后按 object_key 回填）。
-export([bind_moment_scope_ref/2]).

%% @doc 孤儿附件统计（预览用）
-export([orphan_stats/1]).

%% @doc 获取孤儿附件列表（供物理删除）
-export([orphan_list_for_delete/1]).

%% @doc 物理删除数据库行（S3 删除成功后调用）
-export([hard_delete_by_ids/1]).

%% @doc 根据 object_key(path) 和上传者 uid 查找附件（用于归属校验）
-export([find_by_path_and_uid/2]).

%% @doc 根据 object_key(path) 查找附件（不带 uid，读鉴权按 scope 在 logic 层判定）
-export([find_by_path/1]).

-export([find_path_by_id/1]).

%% @doc 单 statement 校验群附件的当前世代边界。
-export([authorize_group_access/2]).
%% 真库回归直接执行生产 SQL，避免用 mock/字符串包含断言替代 PostgreSQL 语义。
-export([group_access_sql/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% image mime 归一为标准子类型（eunit 验收；修复前 image/jpeg 被扩展名
%% 重写成 image/jpg），定义见文件尾 normalize_image_mime/1
-ifdef(TEST).
-export([normalize_image_mime/1]).
-endif.
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"attachment">>).

%%% 保存附近信息，不存在就新增，存在就递增应用次数
-spec save(epgsql:connection() | pid(), binary(), integer() | binary(), [map()]) -> ok.
save(_Conn, _CreatedAt, _Uid, []) ->
    ok;
save(Conn, CreatedAt, Uid, [Attach | Tail]) ->
    %% 双读兼容：优先新键 file_hash256，回退旧键 md5（过渡期）
    Md5 = maps:get(<<"file_hash256">>, Attach, maps:get(<<"md5">>, Attach, <<>>)),
    MimeType = maps:get(<<"mime_type">>, Attach),
    Name = maps:get(<<"name">>, Attach),
    Path = maps:get(<<"path">>, Attach),
    Url = maps:get(<<"url">>, Attach),
    Size = maps:get(<<"size">>, Attach),
    %% 读鉴权范围与绑定实体（缺省 private/NULL，兼容历史调用）
    Scope = maps:get(<<"scope">>, Attach, <<"private">>),
    ScopeRef = maps:get(<<"scope_ref">>, Attach, null),
    %% E2EE-061 密文判别位（迁移 000050）：null = 明文对象。
    %% 缺省 null 保证历史调用方（收藏、转发等不经 confirm 的写入）语义不变。
    Cipher = maps:get(<<"cipher">>, Attach, null),
    AnchorMsgId = maps:get(<<"anchor_msg_id">>, Attach, null),
    GroupFileId = maps:get(<<"group_file_id">>, Attach, null),
    Ext = filename:extension(Path),

    Ext2 = ec_cnv:to_binary(Ext),
    Size2 = ec_cnv:to_integer(Size),
    Path2 = ec_cnv:to_binary(Path),
    % Path2 = ec_cnv:to_binary(Path),
    Attach2 = jsone:encode(Attach),

    %% image/* mime 归一化（2026-09-11 moya 报障根因修复）：历史逻辑用 object_key
    %% 扩展名重写 image 子类型（image/jpeg + .jpg → image/jpg），产生非法 MIME
    %% 子类型——confirm HEAD 核实值本为 image/jpeg，落库却成 image/jpg，客户端
    %% 按 image/jpeg 白名单判不过。改为仅归一化已知别名，标准值原样落库
    %% （历史数据不动，只修新写入）。
    MimeType2 = normalize_image_mime(MimeType),

    % 拼接 ON CONFLICT 子句。
    % 去重键用 path(object_key)，非 md5：object_key 由 build_object_key 构造，
    % 含毫秒时间戳 + 随机串，每次上传天然唯一，仅 confirm 重试/同附件收藏会命中
    % → DO UPDATE 递增 referer（幂等）。md5 仅作文件 hash 完整性参考，不再做全局唯一去重
    % （旧 md5 唯一约束在多 scope/多 owner 下会把 path/scope 锁死成首条，导致越权/孤儿，见迁移 000015）。
    OnConflictUpdate = <<
        "ON CONFLICT (path) DO UPDATE SET "
        "last_referer_user_id = EXCLUDED.last_referer_user_id, "
        "last_referer_at = EXCLUDED.last_referer_at, "
        "updated_at = EXCLUDED.updated_at, "
        "referer_time = public.attachment.referer_time + 1"
    >>,
    % <<"(,,,,,,,,,,,,updated_at,created_at,status)">>,
    NewAttach = #{
        <<"file_hash256">> => Md5,
        <<"mime_type">> => MimeType2,
        <<"ext">> => Ext2,
        <<"name">> => Name,
        <<"path">> => Path2,
        <<"url">> => Url,
        <<"size">> => Size2,
        <<"info">> => Attach2,
        % 初始引用次数
        <<"referer_time">> => 1,
        % 使用绑定变量
        <<"last_referer_user_id">> => Uid,
        % 使用原生时间格式
        <<"last_referer_at">> => CreatedAt,
        <<"creator_user_id">> => Uid,
        <<"scope">> => Scope,
        <<"scope_ref">> => ScopeRef,
        <<"cipher">> => Cipher,
        <<"anchor_msg_id">> => AnchorMsgId,
        <<"group_file_id">> => GroupFileId,
        <<"updated_at">> => CreatedAt,
        <<"created_at">> => CreatedAt,
        <<"status">> => 1
    },

    % 预生成 TSID
    AttId = elib_tsid:generate(attachment),
    NewAttach2 = NewAttach#{<<"id">> => AttId},
    % 构建带ON CONFLICT的INSERT SQL
    {Sql, Params} = elib_pg_sql:insert(tablename(), NewAttach2),
    FullSql = [Sql, <<" ">>, OnConflictUpdate],
    _ = elib_pg:execute(Conn, FullSql, Params),
    % Res = epgsql:execute_batch(Conn, [{Stmt1, []}]),
    % elib_log:info(io_lib:format("attachment_repo:save/4: Res ~p ~n", [Res])),
    % 递归保存附近信息
    save(Conn, CreatedAt, Uid, Tail),
    ok.

%% ===================================================================
%% Admin Query Functions
%% ===================================================================

%% @doc 查询附件统计信息
%% 返回总文件数、总大小、各类型计数、今日上传等聚合数据
-spec stats() -> map().
stats() ->
    Tb = tablename(),
    Sql = <<
        "SELECT "
        "  COUNT(*) AS total_files, "
        "  COALESCE(SUM(size), 0) AS total_size, "
        "  COUNT(*) FILTER (WHERE mime_type LIKE 'image/%') AS image_count, "
        "  COUNT(*) FILTER (WHERE mime_type LIKE 'video/%') AS video_count, "
        "  COUNT(*) FILTER (WHERE mime_type LIKE '%pdf%' OR mime_type LIKE 'text/%' "
        "    OR mime_type LIKE 'application/msword%' OR mime_type LIKE 'application/vnd%') AS document_count, "
        "  COUNT(*) FILTER (WHERE mime_type NOT LIKE 'image/%' AND mime_type NOT LIKE 'video/%' "
        "    AND mime_type NOT LIKE '%pdf%' AND mime_type NOT LIKE 'text/%' "
        "    AND mime_type NOT LIKE 'application/msword%' AND mime_type NOT LIKE 'application/vnd%') AS other_count, "
        "  COUNT(*) FILTER (WHERE created_at >= CURRENT_DATE) AS today_uploads, "
        "  COALESCE(SUM(size) FILTER (WHERE created_at >= CURRENT_DATE), 0) AS today_size "
        "FROM "
    >>,
    FullSql = [Sql, Tb, <<" WHERE status = 1">>],
    case elib_pg:one(FullSql, []) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 分页查询附件列表
%% @param Page 页码（从1开始）
%% @param Size 每页大小
%% @param Opts 筛选选项 #{mime_type => binary(), keyword => binary(),
%%                         status => <<"all">> | <<"1">> | <<"0">> | <<"-1">>}
%%   默认只查正常（status=1）；admin 传 status 筛选禁用/软删除行（否则禁用后无启用入口）
-spec page(pos_integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Opts) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    MimeType = maps:get(mime_type, Opts, undefined),
    Keyword = maps:get(keyword, Opts, undefined),

    {WhereExtra, FilterParams} = build_filter(MimeType, Keyword),
    BaseWhere = [status_where(maps:get(status, Opts, <<"1">>)), WhereExtra],

    CountSql = [<<"SELECT COUNT(*) AS count FROM ">>, Tb, BaseWhere],
    Total =
        case elib_pg:one(CountSql, FilterParams) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,

    ParamN = length(FilterParams),
    LimitN = ParamN + 1,
    OffsetN = ParamN + 2,
    LimitRef = <<"$", (integer_to_binary(LimitN))/binary>>,
    OffsetRef = <<"$", (integer_to_binary(OffsetN))/binary>>,

    DataSql = [
        <<"SELECT id, file_hash256, mime_type, name, path, url, size, referer_time, status, created_at FROM ">>,
        Tb,
        BaseWhere,
        <<" ORDER BY created_at DESC LIMIT ">>,
        LimitRef,
        <<" OFFSET ">>,
        OffsetRef
    ],
    AllParams = FilterParams ++ [Size, Offset],

    Items =
        case elib_pg:query(DataSql, AllParams) of
            {ok, Rows} -> Rows;
            _ -> []
        end,

    TotalPage =
        case Total > 0 of
            true -> ((Total - 1) div Size) + 1;
            false -> 0
        end,

    {ok, #{
        <<"list">> => Items,
        <<"page">> => Page,
        <<"size">> => Size,
        <<"total">> => Total,
        <<"total_page">> => TotalPage
    }}.

%% @doc 状态 WHERE 片段（白名单字面量，无参数拼接）
-spec status_where(binary()) -> binary().
status_where(<<"all">>) -> <<" WHERE status >= -1">>;
status_where(<<"0">>) -> <<" WHERE status = 0">>;
status_where(<<"-1">>) -> <<" WHERE status = -1">>;
status_where(_) -> <<" WHERE status = 1">>.

%% @doc 构建动态 WHERE 条件
-spec build_filter(binary() | undefined, binary() | undefined) -> {iodata(), list()}.
build_filter(undefined, undefined) ->
    {<<>>, []};
build_filter(MimeType, undefined) when MimeType =/= undefined, MimeType =/= <<>> ->
    {<<" AND mime_type LIKE $1">>, [<<MimeType/binary, "%">>]};
build_filter(undefined, Keyword) when Keyword =/= undefined, Keyword =/= <<>> ->
    {<<" AND (name ILIKE $1 OR file_hash256 LIKE $1)">>, [<<"%", Keyword/binary, "%">>]};
build_filter(MimeType, Keyword) when
    MimeType =/= undefined,
    MimeType =/= <<>>,
    Keyword =/= undefined,
    Keyword =/= <<>>
->
    {<<" AND mime_type LIKE $1 AND (name ILIKE $2 OR file_hash256 LIKE $2)">>, [
        <<MimeType/binary, "%">>, <<"%", Keyword/binary, "%">>
    ]};
build_filter(_, _) ->
    {<<>>, []}.

%% ===================================================================
%% Admin Write Functions
%% ===================================================================

-spec update_status(integer() | binary(), integer()) -> ok | {error, term()}.
update_status(Id, Status) when Status =:= -1; Status =:= 0; Status =:= 1 ->
    Tb = tablename(),
    case
        elib_pg:update(
            Tb,
            #{<<"status">> => Status, <<"updated_at">> => elib_dt:now()},
            <<"id = $1">>,
            [Id]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec bind_moment_scope_ref([binary()], integer() | binary()) -> ok | {error, term()}.
bind_moment_scope_ref([], _MomentId) ->
    ok;
bind_moment_scope_ref(ObjectKeys, MomentId) when is_list(ObjectKeys) ->
    Tb = tablename(),
    %% 显式参数编号；scope_ref 为 text 列，MomentId 转 binary。
    %% 只回填 scope='moment' 且 scope_ref IS NULL 的行（刚上传待绑定的媒体），
    %% object_key 含 uid 前缀天然 user-namespaced，按 path 精确匹配安全。
    Sql =
        <<"UPDATE ", Tb/binary, " SET scope_ref = $1, updated_at = $2 ",
            "WHERE scope = 'moment' AND scope_ref IS NULL AND path = ANY($3)">>,
    case elib_pg:query(Sql, [ec_cnv:to_binary(MomentId), elib_dt:now(), ObjectKeys]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec orphan_stats(map()) -> {ok, map()} | {error, term()}.
orphan_stats(Opts) ->
    Tb = tablename(),
    AgeDays = maps:get(age_days, Opts, 30),
    Sql = [
        <<"SELECT COUNT(*) AS count, COALESCE(SUM(size), 0) AS total_size FROM ">>,
        Tb,
        <<
            " WHERE status = 1 AND referer_time = 0"
            " AND created_at < NOW() - ($1 * INTERVAL '1 day')"
            " AND (last_referer_at IS NULL"
            " OR last_referer_at < NOW() - ($1 * INTERVAL '1 day'))"
        >>
    ],
    case elib_pg:one(Sql, [AgeDays]) of
        {ok, Row} -> {ok, Row};
        {error, R} -> {error, R}
    end.

-spec orphan_list_for_delete(map()) -> {ok, [map()]} | {error, term()}.
orphan_list_for_delete(Opts) ->
    Tb = tablename(),
    AgeDays = maps:get(age_days, Opts, 30),
    Sql = [
        <<"SELECT id, path FROM ">>,
        Tb,
        <<
            " WHERE status = 1 AND referer_time = 0"
            " AND created_at < NOW() - ($1 * INTERVAL '1 day')"
            " AND (last_referer_at IS NULL"
            " OR last_referer_at < NOW() - ($1 * INTERVAL '1 day'))"
            " LIMIT 500"
        >>
    ],
    case elib_pg:query(Sql, [AgeDays]) of
        {ok, Rows} -> {ok, Rows};
        {error, R} -> {error, R}
    end.

-spec hard_delete_by_ids([integer()]) -> ok | {error, term()}.
hard_delete_by_ids([]) ->
    ok;
hard_delete_by_ids(Ids) ->
    Tb = tablename(),
    Placeholders = iolist_to_binary(
        lists:join(
            <<",">>,
            [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Ids))]
        )
    ),
    Sql = [<<"DELETE FROM ">>, Tb, <<" WHERE id = ANY(ARRAY[">>, Placeholders, <<"]::bigint[])">>],
    case elib_pg:execute(Sql, Ids) of
        {ok, _} -> ok;
        {error, R} -> {error, R}
    end.

%% ===================================================================
%% Ownership Verification
%% ===================================================================

%% @doc 根据 object_key(path) 和上传者 uid 查找附件
%% 用于归属校验：确认 ObjectKey 是否由 Uid 上传
%% @param ObjectKey 对象存储路径（对应 path 列）
%% @param Uid 用户 ID
%% @returns {ok, map()} | {error, not_found} | {error, term()}
-spec find_by_path_and_uid(binary(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_path_and_uid(ObjectKey, Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, path, creator_user_id FROM ", Tb/binary,
            " WHERE path = $1 AND creator_user_id = $2 AND status >= 0 LIMIT 1">>,
    case elib_pg:query(Sql, [ObjectKey, Uid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% @doc 根据 object_key(path) 查找附件元数据（scope/scope_ref/creator），供读鉴权
%% authorize/2 使用。不带 uid 条件——读归属按 scope 区分，由 logic 层裁决。
-spec find_by_path(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_path(ObjectKey) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, path, url, creator_user_id, scope, scope_ref FROM ", Tb/binary,
            " WHERE path = $1 AND status >= 0 LIMIT 1">>,
    case elib_pg:query(Sql, [ObjectKey]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% @doc 群附件下载授权。聊天附件按 anchor_conv_seq 检查当前 generation；独立
%% 群文件按 group_file_id 保持当前成员共享语义。两类都在同一个 READ COMMITTED
%% statement 内重验 active group/member/open generation；未知或未绑定记录 false。
-spec authorize_group_access(binary(), integer()) -> boolean().
authorize_group_access(ObjectKey, Uid) ->
    Tb = tablename(),
    Sql = group_access_sql(Tb),
    case elib_pg:one(Sql, [ObjectKey, Uid]) of
        {ok, #{<<"allowed">> := true}} -> true;
        _ -> false
    end.

-spec group_access_sql(binary()) -> binary().
group_access_sql(Tb) ->
    <<"SELECT EXISTS (SELECT 1 FROM ", Tb/binary, " a ", "JOIN public.group_member_generation gmg ",
        " ON a.scope_ref = gmg.group_id::text AND gmg.user_id = $2 ", " AND gmg.end_seq IS NULL ",
        "JOIN public.group_member gm ON gm.group_id = gmg.group_id ",
        " AND gm.user_id = gmg.user_id AND gm.status = 1 ",
        "JOIN public.\"group\" grp ON grp.id = gmg.group_id AND grp.status = 1 ",
        "WHERE a.path = $1 AND a.scope = 'group' AND a.status >= 0 ",
        "AND ((a.group_file_id IS NULL AND a.anchor_conv_seq IS NOT NULL ",
        "      AND gmg.start_seq <= a.anchor_conv_seq) ",
        " OR (a.group_file_id IS NOT NULL AND EXISTS (",
        "      SELECT 1 FROM public.group_file gf ",
        "      WHERE gf.id = a.group_file_id AND gf.group_id = gmg.group_id ",
        "        AND gf.status = 1)))) AS allowed">>.

%% @doc 按 id 查询附件 path（ObjectKey），供 admin 下载端点签发 presign GET
%% 仅返回未软删除（status >= 0）的记录
-spec find_path_by_id(integer() | binary()) -> {ok, binary()} | {error, not_found | term()}.
find_path_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"SELECT path FROM ", Tb/binary, " WHERE id = $1 AND status >= 0 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [#{<<"path">> := Path}]} -> {ok, Path};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% image mime 已知别名归一：jpg→jpeg、tif→tiff。其余（含全部标准子类型）
%% 原样返回——不再用 object_key 扩展名重写子类型。
-spec normalize_image_mime(binary()) -> binary().
normalize_image_mime(<<"image/jpg">>) ->
    <<"image/jpeg">>;
normalize_image_mime(<<"image/tif">>) ->
    <<"image/tiff">>;
normalize_image_mime(MimeType) ->
    MimeType.

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
