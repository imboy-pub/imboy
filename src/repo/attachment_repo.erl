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

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
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
    Md5 = maps:get(<<"md5">>, Attach),
    MimeType = maps:get(<<"mime_type">>, Attach),
    Name = maps:get(<<"name">>, Attach),
    Path = maps:get(<<"path">>, Attach),
    Url = maps:get(<<"url">>, Attach),
    Size = maps:get(<<"size">>, Attach),
    Ext = filename:extension(Path),

    Ext2 = ec_cnv:to_binary(Ext),
    Size2 = ec_cnv:to_binary(Size),
    Path2 = ec_cnv:to_binary(Path),
    % Path2 = ec_cnv:to_binary(Path),
    Attach2 = jsone:encode(Attach),

    MimeType2 =
        case binary:split(MimeType, <<"/">>) of
            [<<"image">>, _] ->
                <<".", Ext3/binary>> = Ext2,
                <<"image/", Ext3/binary>>;
            _ ->
                MimeType
        end,

    % 拼接ON CONFLICT子句
    OnConflictUpdate = <<
        "ON CONFLICT (md5) DO UPDATE SET "
        "last_referer_user_id = EXCLUDED.last_referer_user_id, "
        "last_referer_at = EXCLUDED.last_referer_at, "
        "updated_at = EXCLUDED.updated_at, "
        "referer_time = public.attachment.referer_time + 1"
    >>,
    % <<"(,,,,,,,,,,,,updated_at,created_at,status)">>,
    NewAttach = #{
        <<"md5">> => Md5,
        <<"mime_type">> => MimeType2,
        <<"ext">> => Ext2,
        <<"name">> => Name,
        <<"path">> => Path2,
        <<"url">> => Url,
        <<"size">> => Size2,
        <<"info">> => Attach2,
        <<"referer_time">> => 1,                % 初始引用次数
        <<"last_referer_user_id">> => Uid,      % 使用绑定变量
        <<"last_referer_at">> => CreatedAt, % 使用原生时间格式
        <<"creator_user_id">> => Uid,
        <<"updated_at">> => CreatedAt,
        <<"created_at">> => CreatedAt,
        <<"status">> => 1
    },

    % 构建带ON CONFLICT的INSERT SQL
    {Sql, Params} = elib_pg_sql:insert(tablename(), NewAttach, <<>>),
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
        "FROM ">>,
    FullSql = [Sql, Tb, <<" WHERE status = 1">>],
    case elib_pg:one(FullSql, []) of
        {ok, Row} -> Row;
        _ -> #{}
    end.


%% @doc 分页查询附件列表
%% @param Page 页码（从1开始）
%% @param Size 每页大小
%% @param Opts 筛选选项 #{mime_type => binary(), keyword => binary()}
-spec page(pos_integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Opts) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    MimeType = maps:get(mime_type, Opts, undefined),
    Keyword = maps:get(keyword, Opts, undefined),

    {WhereExtra, FilterParams} = build_filter(MimeType, Keyword),
    BaseWhere = [<<" WHERE status = 1">>, WhereExtra],

    CountSql = [<<"SELECT COUNT(*) AS count FROM ">>, Tb, BaseWhere],
    Total = case elib_pg:one(CountSql, FilterParams) of
        {ok, #{<<"count">> := C}} -> C;
        _ -> 0
    end,

    ParamN = length(FilterParams),
    LimitN = ParamN + 1,
    OffsetN = ParamN + 2,
    LimitRef = <<"$", (integer_to_binary(LimitN))/binary>>,
    OffsetRef = <<"$", (integer_to_binary(OffsetN))/binary>>,

    DataSql = [
        <<"SELECT id, md5, mime_type, name, path, url, size, referer_time, status, created_at FROM ">>,
        Tb, BaseWhere,
        <<" ORDER BY created_at DESC LIMIT ">>, LimitRef,
        <<" OFFSET ">>, OffsetRef
    ],
    AllParams = FilterParams ++ [Size, Offset],

    Items = case elib_pg:query(DataSql, AllParams) of
        {ok, Rows} -> Rows;
        _ -> []
    end,

    TotalPage = case Total > 0 of
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


%% @doc 构建动态 WHERE 条件
-spec build_filter(binary() | undefined, binary() | undefined) -> {iodata(), list()}.
build_filter(undefined, undefined) ->
    {<<>>, []};
build_filter(MimeType, undefined) when MimeType =/= undefined, MimeType =/= <<>> ->
    {<<" AND mime_type LIKE $1">>, [<<MimeType/binary, "%">>]};
build_filter(undefined, Keyword) when Keyword =/= undefined, Keyword =/= <<>> ->
    {<<" AND (name ILIKE $1 OR md5 LIKE $1)">>, [<<"%", Keyword/binary, "%">>]};
build_filter(MimeType, Keyword) when MimeType =/= undefined, MimeType =/= <<>>,
                                     Keyword =/= undefined, Keyword =/= <<>> ->
    {<<" AND mime_type LIKE $1 AND (name ILIKE $2 OR md5 LIKE $2)">>,
     [<<MimeType/binary, "%">>, <<"%", Keyword/binary, "%">>]};
build_filter(_, _) ->
    {<<>>, []}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

