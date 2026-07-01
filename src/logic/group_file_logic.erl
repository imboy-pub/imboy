-module(group_file_logic).
-dialyzer({nowarn_function, [upload/5]}).
%%%
% group_file_logic 是 group file logic 缩写
% 群文件业务逻辑层，处理群文件相关的业务逻辑
%%%

-export([upload/5]).
-export([download/2]).
-export([delete/2]).
-export([list/4]).
-export([list/5]).
-export([search/4]).
-export([get_categories/2]).

-include("log.hrl").

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 上传文件到群文件
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param FileName 文件名
%% @param FileBinary 文件二进制数据
%% @param FileType MIME类型
%% @return {ok, FileData} | {error, Reason}
-spec upload(binary(), integer(), binary(), binary(), binary()) -> {ok, map()} | {error, term()}.
upload(Gid, CurrentUid, FileName, FileBinary, FileType) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 调用DS层上传文件
    case group_file_ds:upload_file(Gid2, CurrentUid, FileName, FileBinary, FileType) of
        {ok, FileId} ->
            % 3. 返回文件信息
            FileData = #{
                <<"file_id">> => FileId,
                <<"file_name">> => FileName,
                <<"file_size">> => byte_size(FileBinary),
                <<"file_type">> => FileType,
                <<"file_category">> => elib_oss:get_file_category(FileType),
                <<"created_at">> => elib_dt:timestamp()
            },
            {ok, FileData};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 下载文件
%% @param FileId 文件ID（主键ID）
%% @param CurrentUid 当前用户ID
%% @return {ok, FileUrl} | {error, Reason}
-spec download(integer(), integer()) -> {ok, binary()} | {error, term()}.
download(FileId, CurrentUid) ->
    group_file_ds:download_file(FileId, CurrentUid).

%% @doc 删除文件
%% @param FileId 文件ID（主键ID）
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec delete(integer(), integer()) -> ok | {error, term()}.
delete(FileId, CurrentUid) ->
    group_file_ds:delete_file(FileId, CurrentUid).

%% @doc 查询群文件列表
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, FileList} | {error, Reason}
-spec list(binary(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list(Gid, CurrentUid, Page, Size) ->
    list(Gid, CurrentUid, Page, Size, #{}).

%% @doc 查询群文件列表（带选项）
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @param Options 选项 #{category => binary()}
%% @return {ok, FileList} | {error, Reason}
-spec list(binary(), integer(), integer(), integer(), map()) -> {ok, map()} | {error, term()}.
list(Gid, CurrentUid, Page, Size, Options) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 调用DS层查询文件列表
    case group_file_ds:list_files(Gid2, CurrentUid, Page, Size, Options) of
        {ok, Files} ->
            % 3. 编码ID字段
            Files2 = lists:map(
                fun(File) ->
                    File
                end,
                Files
            ),

            % 4. 查询总数
            {ok, Total} = group_file_ds:count_by_group(Gid2),

            % 5. 返回结果
            FileList = #{
                <<"items">> => Files2,
                <<"total">> => Total,
                <<"page">> => Page,
                <<"size">> => Size
            },
            {ok, FileList};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 搜索群文件
%% @param Gid 群组ID（整数）
%% @param Keyword 关键词
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, Files} | {error, Reason}
-spec search(binary(), binary(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
search(Gid, Keyword, Page, Size) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 调用DS层搜索文件
    case group_file_ds:search_files(Gid2, Keyword, Page, Size) of
        {ok, Files} ->
            % 3. 编码ID字段
            Files2 = lists:map(
                fun(File) ->
                    File
                end,
                Files
            ),
            {ok, Files2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取群文件分类统计
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @return {ok, CategoryStats} | {error, Reason}
-spec get_categories(binary(), integer()) -> {ok, list(map())} | {error, term()}.
get_categories(Gid, CurrentUid) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 验证群成员身份
    case group_ds:is_member(CurrentUid, Gid2) of
        false ->
            {error, not_member};
        true ->
            % 3. 获取分类统计
            case group_file_ds:get_file_categories(Gid2) of
                {ok, Stats} ->
                    % 4. 格式化结果
                    CategoryStats = lists:map(
                        fun({Category, Count, TotalSize}) ->
                            #{
                                <<"category">> => Category,
                                <<"count">> => Count,
                                <<"total_size">> => TotalSize
                            }
                        end,
                        Stats
                    ),
                    {ok, CategoryStats};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
