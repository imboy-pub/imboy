-module(verification_code_repo).

%%%
% verification_code_repo 是 verification_code repository 缩写
% 验证码数据仓库层，提供验证码信息的基础数据库操作
%%%

-dialyzer([
    {nowarn_function, tablename/0}, {nowarn_function, find_by_id/1}, {nowarn_function, save/4}
]).

-export([tablename/0]).
-export([find_by_id/1]).
-export([save/4]).
-export([purge_expired/2]).
-export([count_expired/1]).

%% @doc 获取验证码表的表名
%% @return 返回验证码表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"verification_code">>).

%% @doc 根据ID查找验证码
%% @param Id 验证码ID（通常是邮箱地址）
%% @return Map 查询成功返回验证码信息map，未找到返回undefined
-spec find_by_id(binary()) -> map() | undefined.
find_by_id(Id) ->
    Tb = tablename(),
    Column = <<"id,code,validity_at,created_at">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = \$1">>,
    case elib_pg:one(Sql, [Id]) of
        {ok, Row} ->
            Row;
        _ ->
            undefined
    end.

%% @doc 保存验证码
%% @param ToEmail 接收验证码的邮箱地址
%% @param VerifyCode 验证码
%% @param ValidityAt 验证码有效期
%% @param Now 创建时间
%% @return {ok, Count} 保存成功返回影响行数 | {error, Reason} 保存失败
%% @details 使用 ON CONFLICT 语法实现 upsert
-spec save(binary(), term(), term(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
save(ToEmail, VerifyCode, ValidityAt, Now) ->
    Tb = tablename(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id,code,validity_at,created_at) "
            "VALUES (\$1, \$2, \$3, \$4) "
            "ON CONFLICT (id) DO UPDATE SET "
            "code = EXCLUDED.code, "
            "validity_at = EXCLUDED.validity_at, "
            "created_at = EXCLUDED.created_at">>,
    elib_pg:execute(Sql, [ToEmail, VerifyCode, ValidityAt, Now]).

%% ===================================================================
%% T-02 保留清理（retention-policy.yml sessions_tokens: event-driven→delete）
%% ===================================================================

%% @doc 分批删除过期验证码行。Cutoff 为 rfc3339 binary（毫秒时间戳换算由 worker 负责）。
%% validity_at 为 NULL 的历史行按 created_at 起算。返回本批删除行数。
-spec purge_expired(binary(), pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
purge_expired(Cutoff, Limit) ->
    Tb = tablename(),
    Sql =
        <<"DELETE FROM ", Tb/binary,
            " WHERE id IN ("
            "SELECT id FROM ", Tb/binary,
            " WHERE (validity_at IS NOT NULL AND validity_at < $1)"
            " OR (validity_at IS NULL AND created_at < $1)"
            " LIMIT $2)">>,
    case elib_pg:execute(Sql, [Cutoff, Limit]) of
        {ok, N} when is_integer(N) ->
            {ok, N};
        {ok, _} ->
            {ok, 0};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 过期行计数（dry-run 用）
-spec count_expired(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_expired(Cutoff) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE (validity_at IS NOT NULL AND validity_at < $1)"
            " OR (validity_at IS NULL AND created_at < $1)">>,
    case elib_pg:one(Sql, [Cutoff]) of
        {ok, #{<<"count">> := N}} ->
            {ok, ec_cnv:to_integer(N)};
        {error, Reason} ->
            {error, Reason}
    end.
