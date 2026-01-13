-module(verification_code_repo).

%%%
% verification_code_repo 是 verification_code repository 缩写
% 验证码数据仓库层，提供验证码信息的基础数据库操作
%%%

-dialyzer([{nowarn_function, tablename/0}, {nowarn_function, find_by_id/1}, {nowarn_function, save/4}]).

-export([tablename/0]).
-export([find_by_id/1]).
-export([save/4]).

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
    Sql = <<"INSERT INTO ", Tb/binary, " (id,code,validity_at,created_at) "
            "VALUES (\$1, \$2, \$3, \$4) "
            "ON CONFLICT (id) DO UPDATE SET "
            "code = EXCLUDED.code, "
            "validity_at = EXCLUDED.validity_at, "
            "created_at = EXCLUDED.created_at">>,
    elib_pg:execute(Sql, [ToEmail, VerifyCode, ValidityAt, Now]).

