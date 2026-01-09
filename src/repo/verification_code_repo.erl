-module(verification_code_repo).

%%%
% verification_code_repo 是 verification_code repository 缩写
%%%

-dialyzer([{nowarn_function, tablename/0}, {nowarn_function, find_by_id/1}, {nowarn_function, save/4}]).

-export([tablename/0]).
-export([find_by_id/1]).
-export([save/4]).

-spec tablename() -> binary().
tablename() ->
    imboy_pg_sql:public_tablename(<<"verification_code">>).

-spec find_by_id(binary()) -> map() | undefined.
find_by_id(Id) ->
    Tb = tablename(),
    Column = <<"id,code,validity_at,created_at">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = \$1">>,
    case imboy_pg:one(Sql, [Id]) of
        {ok, Row} ->
            Row;
        _ ->
            undefined
    end.

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
    imboy_pg:execute(Sql, [ToEmail, VerifyCode, ValidityAt, Now]).

