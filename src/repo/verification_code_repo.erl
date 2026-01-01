-module(verification_code_repo).
%%%
% verification_code_repo 是 verification_code repository 缩写
%%%

-export([tablename/0]).
-export([find_by_id/1]).
-export([save/4]).


-spec tablename() -> ok.
tablename() ->
    imboy_pg_sql:public_tablename(<<"verification_code">>).


-spec find_by_id(binary()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Column = <<"id,code,validity_at,created_at">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case imboy_pg:one(Sql, [Id]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.


% verification_code_repo:save(<<"test@imboy.pub">>, imboy_func:num_random(6), imboy_dt:add(imboy_dt:now(), {10, minute}), imboy_dt:now()).
-spec save(binary(), integer(), binary(), binary()) -> ok.
save(ToEmail, VerifyCode, ValidityAt, Now) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"INSERT INTO ", Tb/binary, " (id,code,validity_at,created_at) "
            "VALUES ($1, $2, $3, $4) "
            "ON CONFLICT (id) DO UPDATE SET "
            "code = EXCLUDED.code, "
            "validity_at = EXCLUDED.validity_at, "
            "created_at = EXCLUDED.created_at">>,
    imboy_pg:execute(Sql, [ToEmail, VerifyCode, ValidityAt, Now]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
