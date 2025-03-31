-module(verification_code_repo).
%%%
% verification_code_repo 是 verification_code repository 缩写
%%%

-export([tablename/0]).
-export([find_by_id/1]).
-export([save/4]).


tablename() ->
    imboy_db:public_tablename(<<"verification_code">>).


-spec find_by_id(binary()) -> {ok, ColumnList :: list(), Values :: list()}.
find_by_id(Id) ->
    Tb = tablename(),
    Column = <<"id,code,validity_at,created_at">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    imboy_db:query(Sql, [Id]).


% verification_code_repo:save(<<"test@imboy.pub">>, imboy_func:num_random(6), imboy_dt:add(imboy_dt:now(), {10, minute}), imboy_dt:now()).
-spec save(binary(), integer(), binary(), binary()) -> ok.
save(ToEmail, VerifyCode, ValidityAt, Now) ->
    Tb = tablename(),
    Column = <<"(id,code,validity_at,created_at)">>,
    VerifyCode2 = integer_to_binary(VerifyCode),

    UpSql = <<" UPDATE SET code = ", VerifyCode2/binary, ", validity_at = '", ValidityAt/binary, "', created_at = '", Now/binary, "'">>,

    Value = <<"('", ToEmail/binary, "', '", VerifyCode2/binary, "', '", ValidityAt/binary, "', '", Now/binary,
              "') ON CONFLICT (id) DO ", UpSql/binary>>,
    Sql = imboy_db:assemble_sql(<<"INSERT INTO">>, Tb, Column, Value),
    imboy_db:execute(Sql, []).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
