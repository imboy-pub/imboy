-module(verification_code_repo).
%%%
% verification_code_repo 是 verification_code repository 缩写
%%%

-export([tablename/0]).
-export([get_by_id/1]).
-export([save/4]).

tablename() ->
    imboy_db:public_tablename(<<"verification_code">>).

-spec get_by_id(binary()) -> {ok, ColumnList :: list(), Values :: list()}.
get_by_id(Id) ->
    Tb = tablename(),
    Column = <<"id,code,validity_at,created_at">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary," WHERE id = $1">>,
    imboy_db:query(Sql, [Id]).


% verification_code_repo:save(<<"test@imboy.pub">>,
%   imboy_func:num_random(6),
%   imboy_dt:millisecond() + 600000,
%   imboy_dt:millisecond()).
-spec save(binary(), integer(), integer(), integer()) -> ok.
save(ToEmail, VerifyCode, ValidityAt, Now) ->
    Tb = tablename(),
    Column = <<"(id,code,validity_at,created_at)">>,
    VerifyCode2 = integer_to_binary(VerifyCode),
    ValidityAt2 = integer_to_binary(ValidityAt),
    Now2 = integer_to_binary(Now),

    UpSql = <<" UPDATE SET code = ", VerifyCode2/binary, ", validity_at = ",
        ValidityAt2/binary, ", created_at = ", Now2/binary, ";">>,

    Value = <<"('", ToEmail/binary, "', '", VerifyCode2/binary, "', '"
        , ValidityAt2/binary, "', '", Now2/binary
        , "') ON CONFLICT (id) DO ", UpSql/binary>>,
    Sql = imboy_db:assemble_sql(<<"INSERT INTO">>, Tb, Column, Value),
    imboy_db:execute(Sql, []).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
