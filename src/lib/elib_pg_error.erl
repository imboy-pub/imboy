-module(elib_pg_error).
-moduledoc "PostgreSQL error shape classification shared by repository boundaries.".
-export([sqlstate/1]).

%% Driver errors may be bare epgsql records or wrapped by an adapter.
-spec sqlstate(term()) -> binary() | undefined.
sqlstate({error, Inner}) when is_tuple(Inner), tuple_size(Inner) >= 3 ->
    sqlstate(Inner);
sqlstate(Error) when is_tuple(Error), tuple_size(Error) >= 3, element(1, Error) =:= error ->
    case element(3, Error) of
        Code when is_binary(Code), byte_size(Code) =:= 5 -> Code;
        _ -> undefined
    end;
sqlstate(_) ->
    undefined.
