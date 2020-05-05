-module (imboy_db).

-export ([query/1]).
-export ([query/2]).

query(Sql) ->
    poolboy:transaction(mysql, fun(Pid) -> mysql:query(Pid, Sql) end).

query(Sql, Params) ->
    poolboy:transaction(mysql, fun(Pid) -> mysql:query(Pid, Sql, Params) end).
