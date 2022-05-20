-module(mysql_pool).

-include_lib("imboy/include/common.hrl").

-export([execute/1]).
-export([execute/2]).
-export([query/1]).
-export([query/2]).
-export([insert_into/3]).
-export([replace_into/3]).
-export([assemble_sql/4]).
-export([update/4]).


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

-spec execute(Sql :: any()) ->
          {ok, LastInsertId :: integer()} | {error, any()}.
execute(Sql) ->
    poolboy:transaction(mysql,
                        fun(Pid) ->
                               case mysql:query(Pid, Sql) of
                                   ok ->
                                       {ok, mysql:insert_id(Pid)};
                                   Res ->
                                       Res
                               end
                        end).


execute(Sql, Params) ->
    poolboy:transaction(mysql,
                        fun(Pid) ->
                               case mysql:query(Pid, Sql, Params) of
                                   ok ->
                                       {ok, mysql:insert_id(Pid)};
                                   Res ->
                                       Res
                               end
                        end).


query(Sql) ->
    poolboy:transaction(mysql, fun(Pid) -> mysql:query(Pid, Sql) end).


query(Sql, Params) ->
    poolboy:transaction(mysql,
                        fun(Pid) -> mysql:query(Pid, Sql, Params) end).


replace_into(Table, Column, Value) ->
    % Sql like this "REPLACE INTO foo (k,v) VALUES (1,0), (2,0)"
    insert(<<"REPLACE INTO">>, Table, Column, Value).


insert_into(Table, Column, Value) ->
    % Sql like this "INSERT INTO foo (k,v) VALUES (1,0), (2,0)"
    insert(<<"INSERT INTO">>, Table, Column, Value).


assemble_sql(Prefix, Table, Column, Value) ->
    Sql = <<Prefix/binary, " ", Table/binary, " ", Column/binary,
            " VALUES ", Value/binary>>,
    % ?LOG(Sql),
    Sql.

-spec update(Table::binary(), ID::any(), Field::binary(), list() | binary()) ->
    ok | {error,  {integer(), binary(), Msg::binary()}}.
update(Table, ID, Field, Value) when is_list(Value) ->
    update(Table, ID, Field, list_to_binary(Value));
update(Table, ID, Field, Value) ->
    Sql = <<"UPDATE `", Table/binary,"` SET `",
        Field/binary, "` = ? WHERE `id` = ?">>,
    mysql_pool:query(Sql, [unicode:characters_to_binary(Value), ID]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

insert(Prefix, Table, Column, Value) ->
    Sql = assemble_sql(Prefix, Table, Column, Value),
    % ?LOG(Sql),
    poolboy:transaction(mysql, fun(Pid) -> mysql:query(Pid, Sql) end).


