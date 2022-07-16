-module(mysql_pool).

-export([execute/1]).
-export([execute/2]).
-export([query/1]).
-export([query/2]).
-export([insert_into/3]).
-export([replace_into/3]).
-export([assemble_sql/4]).
-export([update/3]).
-export([update/4]).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/log.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

-spec execute(Sql :: any()) ->
          {ok, LastInsertId :: integer()} | {error, any()}.
execute(Sql) ->
    poolboy:transaction(mysql, fun(Pid) ->
           case mysql:query(Pid, Sql) of
               ok ->
                   {ok, mysql:insert_id(Pid)};
               Res ->
                   Res
           end
    end).


execute(Sql, Params) ->
    poolboy:transaction(mysql, fun(Pid) ->
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
    poolboy:transaction(mysql, fun(Pid) ->
        mysql:query(Pid, Sql, Params)
    end).


replace_into(Table, Column, Value) ->
    % Sql like this "REPLACE INTO foo (k,v) VALUES (1,0), (2,0)"
    insert(<<"REPLACE INTO">>, Table, Column, Value).


insert_into(Table, Column, Value) ->
    % Sql like this "INSERT INTO foo (k,v) VALUES (1,0), (2,0)"
    insert(<<"INSERT INTO">>, Table, Column, Value).

% 组装 SQL 语句
assemble_sql(Prefix, Table, Column, Value) ->
    Sql = <<Prefix/binary, " ", Table/binary, " ", Column/binary,
            " VALUES ", Value/binary>>,
    % ?LOG(io:format("~s\n", [Sql])),
    Sql.

% mysql_pool:update(<<"user">>, "1", <<"sign">>, <<"中国你好！😆"/utf8>>).
% mysql_pool:update(<<"user">>, "1", <<"sign">>, "中国你好！😆").
-spec update(binary(), any(), binary(), list() | binary()) ->
    ok | {error,  {integer(), binary(), Msg::binary()}}.
update(Table, ID, Field, Value) when is_list(Value) ->
    update(Table, ID, Field, unicode:characters_to_binary(Value));
update(Table, ID, Field, Value) ->
    Sql = <<"UPDATE `", Table/binary,"` SET `",
        Field/binary, "` = ? WHERE `id` = ?">>,
    mysql_pool:query(Sql, [Value, ID]).


-spec update(Table::binary(), ID::any(), KV::list()) ->
    ok | {error,  {integer(), binary(), Msg::binary()}}.
update(Table, ID, KV) ->
    KV2 = [{K, update_filter_value(V)} || {K, V} <- KV],
    Set1 = [<<K/binary, " = '", V/binary, "'">> || {K, V} <- KV2],
    Set2 = [binary_to_list(S) || S <- Set1],
    Set3 = lists:concat(lists:join(", ", Set2)),
    Set4 = list_to_binary(Set3),
    Sql = <<"UPDATE `", Table/binary,"` SET ", Set4/binary," WHERE `id` = ?">>,
    % ?LOG(io:format("~s\n", [Sql])),
    mysql_pool:query(Sql, [ID]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


update_filter_value(Val) when is_binary(Val) ->
    Val;
update_filter_value(Val) ->
    unicode:characters_to_binary(Val).

insert(Prefix, Table, Column, Value) ->
    Sql = assemble_sql(Prefix, Table, Column, Value),
    poolboy:transaction(mysql, fun(Pid) -> mysql:query(Pid, Sql) end).


%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).

updateuser_test_() ->
    KV1 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, <<"中国你好！😆"/utf8>>}],
    KV2 = [{<<"gender">>, <<"1">>}, {<<"nickname">>, "中国你好！😆😆"}],

    [
        ?_assert(mysql_pool:update(<<"user">>, 1, KV1)),
        ?_assert(mysql_pool:update(<<"user">>, 2, KV2))
    ].

-endif.
