-module(imboy_migrate).


-export([migrate/0]).
-export([set_max_id_seq/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%------------------------------------------------------------------------------
%% migrate/0
%% 用于执行数据库迁移：读取 SQL 脚本并按顺序升级数据库结构
%%
%% imboy_migrate:migrate().
%% 升级相关sql文件必须是顺序的
%%------------------------------------------------------------------------------
migrate() ->
    Conf = config_ds:env(super_account),
    Path = config_ds:env(scripts_path),
    {ok, Conn} = epgsql:connect(Conf),
    MigrationCall =
      pure_migrations:migrate(
        Path,
        fun(F) -> epgsql:with_transaction(Conn, fun(_) -> F() end) end,
        fun(Q) ->
          case epgsql:squery(Conn, Q) of
            {ok, [{column, <<"version">>, _, _, _, _, _, _, _},
                   {column, <<"filename">>, _, _, _, _, _, _, _}], []} ->
                    [];
            {ok, [{column, <<"version">>, _, _, _, _, _, _, _},
                   {column, <<"filename">>, _, _, _, _, _, _, _}], Data} ->
                [{list_to_integer(binary_to_list(BinV)), binary_to_list(BinF)} || {BinV, BinF} <- Data];
            {ok, [{column, <<"max">>, _, _, _, _, _, _, _}], [{null}]} ->
                % It has to be -1 or it will get an error during initialization
                -1;
            {ok, [{column, <<"max">>, _, _, _, _, _, _, _}], [{N}]} ->
                % The version number is stored in the int4 type and ranges from -2,147,483,648 to 2,147,483,647
              list_to_integer(binary_to_list(N));

            {ok, [
              {column, <<"version">>, _, _, _, _, _},
              {column, <<"filename">>, _, _, _, _, _}], Data} ->
                [{list_to_integer(binary_to_list(BinV)), binary_to_list(BinF)} || {BinV, BinF} <- Data];
            {ok, [{column, <<"max">>, _, _, _, _, _}], [{null}]} -> -1;
            {ok, [{column, <<"max">>, _, _, _, _, _}], [{N}]} ->
              list_to_integer(binary_to_list(N));
            {ok, _, _} -> ok;
            {ok, _} -> ok;
            Default ->
                % io:format("DefaultDefaultDefaultDefaultDefault ~p, q: ~s~n", [Default, Q]),
                % Match multiple SQL statements in a script
                Res = priv_is_valid(Default),
                case Res of
                    true->
                        ok;
                    _ ->
                        Default
                end
          end
        end),
    % ...
    %% more preparation steps if needed
    % ...
    %% migration call
    Res = MigrationCall(),
    % imboy_log:debug(io:format("~p~n", [Res])),
    ok = epgsql:close(Conn),
    Res.

% imboy_migrate:set_max_id_seq().
set_max_id_seq() ->
    Conf = config_ds:env(super_account),
    Path = config_ds:env(scripts_path),
    {ok, Conn} = epgsql:connect(Conf),

    % Get the parent directory of Path
    ParentDir = filename:dirname(Path),
    File = filename:join(ParentDir, "set_max_id_seq.sql"),

    % Read the file content
    {ok, FileContent} = file:read_file(File),

    % Split into individual SQL statements and filter out comments/empty lines
    SqlStatements = lists:filtermap(
        fun(Stmt) ->
            case binary_to_list(Stmt) of
                "" -> false;
                [First|_] = Line ->
                    case First of
                        $# -> false;  % Skip shell-style comments
                        $- when length(Line) > 1, hd(tl(Line)) =:= $- -> false; % Skip SQL comments
                        _ -> {true, Line}
                    end
            end
        end,
        binary:split(FileContent, <<"\n">>, [global, trim])
    ),

    % Execute each statement and collect results
    {SuccessCount, FailCount} =
        lists:foldl(fun(Sql, {S, F}) ->
            io:format("Executing: ~s~n", [Sql]),
            case epgsql:squery(Conn, Sql) of
                {ok, _, _} = Success ->
                    io:format("Success: ~p~n", [Success]),
                    {S + 1, F};
                {error, Error} ->
                    io:format("Error: ~p~n", [Error]),
                    {S, F + 1};
                Other ->
                    io:format("Unexpected result: ~p~n", [Other]),
                    {S, F + 1}
            end
        end, {0, 0}, SqlStatements),

    ok = epgsql:close(Conn),
    #{success => SuccessCount, fail => FailCount}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

priv_is_valid(List) ->
    lists:all(fun(E) ->
        case E of
            {ok, _} -> true;
            {ok, _, _} -> true;
            _ -> false
        end
    end, List).
