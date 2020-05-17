-module(chat_store_repo).
-include_lib("stdlib/include/qlc.hrl" ).

%% API
-export([dirty_insert/3, dirty_delete/1]).

-export([init/0, insert/3, delete/1, lookup/1, lookall/0]).

-record(chat_online_info, {uid, pid, socket_type}).

-include("imboy.hrl").

-define(WAIT_FOR_TABLES, 10000).

%%%===================================================================
%%% API
%%%===================================================================
init()->
    % ok.
    dynamic_db_init().

%--------------------------------------------------------------------
%% @doc Insert a key and pid.
%% @spec insert(Uid, Key, Pid) -> void()
%% @end
%%--------------------------------------------------------------------
insert(Uid, Pid, SocketType) when is_pid(Pid) ->
    Fun = fun() -> mnesia:write(
        #chat_online_info{
            uid = Uid,
            pid = Pid,
            socket_type = SocketType
        }
    ) end,
    {atomic, _} = mnesia:transaction(Fun).

%--------------------------------------------------------------------
%% @doc  dirty insert pid and socket
%% @spec  dirty_insert(Uid, Pid, socket)
%% @end
%%--------------------------------------------------------------------

dirty_insert(Uid, Pid, SocketType) when is_pid(Pid) ->
    mnesia:dirty_write(
        #chat_online_info{
            uid = Uid,
            pid = Pid,
            socket_type = SocketType
        }
    ).

dirty_delete(Pid) when is_pid(Pid)  ->
    mnesia:dirty_delete(chat_online_info, Pid);
dirty_delete(Uid) when is_integer(Uid)  ->
    mnesia:dirty_delete(chat_online_info, Uid).

%%--------------------------------------------------------------------
%% @doc Find a pid given a key.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Pid) when is_pid(Pid)  ->
    do(qlc:q([
        {
            X#chat_online_info.uid,
            X#chat_online_info.pid,
            X#chat_online_info.socket_type
        } || X <- mnesia:table(chat_online_info),X#chat_online_info.pid==Pid]));
lookup(Uid) when is_integer(Uid)  ->
    do(qlc:q([
        {
            X#chat_online_info.uid,
            X#chat_online_info.pid,
            X#chat_online_info.socket_type
        } || X <- mnesia:table(chat_online_info),X#chat_online_info.uid==Uid])).

%%--------------------------------------------------------------------
%% @doc Find all list
%% @spec lookall() -> {List} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookall() ->
    do(qlc:q([
        [
            X#chat_online_info.uid,
            X#chat_online_info.pid,
            X#chat_online_info.socket_type
        ] || X <- mnesia:table(chat_online_info)])).

%%--------------------------------------------------------------------
%% @doc Delete an element by pid from the registrar.
%% @spec delete(Pid) -> void()
%% @end
%%--------------------------------------------------------------------
delete(Pid) when is_pid(Pid) ->
    try
        [#chat_online_info{} = Record] = mnesia:dirty_read(chat_online_info, Pid, #chat_online_info.pid),
        mnesia:dirty_delete_object(Record)
    catch
        _C:_E -> ok
    end;
delete(Uid) when is_integer(Uid)  ->
    try
        [#chat_online_info{} = Record] = mnesia:dirty_read(chat_online_info, Uid, #chat_online_info.uid),
        mnesia:dirty_delete_object(Record)
    catch
        _C:_E -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
dynamic_db_init() ->
    % 不过 mnesia 是否启动这里都先停止它，便于下面初始化成功
    application:stop(mnesia),
    % mnesia检查数据库是否创建
    % 确保先创建 schema 之后再启动 mnesia
    case mnesia:system_info(use_dir) of
        true ->
            alread_created_schema;
        _ ->
            % mnesia:delete_schema([node()|nodes()])
            mnesia:create_schema([node()|nodes()])
    end,

    application:start(mnesia),

    % 创建表 ?CHAT_ONLINE_INFO
    % 确保已经 mnesia:start().
    case lists:member(?CHAT_ONLINE_INFO, mnesia:system_info(tables)) of
        false ->
            % 创建表
            mnesia:create_table(?CHAT_ONLINE_INFO, [{type, set},
                {ram_copies, [node()|nodes()]}, % disc_copies 磁盘 + 内存; ram_copies 内存
                {attributes, record_info(fields, ?CHAT_ONLINE_INFO)}]
            );
        true ->
            alread_created_table
    end,
    % 暂停10毫秒，等待创建、启动mnesia数据库
    timer:sleep(10),
    ok.

do(Query) ->
    F = fun() -> qlc:e(Query) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.
