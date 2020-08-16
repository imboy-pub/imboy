-module(chat_store_repo).
-include_lib("stdlib/include/qlc.hrl" ).

%% API
-export([dirty_insert/3, dirty_delete/1]).

-export([init/0, lookup/1, lookall/0]).

-record(chat_online_info, {pid, uid, socket_type}).

-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================
init()->
    % ok.
    dynamic_db_init().

%--------------------------------------------------------------------
%% @doc  dirty insert pid and socket
%% @spec  dirty_insert(Uid, Pid, socket)
%% @end
%%--------------------------------------------------------------------

dirty_insert(Uid, Pid, SocketType) when is_integer(Uid) ->
    dirty_insert(list_to_binary(integer_to_list(Uid)), Pid, SocketType);
dirty_insert(Uid, Pid, SocketType) when is_list(Uid) ->
    dirty_insert(list_to_binary(Uid), Pid, SocketType);
dirty_insert(Uid, Pid, SocketType) when is_pid(Pid) ->
    mnesia:dirty_write(
        #chat_online_info{
            pid = Pid,
            uid = Uid,
            socket_type = SocketType
        }
    ).

dirty_delete(Pid) when is_pid(Pid) ->
    mnesia:dirty_delete(chat_online_info, Pid);
dirty_delete(Uid) when is_integer(Uid) ->
    dirty_delete(list_to_binary(integer_to_list(Uid)));
dirty_delete(Uid) when is_list(Uid) ->
    dirty_delete(list_to_binary(Uid));
dirty_delete(Uid) ->
    [dirty_delete(Pid) || {chat_online_info, Pid, _, _} <- lookup(Uid)],
    ok.

%%--------------------------------------------------------------------
%% @doc Find a pid given a key.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%% @link https://blog.csdn.net/wudixiaotie/article/details/84735787  chat_store_repo:lookup(1).
%%--------------------------------------------------------------------
lookup(Pid) when is_pid(Pid)  ->
    mnesia:dirty_read(chat_online_info, Pid);
lookup(Uid) when is_integer(Uid)  ->
    lookup(list_to_binary(integer_to_list(Uid)));
lookup(Uid) when is_list(Uid)  ->
    lookup(list_to_binary(Uid));
lookup(Uid) ->
    mnesia:dirty_index_read(chat_online_info, Uid, #chat_online_info.uid).

%%--------------------------------------------------------------------
%% @doc Find all list
%% @spec lookall() -> {List} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookall() ->
    do(qlc:q([
        [
            X#chat_online_info.pid,
            X#chat_online_info.uid,
            X#chat_online_info.socket_type
        ] || X <- mnesia:table(chat_online_info)])).

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

    % 创建表 chat_online_info
    % 确保已经 mnesia:start().
    case lists:member(chat_online_info, mnesia:system_info(tables)) of
        false ->
            % 创建表
            mnesia:create_table(chat_online_info, [{type, set},
                {ram_copies, [node()|nodes()]}, % disc_copies 磁盘 + 内存; ram_copies 内存
                {attributes, record_info(fields, chat_online_info)}]
            ),
            mnesia:add_table_index(chat_online_info, uid);
            % mnesia:add_table_index(chat_online_info, socket_type);
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
