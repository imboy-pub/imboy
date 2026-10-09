-module(personal_message_order_repo).
-moduledoc "个人消息持久接受顺序 / durable acceptance order for personal messages".
-export([
    stage_c2c/2, stage_c2c_ordered/2, execute_formal/5, execute_formal_tx/6, accepted_sequence/3
]).

%% 分配序号与 staging INSERT 同事务；回滚/重复请求不消耗序号。
%% Sequence allocation and staging INSERT share a transaction; retries consume none.
-spec stage_c2c(binary(), map()) -> {ok, integer()} | {error, term()}.
stage_c2c(Table, Data) ->
    case stage_c2c_ordered(Table, Data) of
        {ok, duplicate, _Seq} -> {error, {unique_violation, maps:get(msg_id, Data)}};
        {ok, Id, _Seq} -> {ok, Id};
        Error -> Error
    end.

%% Same transaction returns the trusted seq; delivery never rereads after ACK.
stage_c2c_ordered(Table, Data) ->
    Key = msg_archive_repo:conv_key(<<"c2c">>, maps:get(from_id, Data), maps:get(to_id, Data)),
    case elib_pg:with_tx(fun(Conn) -> stage_tx(Conn, Table, Key, Data) end) of
        {rollback, {accepted_duplicate, Seq}} ->
            {ok, duplicate, Seq};
        {rollback, {message_id_collision, OriginalError}} ->
            resolve_collision(Data, OriginalError);
        {rollback, Reason} ->
            {error, Reason};
        Result ->
            Result
    end.

stage_tx(Conn, Table, Key, Data) ->
    MsgId = maps:get(msg_id, Data),
    Previous = lock_order(Conn, Key, MsgId),
    case query(Conn, identity_sql(), [MsgId]) of
        [] ->
            Seq = Previous + 1,
            {Sql, Params} = elib_pg_sql:insert(Table, Data#{conv_seq => Seq}),
            insert_staging(Conn, Sql, Params),
            set_counter(Conn, Key, Seq),
            {ok, maps:get(id, Data), Seq};
        Rows ->
            Seq = existing_sequence(Data, Rows),
            throw({rollback, {accepted_duplicate, Seq}})
    end.

%% Counter -> msg_id matches the formal dedup trigger's existing namespace.
%% Each transaction accepts one conversation/message; no unordered batch locks.
lock_order(Conn, Key, MsgId) ->
    execute(
        Conn,
        <<
            "INSERT INTO public.msg_store_seq (conv_key, seq) VALUES ($1, 0) "
            "ON CONFLICT (conv_key) DO NOTHING"
        >>,
        [Key]
    ),
    [#{<<"seq">> := Previous}] = query(
        Conn,
        <<"SELECT seq FROM public.msg_store_seq WHERE conv_key = $1 FOR UPDATE">>,
        [Key]
    ),
    query(Conn, <<"SELECT pg_advisory_xact_lock(hashtextextended('msg_c2c:' || $1, 0))">>, [MsgId]),
    Previous.

set_counter(Conn, Key, Seq) ->
    execute(Conn, <<"UPDATE public.msg_store_seq SET seq = $2 WHERE conv_key = $1">>, [Key, Seq]).

%% SQL's final parameter is the server sequence, never an E2EE JSON field.
execute_formal(From, To, MsgId, Sql, Params) ->
    Data = #{from_id => From, to_id => To, msg_id => MsgId},
    Key = msg_archive_repo:conv_key(<<"c2c">>, From, To),
    case elib_pg:with_tx(fun(Conn) -> formal_tx(Conn, Key, Data, Sql, Params) end) of
        {rollback, formal_no_insert} -> {ok, 0};
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

%% Caller owns this transaction, including enterprise origin and audit writes.
%% Throws rollback on failure; never starts or commits a nested transaction.
execute_formal_tx(Conn, From, To, MsgId, Sql, Params) ->
    Key = msg_archive_repo:conv_key(<<"c2c">>, From, To),
    formal_tx(Conn, Key, #{from_id => From, to_id => To, msg_id => MsgId}, Sql, Params).

formal_tx(Conn, Key, Data, Sql, Params) ->
    Previous = lock_order(Conn, Key, maps:get(msg_id, Data)),
    Seq =
        case query(Conn, identity_sql(), [maps:get(msg_id, Data)]) of
            [] -> Previous + 1;
            Rows -> existing_sequence(Data, Rows)
        end,
    case elib_pg:execute(Conn, Sql, Params ++ [Seq]) of
        {ok, 0} ->
            throw({rollback, formal_no_insert});
        {ok, Count} ->
            set_counter(Conn, Key, max(Previous, Seq)),
            {ok, Count};
        {error, Reason} ->
            throw({rollback, Reason})
    end.

%% Read-only compatibility for a worker's pre-migration claimed NULL copy.
accepted_sequence(From, To, MsgId) ->
    Data = #{from_id => From, to_id => To, msg_id => MsgId},
    case elib_pg:query(identity_sql(), [MsgId]) of
        {ok, Rows} ->
            try
                {ok, existing_sequence(Data, Rows)}
            catch
                throw:{rollback, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

existing_sequence(Data, Rows) ->
    case identity_result(Data, Rows) of
        {error, msg_id_conflict} -> throw({rollback, msg_id_conflict});
        _ -> ok
    end,
    Seqs = lists:usort([
        maps:get(<<"conv_seq">>, R, null)
     || R <- Rows,
        maps:get(<<"conv_seq">>, R, null) =/= null
    ]),
    case Seqs of
        [Seq] when is_integer(Seq), Seq > 0 -> Seq;
        [] -> throw({rollback, c2c_sequence_missing});
        _ -> throw({rollback, c2c_sequence_conflict})
    end.

%% 23505 等待获胜事务提交后才返回；先回滚，再读取其权威 from/to。
%% PG reports the unique collision after the winner commits; resolve only after rollback.
resolve_collision(Data, OriginalError) ->
    case elib_pg:query(identity_sql(), [maps:get(msg_id, Data)]) of
        {ok, []} ->
            {error, OriginalError};
        {ok, Rows} ->
            try
                {ok, duplicate, existing_sequence(Data, Rows)}
            catch
                throw:{rollback, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

identity_result(Data, Rows) ->
    From = maps:get(from_id, Data),
    To = maps:get(to_id, Data),
    case
        lists:all(
            fun(Row) ->
                maps:get(<<"from_id">>, Row) =:= From andalso maps:get(<<"to_id">>, Row) =:= To
            end,
            Rows
        )
    of
        true -> {error, {unique_violation, maps:get(msg_id, Data)}};
        false -> {error, msg_id_conflict}
    end.

identity_sql() ->
    <<
        "SELECT from_id, to_id, conv_seq FROM public.msg_store_staging WHERE type = 'c2c' AND msg_id = $1 "
        "UNION ALL SELECT from_id, to_id, conv_seq FROM public.msg_c2c WHERE msg_id = $1 "
        "UNION ALL SELECT from_id, to_id, conv_seq FROM public.msg_store WHERE chat_type = 'c2c' AND msg_id = $1"
    >>.

insert_staging(Conn, Sql, Params) ->
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} ->
            ok;
        {error, {error, _, <<"23505">>, unique_violation, _, Extra} = Reason} ->
            case proplists:get_value(constraint_name, Extra) of
                <<"msg_store_staging_type_msg_id_key">> ->
                    throw({rollback, {message_id_collision, Reason}});
                _ ->
                    throw({rollback, Reason})
            end;
        {error, Reason} ->
            throw({rollback, Reason})
    end.

query(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> throw({rollback, Reason})
    end.

execute(Conn, Sql, Params) ->
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, Reason})
    end.
