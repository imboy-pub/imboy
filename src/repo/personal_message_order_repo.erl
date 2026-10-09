-module(personal_message_order_repo).
-moduledoc "个人消息持久接受顺序 / durable acceptance order for personal messages".
-export([stage_c2c/2]).

%% 分配序号与 staging INSERT 同事务；回滚/重复请求不消耗序号。
%% Sequence allocation and staging INSERT share a transaction; retries consume none.
-spec stage_c2c(binary(), map()) -> {ok, integer()} | {error, term()}.
stage_c2c(Table, Data) ->
    Key = msg_archive_repo:conv_key(<<"c2c">>, maps:get(from_id, Data), maps:get(to_id, Data)),
    case elib_pg:with_tx(fun(Conn) -> stage_tx(Conn, Table, Key, Data) end) of
        {rollback, {message_id_collision, OriginalError}} ->
            resolve_collision(Data, OriginalError);
        {rollback, Reason} ->
            {error, Reason};
        Result ->
            Result
    end.

stage_tx(Conn, Table, Key, Data) ->
    MsgId = maps:get(msg_id, Data),
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
    case query(Conn, identity_sql(), [MsgId]) of
        [] ->
            Seq = Previous + 1,
            {Sql, Params} = elib_pg_sql:insert(Table, Data#{conv_seq => Seq}),
            insert_staging(Conn, Sql, Params),
            execute(Conn, <<"UPDATE public.msg_store_seq SET seq = $2 WHERE conv_key = $1">>, [
                Key, Seq
            ]),
            {ok, maps:get(id, Data)};
        Rows ->
            {error, Reason} = identity_result(Data, Rows),
            throw({rollback, Reason})
    end.

%% 23505 等待获胜事务提交后才返回；先回滚，再读取其权威 from/to。
%% PG reports the unique collision after the winner commits; resolve only after rollback.
resolve_collision(Data, OriginalError) ->
    case elib_pg:query(identity_sql(), [maps:get(msg_id, Data)]) of
        {ok, []} -> {error, OriginalError};
        {ok, Rows} -> identity_result(Data, Rows);
        {error, Reason} -> {error, Reason}
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
        "SELECT from_id, to_id FROM public.msg_store_staging WHERE type = 'c2c' AND msg_id = $1 "
        "UNION ALL SELECT from_id, to_id FROM public.msg_c2c WHERE msg_id = $1 "
        "UNION ALL SELECT from_id, to_id FROM public.msg_store WHERE chat_type = 'c2c' AND msg_id = $1"
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
