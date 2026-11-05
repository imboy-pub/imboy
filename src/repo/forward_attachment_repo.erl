-module(forward_attachment_repo).
-moduledoc "受控附件转发的事务引用和目标会话读授权。".
-export([
    with_context/2, mark_committed/2, accept_tx/6, authorize/2, transaction/1, writable_group_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(CONTEXT, {?MODULE, context}).

%% Only the server-side forward entry installs this; clients cannot supply an atom key.
with_context(Context, Fun) when is_map(Context), is_function(Fun, 0) ->
    case get(?CONTEXT) of
        undefined ->
            put(?CONTEXT, Context),
            try
                Result = Fun(),
                case {Result, get(?CONTEXT)} of
                    {{ok, MsgId}, #{committed := MsgId}} -> Result;
                    {{ok, _}, _} -> {error, forward_reference_not_committed};
                    _ -> Result
                end
            after
                erase(?CONTEXT)
            end;
        _ ->
            {error, nested_forward_context}
    end.

%% Called only after the target transaction returns successfully.
mark_committed(Type, MsgId) ->
    case get(?CONTEXT) of
        #{target_type := Type} = Context ->
            put(?CONTEXT, Context#{committed => MsgId}),
            ok;
        _ ->
            ok
    end.

%% Forwarding may lock a target group before its source group. Retry only a
%% PostgreSQL-detected deadlock after an acknowledged rollback; no commit or
%% external delivery occurs inside this transaction.
transaction(Fun) when is_function(Fun, 1) ->
    case get(?CONTEXT) of
        undefined ->
            elib_pg:with_tx(Fun);
        _ ->
            elib_pg:with_deadlock_retry_tx(fun(Conn) ->
                try
                    Fun(Conn)
                catch
                    throw:{abort_tx,
                        {discussion_access_query_failed, #error{code = <<"40P01">>} = E}} ->
                        throw({abort_tx, E})
                end
            end)
    end.

writable_group_tx(Conn, Gid) ->
    Result =
        case get(?CONTEXT) of
            undefined -> workspace_guard:ensure_writable_tx(Conn, {group, Gid});
            _ -> workspace_guard:ensure_writable_tx_retryable(Conn, {group, Gid})
        end,
    workspace_guard:abort_on_error(Result).

%% Called inside the target staging transaction, after the target row/snapshot exist.
accept_tx(Conn, Type, MsgId, Sender, Target, Seq) ->
    case get(?CONTEXT) of
        undefined ->
            ok;
        #{target_type := Type, target_id := Target, sender := Sender} = Ctx ->
            ensure(source_validate(Conn, Ctx)),
            lists:foreach(
                fun(Key) ->
                    ensure(attachment_repo:bind_reference_lock_tx(Conn, Key)),
                    ensure(check_attachment(Conn, Key, Ctx)),
                    ensure(insert_ref(Conn, Key, Ctx, Type, MsgId, Sender, Target, Seq))
                end,
                lists:usort(maps:get(keys, Ctx))
            ),
            ensure_forward_record(Conn, Ctx, Type, MsgId, Sender, Target),
            ok;
        _ ->
            throw({rollback, forward_context_mismatch})
    end.

ensure_forward_record(Conn, Ctx, Type, MsgId, Sender, Target) ->
    Record = #{
        <<"original_msg_id">> => maps:get(source_id, Ctx),
        <<"original_from_id">> => maps:get(source_from, Ctx),
        <<"original_to_id">> => maps:get(source_target, Ctx),
        <<"original_type">> => maps:get(source_type, Ctx),
        <<"forward_msg_id">> => MsgId,
        <<"forward_from_id">> => Sender,
        <<"forward_to_id">> => Target,
        <<"forward_type">> => Type
    },
    case msg_forward_repo:insert_tx(Conn, Record) of
        {ok, _} -> ok;
        {error, Reason} -> ensure({error, Reason})
    end.

source_validate(Conn, #{
    source_type := <<"c2c">>,
    source_id := Id,
    sender := Uid,
    source_from := From,
    source_target := To,
    source_msg_type := Type,
    source_payload := Payload
}) ->
    case normalized(Payload) of
        M when is_map(M) ->
            case
                elib_pg:query(
                    Conn,
                    <<"SELECT public.fn_forward_source_c2c_validate($1,$2,$3,$4,$5,$6::jsonb) AS allowed">>,
                    [Id, Uid, From, To, Type, jsone:encode(M)]
                )
            of
                {ok, [#{<<"allowed">> := true}]} -> ok;
                {ok, _} -> {error, forbidden};
                {error, _} = Error -> Error
            end;
        _ ->
            {error, forbidden}
    end;
source_validate(Conn, #{
    source_type := <<"c2g">>,
    source_id := Id,
    sender := Uid,
    source_from := From,
    source_target := Gid,
    source_msg_type := Type,
    source_payload := Payload
}) ->
    case normalized(Payload) of
        M when is_map(M) ->
            Discussion = discussion_enabled(),
            case
                elib_pg:query(
                    Conn,
                    <<"SELECT public.fn_forward_source_c2g_validate($1,$2,$3,$4,$5,$6::jsonb,$7) AS allowed">>,
                    [Id, Uid, From, Gid, Type, jsone:encode(M), Discussion]
                )
            of
                {ok, [#{<<"allowed">> := true}]} -> ok;
                {ok, _} -> {error, forbidden};
                {error, _} = Error -> Error
            end;
        _ ->
            {error, forbidden}
    end;
source_validate(_, _) ->
    {error, forbidden}.

normalized(Value) when is_map(Value) -> Value;
normalized(Value) when is_binary(Value) ->
    try
        jsone:decode(Value, [{object_format, map}])
    catch
        _:_ -> invalid
    end;
normalized(_) ->
    invalid.

check_attachment(Conn, Key, Ctx) ->
    case attachment_repo:find_by_path_for_update_tx(Conn, Key) of
        {ok, #{<<"status">> := 1, <<"lifecycle_state">> := <<"live">>} = Row} ->
            case attachment_repo:verify_direct_winner_tx(Conn, Key) of
                ok -> source_scope(Conn, Key, Row, Ctx);
                Error -> Error
            end;
        {ok, _} ->
            {error, forbidden};
        {error, _} = Error ->
            Error
    end.

source_scope(_Conn, _Key, #{<<"scope">> := <<"public">>}, _Ctx) ->
    ok;
source_scope(
    _Conn,
    _Key,
    #{<<"scope">> := <<"private">>, <<"creator_user_id">> := Uid},
    #{sender := Uid}
) ->
    ok;
source_scope(
    Conn,
    Key,
    #{<<"scope">> := <<"group">>, <<"scope_ref">> := Ref},
    #{source_type := <<"c2g">>, source_target := Gid, sender := Uid}
) ->
    case Ref =:= integer_to_binary(Gid) of
        true ->
            Sql = attachment_repo:group_access_sql(attachment_repo:tablename()),
            case elib_pg:query(Conn, Sql, [Key, Uid]) of
                {ok, [#{<<"allowed">> := true}]} -> ok;
                {ok, _} -> source_ref(Conn, Key, <<"c2g">>, Gid, Uid);
                {error, _} = Error -> Error
            end;
        false ->
            source_ref(Conn, Key, <<"c2g">>, Gid, Uid)
    end;
source_scope(
    Conn,
    Key,
    #{<<"scope">> := <<"c2c">>, <<"scope_ref">> := Ref},
    #{source_type := <<"c2c">>, source_from := From, source_target := To, sender := Uid}
) ->
    case conv_key_vo:c2c_members(Ref) of
        {ok, {A, B}} when min(A, B) =:= min(From, To), max(A, B) =:= max(From, To) -> ok;
        _ -> source_ref(Conn, Key, <<"c2c">>, To, Uid)
    end;
source_scope(Conn, Key, _Row, #{source_type := Type, source_target := Target, sender := Uid}) ->
    source_ref(Conn, Key, Type, Target, Uid).

%% The source gate has already revalidated the source message's current audience.
source_ref(Conn, Key, Type, _Target, Uid) ->
    Ctx = get(?CONTEXT),
    Id = maps:get(source_id, Ctx),
    Sql = <<
        "SELECT EXISTS (SELECT 1 FROM public.forward_attachment_reference"
        " WHERE object_key=$1 AND target_type=$2 AND target_msg_id=$3"
        " AND (target_type='c2g' OR target_sender=$4 OR target_id=$4)) AS allowed"
    >>,
    case elib_pg:query(Conn, Sql, [Key, Type, Id, Uid]) of
        {ok, [#{<<"allowed">> := true}]} -> ok;
        {ok, _} -> {error, forbidden};
        {error, _} = Error -> Error
    end.

insert_ref(Conn, Key, Ctx, Type, MsgId, Sender, Target, Seq) ->
    Sql =
        <<"SELECT public.fn_forward_attachment_bind($1,$2,$3,$4,$5,$6,$7::jsonb,$8,$9,$10,$11,$12,$13) AS accepted">>,
    Params = [
        Key,
        maps:get(source_type, Ctx),
        maps:get(source_id, Ctx),
        maps:get(source_from, Ctx),
        maps:get(source_target, Ctx),
        maps:get(source_msg_type, Ctx),
        jsone:encode(normalized(maps:get(source_payload, Ctx))),
        Type,
        MsgId,
        Sender,
        Target,
        Seq,
        discussion_enabled()
    ],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"accepted">> := true}]} -> ok;
        {ok, _} -> {error, forward_reference_failed};
        {error, _} = Error -> Error
    end.

ensure(ok) -> ok;
ensure({error, #error{code = <<"40P01">>} = Reason}) -> throw({rollback, Reason});
ensure({error, Reason}) -> throw({rollback, {forward_attachment, Reason}}).

%% The SECURITY DEFINER function reads only the target audience and parent policy.
authorize(Key, Uid) when is_binary(Key), is_integer(Uid), Uid > 0 ->
    DiscussionEnabled = discussion_enabled(),
    case
        elib_pg:query(
            <<"SELECT public.fn_forward_attachment_authorize($1,$2,$3) AS allowed">>,
            [Key, Uid, DiscussionEnabled]
        )
    of
        {ok, [#{<<"allowed">> := true}]} -> true;
        _ -> false
    end;
authorize(_, _) ->
    false.

discussion_enabled() ->
    try
        imboy_policy:current_profile() =/= phase1_delivery andalso imboy_feature:enabled(channel)
    catch
        _:_ -> false
    end.
