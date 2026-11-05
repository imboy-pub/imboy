-module(attachment_avatar_publication_repo).
-moduledoc "头像公开复制意图；未知远端结果保留源附件。".
-export([reserve/2, start_copy/1, finish_copy/3, discard_reserved/1]).

%% This transaction commits before scanning or Copy. Source and destination
%% identity cannot be changed on retry.
reserve(ObjectKey, PublicBucket) when
    is_binary(ObjectKey),
    byte_size(ObjectKey) > 0,
    is_binary(PublicBucket),
    byte_size(PublicBucket) > 0
->
    elib_pg:with_tx(
        fun(C) ->
            case attachment_repo:find_by_path_for_update_tx(C, ObjectKey) of
                {ok, #{<<"status">> := 1, <<"lifecycle_state">> := <<"live">>} = Row} ->
                    case attachment_repo:verify_direct_winner_tx(C, ObjectKey) of
                        ok -> reserve_row(C, Row, PublicBucket);
                        Error -> Error
                    end;
                {ok, _} ->
                    {error, avatar_not_validatable};
                {error, _} = Error ->
                    Error
            end
        end,
        [{reraise, false}]
    );
reserve(_, _) ->
    {error, invalid_avatar_publication}.

reserve_row(
    C, #{<<"id">> := Aid, <<"path">> := Key, <<"storage_container">> := Source} = Row, Bucket
) when
    is_binary(Source), Source =/= Bucket
->
    case
        elib_pg:query(
            C,
            <<
                "INSERT INTO public.attachment_avatar_publication"
                " (attachment_id, object_key, public_bucket, public_key) VALUES ($1,$2,$3,$2)"
                " ON CONFLICT (attachment_id) DO NOTHING RETURNING state"
            >>,
            [Aid, Key, Bucket]
        )
    of
        {ok, [#{<<"state">> := State}]} -> {ok, Row, State};
        {ok, []} -> existing_row(C, Row, Bucket);
        {error, _} = Error -> Error
    end;
reserve_row(_, _, _) ->
    {error, avatar_bucket_not_private}.

existing_row(C, #{<<"id">> := Aid, <<"path">> := Key} = Row, Bucket) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT state FROM public.attachment_avatar_publication"
                " WHERE attachment_id=$1 AND object_key=$2 AND public_bucket=$3 AND public_key=$2"
                " FOR UPDATE"
            >>,
            [Aid, Key, Bucket]
        )
    of
        {ok, [#{<<"state">> := State}]} -> {ok, Row, State};
        {ok, []} -> {error, avatar_publication_identity_conflict};
        {error, _} = Error -> Error
    end.

%% unknown commits before dispatch and cannot be dispatched a second time.
start_copy(Aid) ->
    elib_pg:with_tx(
        fun(C) ->
            case
                elib_pg:query(
                    C,
                    <<
                        "SELECT a.path FROM public.attachment a"
                        " JOIN public.attachment_avatar_publication p ON p.attachment_id=a.id"
                        " WHERE a.id=$1 AND a.lifecycle_state='live' AND a.status=1"
                        " AND p.state='reserved' FOR UPDATE OF a,p"
                    >>,
                    [Aid]
                )
            of
                {ok, [#{<<"path">> := Key}]} ->
                    case attachment_repo:verify_direct_winner_tx(C, Key) of
                        ok -> mark_unknown(C, Aid);
                        Error -> Error
                    end;
                {ok, []} ->
                    {error, avatar_publication_not_retryable};
                {error, _} = Error ->
                    Error
            end
        end,
        [{reraise, false}]
    ).

mark_unknown(C, Aid) ->
    case
        elib_pg:query(
            C,
            <<
                "UPDATE public.attachment_avatar_publication SET state='unknown',"
                " dispatch_token=gen_random_uuid(),updated_at=now()"
                " WHERE attachment_id=$1 AND state='reserved' RETURNING dispatch_token::text"
            >>,
            [Aid]
        )
    of
        {ok, [#{<<"dispatch_token">> := Token}]} when is_binary(Token) -> {ok, Token};
        {ok, _} -> {error, avatar_publication_not_retryable};
        {error, _} = Error -> Error
    end.

%% Only a storage preflight failure can reset this exact dispatch.
finish_copy(Aid, Token, {error, {not_dispatched, Reason}}) ->
    case settle(Aid, Token, <<"reserved">>) of
        ok -> {error, {not_dispatched, Reason}};
        {error, _} = Error -> Error
    end;
finish_copy(Aid, Token, {ok, _} = Result) ->
    case settle(Aid, Token, <<"complete">>) of
        ok -> Result;
        {error, _} = Error -> Error
    end;
finish_copy(_, _, {error, _} = Error) ->
    Error.

settle(Aid, Token, State) when is_binary(Token), byte_size(Token) =:= 36 ->
    case
        elib_pg:query(
            <<
                "UPDATE public.attachment_avatar_publication SET state=$3,"
                " dispatch_token=CASE WHEN $3='reserved' THEN NULL ELSE dispatch_token END,"
                " updated_at=now() WHERE attachment_id=$1 AND state='unknown'"
                " AND dispatch_token=$2::uuid RETURNING state"
            >>,
            [Aid, Token, State]
        )
    of
        {ok, [#{<<"state">> := State}]} -> ok;
        _ -> {error, avatar_publication_unsettled}
    end;
settle(_, _, _) ->
    {error, avatar_publication_unsettled}.

%% Only a never-dispatched reservation may be discarded after scan failure.
discard_reserved(Aid) ->
    case
        elib_pg:execute(
            <<
                "DELETE FROM public.attachment_avatar_publication"
                " WHERE attachment_id=$1 AND state='reserved'"
            >>,
            [Aid]
        )
    of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.
