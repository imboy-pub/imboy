-module(talk_record_transfer_candidates).
-moduledoc "移交责任人候选：owner门控，同企业/工作区资格，不排除已有授权。".
-export([for_record/5]).

for_record(Uid, Rid, Q, Cursor, Limit) when
    is_binary(Q), is_integer(Limit), Limit > 0, Limit =< 100
->
    case string:length(Q) =< 100 of
        true -> owner_candidates(Uid, Rid, Q, Cursor, Limit);
        false -> {error, invalid_filter}
    end;
for_record(_, _, _, _, _) ->
    {error, invalid_filter}.

owner_candidates(Uid, Rid, Q, Cursor, Limit) ->
    case talk_record_repo:find_visible(Uid, Rid) of
        {ok, #{<<"owner_user_id">> := Uid, <<"scope">> := <<"organization">>} = R} ->
            page(Uid, Rid, R, Q, Cursor, Limit);
        {ok, #{<<"owner_user_id">> := Uid}} ->
            {error, personal_not_transferable};
        {ok, _} ->
            {error, forbidden};
        Error ->
            Error
    end.

page(Uid, Rid, R, Q, Cursor, Limit) ->
    Binding = #{
        <<"uid">> => Uid,
        <<"record">> => Rid,
        <<"q">> => Q,
        <<"org">> => maps:get(<<"organization_id">>, R),
        <<"ws">> => maps:get(<<"workspace_id">>, R),
        <<"acl">> => maps:get(<<"acl_revision">>, R)
    },
    case talk_record_ds:decode_page_cursor(Cursor, <<"transfer_candidates">>, Binding) of
        {ok, Last} when Last =:= undefined; is_integer(Last) ->
            query(Uid, R, Binding, Q, Last, Limit);
        {error, key_unavailable} when Cursor =:= undefined; Cursor =:= <<>> ->
            query(Uid, R, Binding, Q, undefined, Limit);
        _ ->
            {error, cursor_invalid}
    end.

query(Uid, R, Binding, Q, Last, Limit) ->
    Pattern =
        case Q of
            <<>> -> <<>>;
            _ -> <<"%", (elib_pg:escape_like(Q))/binary, "%">>
        end,
    Spec = #{
        scope => <<"organization">>,
        actor_uid => Uid,
        organization_id => maps:get(<<"organization_id">>, R),
        workspace_id => maps:get(<<"workspace_id">>, R),
        exclude_uids => [],
        q => Pattern,
        cursor_id => Last,
        limit => Limit + 1
    },
    case elib_pg:with_conn(fun(C) -> talk_record_grant_repo:page_candidates_tx(C, Spec) end) of
        {ok, Rows} ->
            Page = lists:sublist(Rows, Limit),
            Next = next_cursor(length(Rows) > Limit, Page, Binding),
            {ok, #{<<"list">> => Page, <<"next_cursor">> => Next}};
        Error ->
            Error
    end.

next_cursor(false, _, _) ->
    null;
next_cursor(true, Page, Binding) ->
    case
        talk_record_ds:encode_cursor(
            <<"transfer_candidates">>,
            Binding,
            maps:get(<<"user_id">>, lists:last(Page))
        )
    of
        {ok, Cursor} -> Cursor;
        _ -> null
    end.
