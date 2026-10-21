-module(workspace_attachment_logic).
-export([search/3, search_organization/3, open/3, filters/1, project/1]).

open(Uid, Ws, Id) ->
    case workspace_logic:ensure_member(Ws, Uid) of
        {ok, _} ->
            case workspace_attachment_ds:scan(Ws, #{keyword => <<>>, attachment_id => Id}) of
                {ok, [Row]} ->
                    case visible(Uid, Row, false) of
                        {ok, VisibleRow} -> open_authorized(Uid, VisibleRow);
                        false -> {error, {404, <<"Attachment unavailable">>}}
                    end;
                {ok, _} ->
                    {error, {404, <<"Attachment unavailable">>}};
                {error, _} ->
                    {error, {503, <<"Attachment index unavailable">>}}
            end;
        {error, _} = Error ->
            Error
    end.

open_authorized(_Uid, #{<<"scope">> := <<"talk_record">>, <<"record_id">> := Record}) ->
    {ok, #{open_kind => <<"talk_record">>, record_id => Record}};
open_authorized(Uid, Row) ->
    case maps:get(<<"cipher">>, Row, null) of
        null -> attach_logic:view_url(Uid, maps:get(<<"path">>, Row));
        _ -> {error, {409, <<"Open encrypted attachments in the source conversation">>}}
    end.

search(Uid, Ws, Input) ->
    case workspace_logic:ensure_member(Ws, Uid) of
        {ok, _} -> search_allowed(Uid, Ws, Input);
        {error, _} = Error -> Error
    end.

search_organization(Uid, Org, Input) ->
    case organization_logic:detail(Uid, Org) of
        {ok, #{<<"status">> := <<"active">>}} -> search_index(Uid, Org, Input, true);
        {ok, _} -> {error, {403, <<"Organization unavailable">>}};
        {error, _} = Error -> Error
    end.

search_allowed(Uid, Ws, Input) -> search_index(Uid, Ws, Input, false).

search_index(Uid, Id, Input, IsOrganization) ->
    case filters(Input) of
        {error, _} = Error ->
            Error;
        {ok, Filters} ->
            Scan =
                case IsOrganization of
                    true -> workspace_attachment_ds:scan_organization(Id, Filters);
                    false -> workspace_attachment_ds:scan(Id, Filters)
                end,
            case Scan of
                {ok, Rows} ->
                    Allowed = [
                        R
                     || Row <- Rows,
                        {ok, R} <- [visible(Uid, Row, IsOrganization)]
                    ],
                    Visible = governance_projection(Uid, Allowed),
                    Cursor =
                        case length(Rows) of
                            100 -> maps:get(<<"id">>, lists:last(Rows));
                            _ -> null
                        end,
                    Scope =
                        case IsOrganization of
                            true -> #{organization_id => Id, workspace_id => null};
                            false -> #{workspace_id => Id}
                        end,
                    {ok, Scope#{items => Visible, next_cursor => Cursor}};
                {error, _} ->
                    {error, {503, <<"Attachment index unavailable">>}}
            end
    end.

governance_projection(Uid, Rows) ->
    Orgs = lists:usort([
        maps:get(<<"organization_id">>, R, null)
     || #{<<"scope">> := <<"talk_record">>} = R <- Rows
    ]),
    Grants = maps:from_list([
        {O, workspace_attachment_ds:can_govern(Uid, O)}
     || O <- Orgs, is_integer(O)
    ]),
    [
        project(
            case R of
                #{<<"scope">> := <<"talk_record">>} ->
                    R#{
                        <<"can_govern">> =>
                            maps:get(maps:get(<<"organization_id">>, R, null), Grants, false)
                    };
                _ ->
                    R
            end
        )
     || R <- Rows
    ].

visible(Uid, #{<<"scope">> := <<"talk_record">>, <<"record_id">> := Id} = Row, _) ->
    case workspace_attachment_ds:visible_record(Uid, Id) of
        {ok, Record} ->
            {ok, Row#{
                <<"source_name">> => maps:get(<<"title">>, Record),
                <<"open_kind">> => <<"talk_record">>
            }};
        _ ->
            false
    end;
visible(Uid, Row, IsOrganization) ->
    Member =
        case IsOrganization of
            true -> workspace_logic:ensure_member(maps:get(<<"workspace_id">>, Row), Uid);
            false -> {ok, member}
        end,
    case Member of
        {ok, _} ->
            case attach_logic:authorize(Uid, Row) of
                true -> {ok, Row};
                false -> false
            end;
        _ ->
            false
    end.

filters(Input) ->
    try
        Keys = [group_id, channel_id, user_id, from_ms, until_ms, cursor],
        Numbers = maps:from_list([
            {K, number(maps:get(atom_to_binary(K), Input, <<>>))}
         || K <- Keys
        ]),
        From = maps:get(from_ms, Numbers),
        Until = maps:get(until_ms, Numbers),
        true = From =:= null orelse Until =:= null orelse From < Until,
        true =
            maps:get(group_id, Numbers) =:= null orelse
                maps:get(channel_id, Numbers) =:= null,
        Source = maps:get(<<"source">>, Input, <<"all">>),
        true = lists:member(Source, [<<"all">>, <<"group">>, <<"channel">>, <<"talk_record">>]),
        true =
            Source =/= <<"talk_record">> orelse
                (maps:get(group_id, Numbers) =:= null andalso
                    maps:get(channel_id, Numbers) =:= null),
        Keyword = maps:get(<<"keyword">>, Input, <<>>),
        true = is_binary(Keyword) andalso byte_size(Keyword) =< 200,
        Pattern =
            case Keyword of
                <<>> -> <<>>;
                _ -> <<"%", (escape(Keyword))/binary, "%">>
            end,
        User = maps:get(<<"user_keyword">>, Input, <<>>),
        true = is_binary(User) andalso byte_size(User) =< 200,
        UserPattern =
            case User of
                <<>> -> <<>>;
                _ -> <<"%", (escape(User))/binary, "%">>
            end,
        {ok, Numbers#{keyword => Pattern, user_keyword => UserPattern, source => Source}}
    catch
        error:_ -> {error, {400, <<"Invalid attachment filters">>}}
    end.

number(<<>>) ->
    null;
number(Value) when is_binary(Value) ->
    N = binary_to_integer(Value),
    true = N > 0 andalso N =< 9223372036854775807,
    N.

escape(Value) ->
    lists:foldl(
        fun({From, To}, Acc) -> binary:replace(Acc, From, To, [global]) end,
        Value,
        [{<<"!">>, <<"!!">>}, {<<"%">>, <<"!%">>}, {<<"_">>, <<"!_">>}]
    ).

project(Row) ->
    maps:with(
        [
            <<"id">>,
            <<"name">>,
            <<"size">>,
            <<"mime_type">>,
            <<"created_at">>,
            <<"creator_user_id">>,
            <<"creator_name">>,
            <<"scope">>,
            <<"scope_ref">>,
            <<"source_name">>,
            <<"cipher">>,
            <<"retention_state">>,
            <<"retained_at">>,
            <<"organization_id">>,
            <<"workspace_id">>,
            <<"record_id">>,
            <<"open_kind">>,
            <<"can_govern">>,
            <<"attachment_role">>
        ],
        Row
    ).
