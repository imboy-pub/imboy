-module(workspace_attachment_logic).
-export([search/3, open/3, filters/1, project/1]).

open(Uid, Ws, Id) ->
    case workspace_logic:ensure_member(Ws, Uid) of
        {ok, _} ->
            case workspace_attachment_ds:scan(Ws, #{keyword => <<>>, attachment_id => Id}) of
                {ok, [Row]} ->
                    case attach_logic:authorize(Uid, Row) of
                        true -> open_authorized(Uid, Row);
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

search_allowed(Uid, Ws, Input) ->
    case filters(Input) of
        {error, _} = Error ->
            Error;
        {ok, Filters} ->
            case workspace_attachment_ds:scan(Ws, Filters) of
                {ok, Rows} ->
                    Visible = [project(Row) || Row <- Rows, attach_logic:authorize(Uid, Row)],
                    Cursor =
                        case length(Rows) of
                            100 -> maps:get(<<"id">>, lists:last(Rows));
                            _ -> null
                        end,
                    {ok, #{workspace_id => Ws, items => Visible, next_cursor => Cursor}};
                {error, _} ->
                    {error, {503, <<"Attachment index unavailable">>}}
            end
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
        {ok, Numbers#{keyword => Pattern, user_keyword => UserPattern}}
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
            <<"retained_at">>
        ],
        Row
    ).
