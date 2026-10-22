-module(enterprise_directory_source_read_logic).
-moduledoc "Read-only source reconciliation, with signed application/organization/filter-bound cursors. No writes"
" or account provisioning.".
-export([page/3, page_tx/4, validate/2]).

page(Ctx, Kind, Query) ->
    elib_pg:with_tx(fun(Conn) -> page_tx(Conn, Ctx, Kind, Query) end).

page_tx(Conn, Ctx, Kind, Query) ->
    case {validate(Kind, Query), enterprise_cursor_v2:signing_key()} of
        {{error, _} = Error, _} -> Error;
        {_, {error, _}} -> {error, security_gate_closed};
        {{ok, Opts}, {ok, Key}} -> authorized_page(Conn, Ctx, Kind, Opts, Key)
    end.

validate(Kind, Query) when is_map(Query) ->
    try
        true = lists:member(Kind, [organizations, departments, persons]),
        true = maps:keys(Query) -- allowed_keys(Kind) =:= [],
        Filter = maps:without([<<"page_size">>, <<"cursor">>], Query),
        validate_filter(Kind, Filter),
        Size = size_param(maps:get(<<"page_size">>, Query, <<"50">>)),
        Cursor = maps:get(<<"cursor">>, Query, undefined),
        true =
            Cursor =:= undefined orelse
                (is_binary(Cursor) andalso byte_size(Cursor) > 0 andalso byte_size(Cursor) =< 8192),
        {ok, #{filter => Filter, page_size => Size, cursor => Cursor}}
    catch
        error:_ -> {error, invalid_request}
    end;
validate(_, _) ->
    {error, invalid_request}.

allowed_keys(organizations) ->
    [<<"cursor">>, <<"page_size">>];
allowed_keys(departments) ->
    [
        <<"cursor">>,
        <<"page_size">>,
        <<"external_organization_id">>,
        <<"external_department_id">>,
        <<"status">>
    ];
allowed_keys(persons) ->
    [
        <<"cursor">>,
        <<"page_size">>,
        <<"external_organization_id">>,
        <<"external_user_id">>,
        <<"source_status">>,
        <<"membership_status">>,
        <<"projection_status">>
    ].

validate_filter(organizations, _) ->
    ok;
validate_filter(Kind, Filter) ->
    text(maps:get(<<"external_organization_id">>, Filter)),
    maps:foreach(
        fun
            (<<"external_organization_id">>, _) ->
                ok;
            (<<"external_department_id">>, V) ->
                text(V);
            (<<"external_user_id">>, V) ->
                text(V);
            (<<"projection_status">>, V) ->
                true = lists:member(V, [
                    <<"unlinked">>,
                    <<"pending">>,
                    <<"projected">>,
                    <<"removed">>,
                    <<"retired">>
                ]);
            (_, V) ->
                true = lists:member(V, [<<"active">>, <<"removed">>])
        end,
        Filter
    ),
    true = Kind =:= departments orelse Kind =:= persons.

text(V) ->
    true = is_binary(V) andalso byte_size(V) > 0 andalso byte_size(V) =< 256,
    true = is_list(unicode:characters_to_list(V)).

size_param(V) when is_binary(V), byte_size(V) > 0, byte_size(V) =< 3 ->
    true = re:run(V, <<"^[1-9][0-9]*$">>, [{capture, none}]) =:= match,
    N = binary_to_integer(V),
    true = N =< 100,
    N.

authorized_page(Conn, Ctx, Kind, Opts, Key) ->
    case context(Conn, Ctx, Kind, maps:get(filter, Opts)) of
        {ok, BoundCtx} -> read_page(Conn, BoundCtx, Kind, Opts, Key);
        Error -> Error
    end.

context(_, Ctx, organizations, _) ->
    {ok, Ctx#{organization_id => 0}};
context(Conn, Ctx, _, Filter) ->
    External = maps:get(<<"external_organization_id">>, Filter),
    case enterprise_directory_source_read_repo:organization_tx(Conn, Ctx, External) of
        {ok, #{<<"organization_id">> := OrgId}} -> {ok, Ctx#{organization_id => OrgId}};
        Error -> Error
    end.

read_page(Conn, Ctx, Kind, #{filter := Filter, page_size := Size} = Opts, Key) ->
    case pivot(maps:get(cursor, Opts), Ctx, Kind, Filter, Key) of
        {ok, After} ->
            case
                enterprise_directory_source_read_repo:page_tx(
                    Conn, Kind, Ctx, Filter, After, Size + 1
                )
            of
                {ok, Rows} -> result(Ctx, Kind, Filter, Size, Rows, Key);
                {error, _} -> {error, internal_error}
            end;
        Error ->
            Error
    end.

pivot(undefined, _, _, _, _) ->
    {ok, null};
pivot(Cursor, Ctx, Kind, Filter, Key) ->
    case enterprise_cursor_v2:verify(Cursor, Key) of
        {ok, P} ->
            Bound =
                maps:get(<<"family">>, P, undefined) =:= family(Kind) andalso
                    maps:get(<<"application_id">>, P, undefined) =:= maps:get(application_id, Ctx) andalso
                    maps:get(<<"organization_id">>, P, undefined) =:= maps:get(organization_id, Ctx) andalso
                    maps:get(<<"filter">>, P, undefined) =:= Filter,
            cursor_key(Bound, maps:get(<<"sort_tuple">>, P, []));
        {error, _} ->
            {error, invalid_request}
    end.

cursor_key(true, [Last]) ->
    try
        text(Last),
        {ok, Last}
    catch
        error:_ -> {error, invalid_request}
    end;
cursor_key(_, _) ->
    {error, invalid_request}.

result(Ctx, Kind, Filter, Size, Rows, Key) ->
    Items = [decode_item(R) || R <- lists:sublist(Rows, Size)],
    More = length(Rows) > Size,
    case next_cursor(More, Ctx, Kind, Filter, Items, Key) of
        {ok, Cursor} ->
            {ok, #{
                <<"items">> => Items,
                <<"page_size">> => Size,
                <<"has_more">> => More,
                <<"next_cursor">> => Cursor
            }};
        {error, _} ->
            {error, internal_error}
    end.

next_cursor(false, _, _, _, _, _) ->
    {ok, null};
next_cursor(true, Ctx, Kind, Filter, Items, Key) ->
    Last = maps:get(id_field(Kind), lists:last(Items)),
    Payload = enterprise_cursor_v2:build_payload(
        family(Kind),
        maps:get(organization_id, Ctx),
        maps:get(application_id, Ctx),
        Filter,
        [Last],
        os:system_time(second)
    ),
    enterprise_cursor_v2:sign(Payload, Key).

decode_item(#{<<"department_external_ids">> := Json} = R) when is_binary(Json) ->
    R#{<<"department_external_ids">> := jsone:decode(Json)};
decode_item(R) ->
    R.

family(organizations) -> <<"source_organizations">>;
family(departments) -> <<"source_departments">>;
family(persons) -> <<"source_persons">>.

id_field(organizations) -> <<"external_organization_id">>;
id_field(departments) -> <<"external_department_id">>;
id_field(persons) -> <<"external_user_id">>.
