-module(enterprise_directory_member_logic).
-moduledoc "OA offboarding preflight and provenance-bound suspension of source-managed ordinary members.".
-export([read_tx/3, write_tx/7, validate/2, transition/3]).

read_tx(C, Ctx, Query) ->
    must(validate(read, Query)),
    {Bound, Row} = target(C, Ctx, Query, read),
    {ok, view(C, Bound, Row)}.

write_tx(C, Ctx, update, Key, Digest, _Id, Body) ->
    ok = enterprise_internal_idempotency:serialize_tx(C, Ctx, Key),
    must(validate(write, Body)),
    {Bound, Row} = target(C, Ctx, Body, write),
    case managed(Row) of
        true -> ok;
        false -> fail(resource_conflict)
    end,
    Type = <<"directory_member_status">>,
    case enterprise_internal_idempotency:begin_tx(C, Bound, Type, Key, Digest) of
        {ok, replay, #{response_code := Status, response_body := Bytes}} ->
            {ok, Status, Bytes, [enterprise_internal_idempotency:replay_header()]};
        {ok, inserted} ->
            change(C, Bound, Row, Key, Body);
        {ok, pending} ->
            fail(idempotency_conflict);
        {error, digest_conflict} ->
            fail(idempotency_conflict);
        _ ->
            fail(internal_error)
    end.

target(C, Ctx, Input, Mode) ->
    App = maps:get(application_id, Ctx),
    ExtOrg = maps:get(<<"external_organization_id">>, Input),
    ExtUser = maps:get(<<"external_user_id">>, Input),
    case Mode of
        write -> _ = must(enterprise_directory_source_repo:lock_source_tx(C, Ctx));
        read -> ok
    end,
    #{<<"organization_id">> := Org} = must(
        enterprise_directory_source_read_repo:organization_tx(C, Ctx, ExtOrg)
    ),
    Bound = Ctx#{organization_id => Org},
    case Mode of
        write -> lock_target(C, Bound, ExtOrg, ExtUser);
        read -> ok
    end,
    Row = must(enterprise_directory_member_repo:target_tx(C, App, Org, ExtUser)),
    {Bound, Row}.

lock_target(C, Ctx, ExtOrg, External) ->
    App = maps:get(application_id, Ctx),
    Org = maps:get(organization_id, Ctx),
    _ = must(enterprise_directory_source_repo:organization_tx(C, App, ExtOrg)),
    _ = must(organization_repo:find_for_update_tx(C, Org)),
    Person = must(enterprise_source_person_repo:find_tx(C, App, External)),
    _ = must(enterprise_directory_source_repo:membership_tx(C, App, Org, External)),
    case maps:get(<<"user_id">>, Person) of
        User when is_integer(User) ->
            _ = must(organization_member_repo:find_for_update_tx(C, Org, User, <<"role,status">>)),
            ok;
        _ ->
            fail(resource_conflict)
    end.

change(C, Ctx, Row, Key, Body) ->
    To = maps:get(<<"status">>, Body),
    Version = maps:get(<<"expected_member_version">>, Body),
    must(transition(Row, To, Version)),
    Org = maps:get(organization_id, Ctx),
    App = maps:get(application_id, Ctx),
    User = maps:get(<<"user_id">>, Row),
    External = maps:get(<<"external_user_id">>, Row),
    From = maps:get(<<"member_status">>, Row),
    New = must(enterprise_directory_member_repo:status_tx(C, Org, User, From, To, Version)),
    Marker =
        case To of
            <<"suspended">> -> New;
            <<"active">> -> null
        end,
    must(enterprise_directory_member_repo:remember_tx(C, App, Org, External, Marker)),
    must(audit(C, Ctx, User, From, To)),
    %% Recheck live authority even on long transactions. A replay also enters
    %% through target/6, so revoked source/credential/grant never replays data.
    _ = must(
        enterprise_directory_source_read_repo:organization_tx(
            C,
            Ctx,
            maps:get(<<"external_organization_id">>, Body)
        )
    ),
    complete(C, Ctx, Key, User, External, To, New).

complete(C, Ctx, Key, User, External, To, New) ->
    Bytes = jsone:encode(#{
        <<"external_user_id">> => External,
        <<"member_status">> => To,
        <<"member_version">> => New
    }),
    must(
        enterprise_internal_idempotency:complete_tx(
            C,
            Ctx,
            <<"directory_member_status">>,
            Key,
            User,
            200,
            Bytes
        )
    ),
    {ok, 200, Bytes, []}.

transition(Row, To, Version) ->
    case managed(Row) of
        false ->
            {error, resource_conflict};
        true ->
            Current = maps:get(<<"member_version">>, Row, null),
            Marker = maps:get(<<"suspension_member_version">>, Row, null),
            case {maps:get(<<"member_status">>, Row, null), To, Current =:= Version} of
                {_, _, false} -> {error, version_conflict};
                {<<"active">>, <<"suspended">>, true} -> ok;
                {<<"suspended">>, <<"active">>, true} when Marker =:= Current -> ok;
                _ -> {error, resource_conflict}
            end
    end.

managed(R) ->
    maps:get(<<"managed">>, R, false) =:= true andalso
        maps:get(<<"membership_status">>, R, null) =:= <<"active">> andalso
        maps:get(<<"source_status">>, R, null) =:= <<"active">> andalso
        maps:get(<<"binding_retired">>, R, true) =:= false andalso
        maps:get(<<"valid_user">>, R, false) =:= true andalso
        maps:get(<<"role">>, R, null) =:= <<"member">> andalso
        is_integer(maps:get(<<"user_id">>, R, null)) andalso
        maps:get(<<"projected_user_id">>, R, null) =:= maps:get(<<"user_id">>, R, null).

view(C, Ctx, Row) ->
    Version = maps:get(<<"member_version">>, Row),
    Counts =
        case maps:get(<<"user_id">>, Row) of
            U when is_integer(U) ->
                must(
                    enterprise_directory_member_repo:blockers_tx(
                        C, maps:get(organization_id, Ctx), U
                    )
                );
            _ ->
                maps:from_list([
                    {K, 0}
                 || K <- [
                        <<"owned_workspaces">>,
                        <<"owned_projects">>,
                        <<"unfinished_tasks">>,
                        <<"owned_groups">>,
                        <<"owned_channels">>,
                        <<"active_assignments">>
                    ]
                ])
        end,
    Reasons0 = [K || {K, N} <- maps:to_list(Counts), N > 0],
    Reasons1 =
        case managed(Row) of
            true -> Reasons0;
            false -> [<<"not_managed_member">> | Reasons0]
        end,
    Reasons =
        case {maps:get(<<"member_status">>, Row), transition(Row, <<"active">>, Version)} of
            {<<"suspended">>, {error, _}} -> [<<"local_suspension_conflict">> | Reasons1];
            _ -> Reasons1
        end,
    #{
        <<"external_user_id">> => maps:get(<<"external_user_id">>, Row),
        <<"member_status">> => maps:get(<<"member_status">>, Row),
        <<"member_version">> => Version,
        <<"blocker_counts">> => Counts,
        <<"blockers">> => lists:sort(Reasons),
        <<"can_suspend">> => transition(Row, <<"suspended">>, Version) =:= ok,
        <<"can_restore">> => transition(Row, <<"active">>, Version) =:= ok,
        <<"can_remove">> => Reasons =:= [] andalso
            lists:member(maps:get(<<"member_status">>, Row), [<<"active">>, <<"suspended">>])
    }.

audit(C, Ctx, User, From, To) ->
    enterprise_audit_event_repo:append_tx(C, maps:get(organization_id, Ctx), #{
        resource_type => <<"organization_member">>,
        resource_id => User,
        action => <<"directory.member_status_changed">>,
        actor_role => <<"enterprise_application">>,
        detail => #{
            <<"origin_application_id">> => maps:get(application_id, Ctx),
            <<"correlation_id">> => maps:get(correlation_id, Ctx),
            <<"from_status">> => From,
            <<"to_status">> => To
        }
    }).

validate(Mode, Input) when is_map(Input) ->
    try
        Base = [<<"external_organization_id">>, <<"external_user_id">>],
        Keys =
            case Mode of
                read -> Base;
                write -> Base ++ [<<"status">>, <<"expected_member_version">>]
            end,
        true = lists:sort(maps:keys(Input)) =:= lists:sort(Keys),
        [
            begin
                V = maps:get(K, Input),
                true = is_binary(V) andalso byte_size(V) > 0 andalso byte_size(V) =< 256,
                true = is_list(unicode:characters_to_list(V))
            end
         || K <- Base
        ],
        case Mode of
            read ->
                ok;
            write ->
                true = lists:member(maps:get(<<"status">>, Input), [<<"active">>, <<"suspended">>]),
                N = maps:get(<<"expected_member_version">>, Input),
                true = is_integer(N) andalso N > 0 andalso N =< 9223372036854775807
        end,
        ok
    catch
        error:_ -> {error, invalid_request}
    end;
validate(_, _) ->
    {error, invalid_request}.

must(ok) -> ok;
must({ok, Value}) -> Value;
must({error, not_found}) -> fail(resource_not_found);
must({error, Code}) when is_atom(Code) -> fail(Code);
must(_) -> fail(internal_error).
fail(Code) -> throw({rollback, {internal_error_code, atom_to_binary(Code, utf8)}}).
