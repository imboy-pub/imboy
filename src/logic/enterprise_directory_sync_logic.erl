-module(enterprise_directory_sync_logic).
-moduledoc "Directory-only incremental sync. Source organization rows are provisioned by trusted configuration; clients cannot nominate an Owner or UID. sync/3 owns the transaction; sync_tx/4 requires its caller to rollback abort_tx and run elib_pg commit actions. Explicit higher source versions may restore removed personnel under R20; retired account bindings cannot be restored. Organization membership removal is recoverable.".
-export([sync/3, sync_tx/4, claim_and_project_tx/4, claim_login_tx/4, validate/1, version/2]).

%% Login proves an identity, not directory write authority. Keep unauthorized
%% memberships pending; never elevate the issuer's scopes to project them.
claim_login_tx(Conn, Ctx, ExternalId, UserId) ->
    AppId = maps:get(application_id, Ctx),
    Authority =
        case maps:get(authentication, Ctx, legacy) of
            hmac ->
                enterprise_oa_login_repo:authority_signed_tx(
                    Conn, AppId, maps:get(credential_id, Ctx)
                );
            _ ->
                enterprise_oa_login_repo:authority_tx(Conn, AppId, maps:get(credential_id, Ctx))
        end,
    _ = must(Authority),
    Person = must(enterprise_source_person_repo:claim_tx(Conn, AppId, ExternalId, UserId)),
    Sources = must(enterprise_directory_source_repo:login_projections_tx(Conn, AppId, ExternalId)),
    Pending = lists:foldl(
        fun(Source, Count) ->
            case maps:get(<<"source_current">>, Source) of
                true -> ok;
                false -> fail(source_unavailable)
            end,
            TxCtx = Ctx#{organization_id => maps:get(<<"organization_id">>, Source)},
            case enterprise_directory_source_repo:authorize_tx(Conn, TxCtx) of
                {ok, _} ->
                    project_claim(Conn, Ctx, ExternalId, Person, Source),
                    Count;
                {error, insufficient_scope} ->
                    Count + 1;
                {error, organization_boundary_violation} ->
                    Count + 1;
                {error, Reason} ->
                    fail(Reason)
            end
        end,
        0,
        Sources
    ),
    {ok, Person#{<<"projection_pending">> => Pending > 0}}.

claim_and_project_tx(Conn, Ctx, ExternalId, UserId) ->
    _ = must(enterprise_directory_source_repo:lock_source_tx(Conn, Ctx)),
    AppId = maps:get(application_id, Ctx),
    Person = must(enterprise_source_person_repo:claim_tx(Conn, AppId, ExternalId, UserId)),
    Sources = must(
        enterprise_directory_source_repo:pending_projections_tx(Conn, AppId, ExternalId)
    ),
    lists:foreach(
        fun(Source) ->
            project_claim(Conn, Ctx, ExternalId, Person, Source)
        end,
        Sources
    ),
    {ok, Person}.

project_claim(Conn, Ctx, ExternalId, Person, Source) ->
    OrgId = maps:get(<<"organization_id">>, Source),
    TxCtx = Ctx#{organization_id => OrgId},
    _ = must(enterprise_directory_source_repo:authorize_tx(Conn, TxCtx)),
    _ = organization(Conn, Source, #{}),
    Departments = maps:get(<<"department_external_ids">>, Source),
    Spec = #{
        <<"version">> => maps:get(<<"source_version">>, Source),
        <<"status">> => <<"active">>,
        <<"department_external_ids">> =>
            case Departments of
                Json when is_binary(Json) -> jsone:decode(Json);
                List -> List
            end
    },
    must(
        case version(Source, Spec) of
            replay -> ok;
            Other -> {error, {invalid_pending_projection, Other}}
        end
    ),
    save_projection(Conn, TxCtx, ExternalId, Person, Source, Spec),
    _ = must(
        enterprise_audit_event_repo:append_tx(Conn, OrgId, #{
            resource_type => <<"directory_sync">>,
            resource_id => OrgId,
            action => <<"directory.claim_projected">>,
            actor_role => <<"directory_source">>,
            detail => #{
                <<"application_id">> => maps:get(application_id, Ctx),
                <<"user_id">> => maps:get(<<"user_id">>, Person)
            }
        })
    ),
    _ = must(enterprise_directory_source_repo:authorize_tx(Conn, TxCtx)),
    ok.

sync(Ctx, Key, Body) ->
    case validate(Body) of
        ok -> elib_pg:with_tx(fun(Conn) -> sync_tx(Conn, Ctx, Key, Body) end);
        Error -> Error
    end.

sync_tx(Conn, Ctx, Key, Body) ->
    must(validate(Body)),
    case enterprise_internal_idempotency:valid_key(Key) of
        true -> ok;
        false -> fail(invalid_idempotency_key)
    end,
    _ = must(enterprise_directory_source_repo:lock_source_tx(Conn, Ctx)),
    AppId = maps:get(application_id, Ctx),
    OrgSpec = maps:get(<<"organization">>, Body),
    ExternalId = maps:get(<<"external_id">>, OrgSpec),
    Source = must(enterprise_directory_source_repo:organization_tx(Conn, AppId, ExternalId)),
    Org =
        case maps:get(<<"organization_id">>, Source) of
            null -> bootstrap_organization(Conn, Ctx, Source, OrgSpec);
            _ -> organization(Conn, Source, OrgSpec)
        end,
    OrgId = maps:get(<<"id">>, Org),
    TxCtx = Ctx#{organization_id => OrgId},
    _ = must(enterprise_directory_source_repo:authorize_tx(Conn, TxCtx)),
    Digest = must(
        enterprise_internal_idempotency:request_digest(
            <<"POST">>, <<"/api/internal/v1/directory/sync">>, Body
        )
    ),
    case enterprise_internal_idempotency:begin_tx(Conn, TxCtx, <<"directory_sync">>, Key, Digest) of
        {ok, replay, Snapshot} -> {ok, replay, Snapshot};
        {ok, pending} -> fail(idempotency_pending);
        {error, Reason} -> fail(Reason);
        {ok, inserted} -> apply_sync(Conn, TxCtx, Key, Body, Source, Org)
    end.

bootstrap_organization(Conn, Ctx, Source, Spec) ->
    Org = must(enterprise_directory_source_repo:bootstrap_tx(Conn, Ctx, Source, Spec)),
    OrgId = maps:get(<<"id">>, Org),
    _ = must(
        enterprise_audit_event_repo:append_tx(Conn, OrgId, #{
            resource_type => <<"directory_sync">>,
            resource_id => OrgId,
            action => <<"directory.bootstrapped">>,
            actor_role => <<"directory_source">>,
            detail => #{
                <<"application_id">> => maps:get(application_id, Ctx),
                <<"credential_id">> => maps:get(credential_id, Ctx),
                <<"confirmed_owner_user_id">> => maps:get(<<"owner_id">>, Org)
            }
        })
    ),
    Org.

organization(Conn, #{<<"organization_id">> := OrgId}, _) ->
    case must(organization_repo:find_for_update_tx(Conn, OrgId)) of
        #{<<"status">> := <<"active">>} = Org -> Org;
        _ -> fail(organization_not_active)
    end.

apply_sync(Conn, Ctx, Key, Body, Source, Org) ->
    AppId = maps:get(application_id, Ctx),
    OrgId = maps:get(organization_id, Ctx),
    Spec = maps:get(<<"organization">>, Body),
    case version(Source, Spec) of
        replay ->
            ok;
        advance ->
            _ = must(
                organization_repo:update_tx(Conn, OrgId, maps:get(<<"name">>, Spec), null, null)
            ),
            must(
                enterprise_directory_source_repo:save_organization_tx(
                    Conn, AppId, maps:get(<<"external_id">>, Spec), OrgId, Spec
                )
            );
        {error, Reason} ->
            fail(Reason)
    end,
    Departments = maps:get(<<"departments">>, Body),
    lists:foreach(
        fun(Dept) -> department(Conn, AppId, OrgId, Dept) end,
        order_departments([
            Dept
         || Dept <- Departments, maps:get(<<"status">>, Dept) =:= <<"active">>
        ])
    ),
    lists:foreach(fun(Person) -> person(Conn, Ctx, Person) end, maps:get(<<"persons">>, Body)),
    lists:foreach(
        fun(Dept) -> department(Conn, AppId, OrgId, Dept) end,
        lists:reverse(
            order_departments([
                Dept
             || Dept <- Departments,
                maps:get(<<"status">>, Dept) =:= <<"removed">>
            ])
        )
    ),
    Response = jsone:encode(#{<<"organization_id">> => OrgId, <<"synced">> => true}),
    _ = must(
        enterprise_audit_event_repo:append_tx(Conn, OrgId, #{
            resource_type => <<"directory_sync">>,
            resource_id => OrgId,
            action => <<"directory.synced">>,
            actor_role => <<"directory_source">>,
            detail => #{
                <<"application_id">> => AppId,
                <<"credential_id">> => maps:get(credential_id, Ctx),
                <<"request_digest">> => digest(Body)
            }
        })
    ),
    _ = must(enterprise_directory_source_repo:authorize_tx(Conn, Ctx)),
    must(
        enterprise_internal_idempotency:complete_tx(
            Conn, Ctx, <<"directory_sync">>, Key, maps:get(<<"id">>, Org), 200, Response
        )
    ),
    {ok, applied, #{resource_id => OrgId, response_code => 200, response_body => Response}}.

order_departments([]) ->
    [];
order_departments(Departments) ->
    ExternalIds = [maps:get(<<"external_id">>, Dept) || Dept <- Departments],
    {Ready, Pending} = lists:partition(
        fun(Dept) ->
            not lists:member(maps:get(<<"parent_external_id">>, Dept), ExternalIds)
        end,
        Departments
    ),
    case Ready of
        [] -> fail(cycle);
        _ -> Ready ++ order_departments(Pending)
    end.

department(Conn, AppId, OrgId, Spec) ->
    ExternalId = maps:get(<<"external_id">>, Spec),
    Existing = enterprise_directory_source_repo:department_tx(Conn, AppId, OrgId, ExternalId),
    case Existing of
        {error, not_found} ->
            create_department(Conn, AppId, OrgId, Spec);
        {ok, Source} ->
            case version(Source, Spec) of
                replay -> ok;
                advance -> update_department(Conn, AppId, OrgId, Source, Spec);
                {error, Reason} -> fail(Reason)
            end;
        {error, Reason} ->
            fail(Reason)
    end.

create_department(Conn, AppId, OrgId, #{<<"status">> := <<"active">>} = Spec) ->
    ParentId = parent(Conn, AppId, OrgId, maps:get(<<"parent_external_id">>, Spec)),
    Dept = must(
        organization_department_pg:insert_department_tx(
            Conn,
            OrgId,
            ParentId,
            maps:get(<<"name">>, Spec),
            null,
            fun() -> elib_tsid:generate(organization_department) end
        )
    ),
    must(
        enterprise_directory_source_repo:save_department_tx(
            Conn,
            AppId,
            OrgId,
            maps:get(<<"external_id">>, Spec),
            maps:get(id, Dept),
            Spec
        )
    );
create_department(_, _, _, _) ->
    fail(department_not_found).

update_department(Conn, AppId, OrgId, Source, Spec) ->
    DeptId = maps:get(<<"department_id">>, Source),
    Dept = must(organization_department_pg:fetch_department_tx(Conn, OrgId, DeptId)),
    apply_department(Conn, AppId, OrgId, Dept, Spec),
    must(
        enterprise_directory_source_repo:save_department_tx(
            Conn,
            AppId,
            OrgId,
            maps:get(<<"external_id">>, Spec),
            DeptId,
            Spec
        )
    ).

apply_department(Conn, AppId, OrgId, Dept, #{<<"status">> := <<"active">>} = Spec) ->
    DeptId = maps:get(id, Dept),
    ParentId = parent(Conn, AppId, OrgId, maps:get(<<"parent_external_id">>, Spec)),
    case maps:get(status, Dept) of
        active ->
            update_active_department(Conn, OrgId, Dept, ParentId, Spec);
        archived ->
            must(
                enterprise_directory_source_repo:restore_department_tx(
                    Conn, OrgId, DeptId, ParentId, maps:get(<<"name">>, Spec)
                )
            )
    end;
apply_department(Conn, _, OrgId, Dept, _) ->
    must(enterprise_directory_source_repo:archive_department_tx(Conn, OrgId, maps:get(id, Dept))).

update_active_department(Conn, OrgId, Dept, ParentId, Spec) ->
    DeptId = maps:get(id, Dept),
    _ = must(
        organization_department_pg:move_in_tx(
            Conn,
            OrgId,
            DeptId,
            ParentId,
            null,
            maps:get(version, Dept)
        )
    ),
    Updated = must(organization_department_pg:fetch_department_tx(Conn, OrgId, DeptId)),
    must(
        organization_department_pg:update_name_tx(
            Conn,
            OrgId,
            DeptId,
            maps:get(<<"name">>, Spec),
            null,
            maps:get(version, Updated)
        )
    ).

parent(_, _, _, null) ->
    null;
parent(Conn, AppId, OrgId, ExternalId) ->
    Source = must(enterprise_directory_source_repo:department_tx(Conn, AppId, OrgId, ExternalId)),
    DeptId = maps:get(<<"department_id">>, Source),
    case must(organization_department_pg:fetch_department_tx(Conn, OrgId, DeptId)) of
        #{status := active} -> DeptId;
        _ -> fail(department_archived)
    end.

person(Conn, Ctx, Spec) ->
    AppId = maps:get(application_id, Ctx),
    ExternalId = maps:get(<<"external_user_id">>, Spec),
    Membership = maps:get(<<"membership">>, Spec),
    SourceStatus = maps:get(<<"source_status">>, Spec),
    case SourceStatus of
        <<"removed">> ->
            case maps:get(<<"status">>, Membership) of
                <<"removed">> -> ok;
                _ -> fail(invalid_source_membership_status)
            end;
        _ ->
            ok
    end,
    Person =
        case {SourceStatus, enterprise_source_person_repo:find_tx(Conn, AppId, ExternalId)} of
            {<<"removed">>, {ok, Existing}} ->
                Existing;
            {_, {error, Reason}} when Reason =/= not_found -> fail(Reason);
            _ ->
                must(
                    enterprise_source_person_repo:sync_tx(
                        Conn,
                        AppId,
                        ExternalId,
                        maps:get(<<"display_name">>, Spec),
                        SourceStatus,
                        maps:get(<<"source_version">>, Spec)
                    )
                )
        end,
    membership(Conn, Ctx, ExternalId, Person, Membership),
    case SourceStatus of
        <<"removed">> ->
            must(enterprise_directory_source_repo:retire_person_tx(Conn, AppId, ExternalId)),
            _ = must(
                enterprise_source_person_repo:sync_tx(
                    Conn,
                    AppId,
                    ExternalId,
                    maps:get(<<"display_name">>, Spec),
                    SourceStatus,
                    maps:get(<<"source_version">>, Spec)
                )
            );
        _ ->
            ok
    end.

membership(Conn, Ctx, ExternalId, Person, Spec) ->
    AppId = maps:get(application_id, Ctx),
    OrgId = maps:get(organization_id, Ctx),
    Existing = enterprise_directory_source_repo:membership_tx(Conn, AppId, OrgId, ExternalId),
    Source =
        case Existing of
            {error, not_found} ->
                #{
                    <<"source_version">> => 0,
                    <<"managed">> => false,
                    <<"projected_user_id">> => null
                };
            _ ->
                must(Existing)
        end,
    case version(Source, Spec) of
        replay ->
            case
                {maps:get(<<"projected_user_id">>, Source, null), maps:get(<<"user_id">>, Person)}
            of
                {null, UserId} when is_integer(UserId) ->
                    save_projection(Conn, Ctx, ExternalId, Person, Source, Spec);
                _ ->
                    ok
            end;
        {error, Reason} ->
            fail(Reason);
        advance ->
            save_projection(Conn, Ctx, ExternalId, Person, Source, Spec)
    end.

save_projection(Conn, Ctx, ExternalId, Person, Source, Spec) ->
    {Managed, RemovedAt} = project_membership(Conn, Ctx, Person, Source, Spec),
    must(
        enterprise_directory_source_repo:save_membership_tx(
            Conn,
            maps:get(application_id, Ctx),
            maps:get(organization_id, Ctx),
            ExternalId,
            Managed,
            maps:get(<<"user_id">>, Person),
            Spec
        )
    ),
    must(
        enterprise_directory_source_repo:save_offboarding_tx(
            Conn,
            maps:get(application_id, Ctx),
            maps:get(organization_id, Ctx),
            ExternalId,
            RemovedAt
        )
    ).

project_membership(_, _, #{<<"user_id">> := null}, Source, _) ->
    {maps:get(<<"managed">>, Source), null};
project_membership(Conn, Ctx, #{<<"user_id">> := UserId}, Source, Spec) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    Status = maps:get(<<"status">>, Spec),
    Managed = maps:get(<<"managed">>, Source),
    case
        must(
            enterprise_directory_source_repo:project_member_tx(
                Conn, OrgId, UserId, Status, Managed, Source
            )
        )
    of
        remove ->
            must(
                enterprise_directory_source_repo:department_members_tx(
                    Conn, AppId, OrgId, UserId, []
                )
            ),
            _ = must(organization_member_logic:directory_remove_tx(Conn, Ctx, OrgId, UserId)),
            {Managed, actual_removed};
        NewManaged ->
            DeptIds = [
                parent(Conn, AppId, OrgId, ExternalDept)
             || ExternalDept <- maps:get(<<"department_external_ids">>, Spec)
            ],
            must(
                enterprise_directory_source_repo:department_members_tx(
                    Conn, AppId, OrgId, UserId, DeptIds
                )
            ),
            RemovedAt =
                case {Status, maps:get(<<"status">>, Source, undefined)} of
                    {<<"removed">>, <<"removed">>} ->
                        maps:get(<<"offboarding_member_updated_at">>, Source, null);
                    _ ->
                        null
                end,
            {NewManaged, RemovedAt}
    end.

version(#{<<"source_version">> := Current} = Source, Spec) ->
    Incoming = maps:get(<<"version">>, Spec),
    if
        Incoming > Current ->
            advance;
        Incoming < Current ->
            {error, stale_source_version};
        true ->
            case maps:get(<<"payload_digest">>, Source, null) =:= digest(Spec) of
                true -> replay;
                false -> {error, source_version_conflict}
            end
    end.

validate(Body) ->
    try
        keys(Body, [<<"organization">>, <<"departments">>, <<"persons">>]),
        Org = maps:get(<<"organization">>, Body),
        keys(Org, [<<"external_id">>, <<"name">>, <<"version">>]),
        text(maps:get(<<"external_id">>, Org), 256),
        text(maps:get(<<"name">>, Org), 200),
        positive(maps:get(<<"version">>, Org)),
        Departments = maps:get(<<"departments">>, Body),
        Persons = maps:get(<<"persons">>, Body),
        bounded(Departments),
        bounded(Persons),
        lists:foreach(fun valid_department/1, Departments),
        lists:foreach(fun valid_person/1, Persons),
        unique([maps:get(<<"external_id">>, Dept) || Dept <- Departments]),
        unique([maps:get(<<"external_user_id">>, Person) || Person <- Persons]),
        ok
    catch
        _:_ -> {error, invalid_request}
    end.

valid_department(Dept) ->
    keys(Dept, [
        <<"external_id">>, <<"parent_external_id">>, <<"name">>, <<"version">>, <<"status">>
    ]),
    text(maps:get(<<"external_id">>, Dept), 256),
    text(maps:get(<<"name">>, Dept), 128),
    positive(maps:get(<<"version">>, Dept)),
    status(maps:get(<<"status">>, Dept)),
    case maps:get(<<"parent_external_id">>, Dept) of
        null -> ok;
        Parent -> text(Parent, 256)
    end.

valid_person(Person) ->
    keys(Person, [
        <<"external_user_id">>,
        <<"display_name">>,
        <<"source_status">>,
        <<"source_version">>,
        <<"membership">>
    ]),
    text(maps:get(<<"external_user_id">>, Person), 256),
    text(maps:get(<<"display_name">>, Person), 128),
    positive(maps:get(<<"source_version">>, Person)),
    status(maps:get(<<"source_status">>, Person)),
    Membership = maps:get(<<"membership">>, Person),
    keys(Membership, [<<"version">>, <<"status">>, <<"department_external_ids">>]),
    positive(maps:get(<<"version">>, Membership)),
    status(maps:get(<<"status">>, Membership)),
    Depts = maps:get(<<"department_external_ids">>, Membership),
    bounded(Depts),
    lists:foreach(fun(Dept) -> text(Dept, 256) end, Depts),
    unique(Depts),
    true = maps:get(<<"status">>, Membership) =:= <<"active">> orelse Depts =:= [].

keys(Map, Keys) -> true = is_map(Map) andalso lists:sort(maps:keys(Map)) =:= lists:sort(Keys).
text(Value, Max) ->
    true = is_binary(Value) andalso byte_size(Value) > 0 andalso byte_size(Value) =< Max,
    true = is_list(unicode:characters_to_list(Value)).
positive(Value) -> true = is_integer(Value) andalso Value > 0 andalso Value =< 9223372036854775807.
status(Value) -> true = Value =:= <<"active">> orelse Value =:= <<"removed">>.
bounded(List) -> true = is_list(List) andalso length(List) =< 100.
unique(List) -> true = length(List) =:= length(lists:usort(List)).
digest(Spec) ->
    must(enterprise_internal_idempotency:request_digest(<<"POST">>, <<"directory">>, Spec)).
must(ok) -> ok;
must({ok, Value}) -> Value;
must({error, Reason}) -> fail(Reason).
fail(Reason) -> throw({abort_tx, Reason}).
