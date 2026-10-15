-module(enterprise_oa_login_logic).
-moduledoc "OA inbound login: verified source tickets, PKCE and explicit account association.".
-export([issue/2, exchange/1, link/4, register/1, validate_issue/1, pkce/1]).

issue(Ctx, P) ->
    case validate_issue(P) of
        ok ->
            transaction(fun(C) ->
                App = maps:get(application_id, Ctx),
                Cred = maps:get(credential_id, Ctx),
                _ = must(enterprise_oa_login_repo:authority_tx(C, App, Cred)),
                Ticket = random_token(),
                _ = must(
                    enterprise_oa_login_repo:issue_tx(
                        C,
                        digest(Ticket),
                        App,
                        Cred,
                        maps:get(<<"external_user_id">>, P),
                        {maps:get(<<"intent">>, P), maps:get(<<"code_challenge">>, P)}
                    )
                ),
                audit(C, App, <<"oa.login_ticket_issued">>, #{}),
                _ = must(enterprise_oa_login_repo:authority_tx(C, App, Cred)),
                {ok, #{<<"ticket">> => Ticket, <<"expires_in">> => 60}}
            end);
        Error ->
            Error
    end.

exchange(P) ->
    transaction(fun(C) ->
        {Row, Digest, Did} = verified_ticket(C, P, <<"issued">>, <<"ticket">>),
        App = maps:get(<<"application_id">>, Row),
        Person = must(
            enterprise_oa_login_repo:person_tx(C, App, maps:get(<<"external_user_id">>, Row))
        ),
        case {maps:get(<<"intent">>, Row), Person} of
            {<<"login">>, [
                #{
                    <<"user_id">> := Uid,
                    <<"status">> := <<"active">>,
                    <<"binding_retired">> := false
                }
            ]} when is_integer(Uid) ->
                Projection = claim(C, Row, Uid),
                complete(C, Row, Digest, Uid, Did, P#{projection_pending => Projection});
            {_, []} ->
                pending(C, Row, Digest, Did);
            {_, [#{<<"status">> := <<"active">>, <<"binding_retired">> := false}]} ->
                pending(C, Row, Digest, Did);
            _ ->
                fail(identity_conflict)
        end
    end).

%% UID/DID/epoch originate only from the authenticated Human token, never JSON.
link(Uid, AuthDid, {Epoch, Expiry}, P) ->
    transaction(fun(C) ->
        {Row, Digest, Did} = verified_ticket(C, P, <<"pending">>, <<"pending_token">>),
        require(
            Did =:= AuthDid andalso is_integer(Epoch) andalso
                is_integer(Expiry) andalso Expiry > erlang:system_time(second)
        ),
        _ = must(enterprise_oa_login_repo:human_tx(C, Uid)),
        #{<<"epoch">> := Current} = must(enterprise_oa_login_repo:epoch_tx(C, Uid)),
        require(Epoch =:= Current),
        {ok, [_]} = require_device(C, Uid, Did),
        Projection = claim(C, Row, Uid),
        require(Expiry > erlang:system_time(second)),
        complete(C, Row, Digest, Uid, Did, P#{projection_pending => Projection})
    end).

register(P) ->
    case maps:get(<<"confirm_create">>, P, false) of
        true ->
            %% Validate before spending allocator resources. Allocation borrows a
            %% separate connection; the create transaction repeats every check.
            case
                transaction(fun(C) ->
                    _ = registration_ticket_tx(C, P),
                    {ok, ready}
                end)
            of
                {ok, ready} ->
                    case passport_logic:quota_guard() of
                        ok -> register_account(P, account_ds:allocate());
                        _ -> {error, account_quota_exceeded}
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, explicit_confirmation_required}
    end.

register_account(_P, {error, Reason}) ->
    {error, Reason};
register_account(P, Account) when is_integer(Account) ->
    transaction(fun(C) ->
        {Row, Digest, Did} = registration_ticket_tx(C, P),
        Uid = must(
            user_repo:create_tx(C, #{
                account => Account,
                nickname => <<>>,
                source => <<"oa_login">>,
                reg_ip => <<>>,
                reg_cosv => <<"oa_login">>,
                password => elib_password:generate(random_token())
            })
        ),
        Projection = claim(C, Row, Uid),
        complete(C, Row, Digest, Uid, Did, P#{projection_pending => Projection})
    end).

registration_ticket_tx(C, P) ->
    {Row, Digest, Did} = verified_ticket(C, P, <<"pending">>, <<"pending_token">>),
    require(maps:get(<<"intent">>, Row) =:= <<"login">>),
    %% Serialize absent identities too; both claim paths take the same App lock.
    _ = lock_source(C, Row),
    Existing = must(
        enterprise_oa_login_repo:person_tx(
            C,
            maps:get(<<"application_id">>, Row),
            maps:get(<<"external_user_id">>, Row)
        )
    ),
    case Existing of
        [] ->
            ok;
        [
            #{
                <<"user_id">> := null,
                <<"binding_retired">> := false,
                <<"status">> := <<"active">>
            }
        ] ->
            ok;
        _ ->
            fail(identity_conflict)
    end,
    {Row, Digest, Did}.

claim(C, Row, Uid) ->
    Person = must(
        enterprise_directory_sync_logic:claim_login_tx(
            C,
            context(Row),
            maps:get(<<"external_user_id">>, Row),
            Uid
        )
    ),
    audit(C, maps:get(<<"application_id">>, Row), <<"oa.identity_linked">>, #{<<"user_id">> => Uid}),
    maps:get(<<"projection_pending">>, Person).

lock_source(C, Row) ->
    must(
        enterprise_oa_login_repo:authority_tx(
            C,
            maps:get(<<"application_id">>, Row),
            maps:get(<<"credential_id">>, Row)
        )
    ).
context(Row) ->
    #{
        application_id => maps:get(<<"application_id">>, Row),
        credential_id => maps:get(<<"credential_id">>, Row)
    }.

pending(C, Row, Digest, Did) ->
    Token = random_token(),
    _ = must(enterprise_oa_login_repo:pending_tx(C, Digest, digest(Token), Did)),
    audit(C, maps:get(<<"application_id">>, Row), <<"oa.login_pending">>, #{}),
    _ = lock_source(C, Row),
    {ok, #{
        <<"pending_token">> => Token, <<"requires_account_choice">> => true, <<"expires_in">> => 300
    }}.

verified_ticket(C, P, Stage, Field) ->
    Token = maps:get(Field, P, <<>>),
    Verifier = maps:get(<<"code_verifier">>, P, <<>>),
    Did = maps:get(<<"did">>, P, <<>>),
    require(
        valid_token(Token, 43, 43) andalso valid_token(Verifier, 43, 128) andalso
            is_binary(Did) andalso byte_size(Did) > 0 andalso byte_size(Did) =< 40
    ),
    Digest = digest(Token),
    Row = must(enterprise_oa_login_repo:lock_tx(C, Digest, Stage)),
    require(pkce(Verifier) =:= maps:get(<<"challenge">>, Row)),
    require(Stage =:= <<"issued">> orelse Did =:= maps:get(<<"device_id">>, Row)),
    Authority = must(
        enterprise_oa_login_repo:authority_tx(
            C,
            maps:get(<<"application_id">>, Row),
            maps:get(<<"credential_id">>, Row)
        )
    ),
    require(
        maps:get(<<"application_key">>, P, undefined) =:=
            maps:get(<<"application_key">>, Authority)
    ),
    {Row, Digest, Did}.

complete(C, Row, Digest, Uid, Did, P) ->
    User = must(enterprise_oa_login_repo:human_tx(C, Uid)),
    _ = must(enterprise_oa_login_repo:device_tx(C, Uid, Did, P)),
    #{<<"epoch">> := Epoch} = must(enterprise_oa_login_repo:epoch_tx(C, Uid)),
    audit(C, maps:get(<<"application_id">>, Row), <<"oa.login_completed">>, #{<<"user_id">> => Uid}),
    _ = must(
        enterprise_oa_login_repo:authority_tx(
            C,
            maps:get(<<"application_id">>, Row),
            maps:get(<<"credential_id">>, Row)
        )
    ),
    _ = must(enterprise_oa_login_repo:consume_tx(C, Digest)),
    %% The payload is returned only after the encompassing transaction commits.
    {ok,
        maps:merge(
            User#{
                <<"uid">> => Uid,
                <<"user_id">> => Uid,
                <<"projection_pending">> => maps:get(projection_pending, P, false)
            },
            token_ds:issue_device_session(Uid, Did, Epoch)
        )}.

require_device(C, Uid, Did) ->
    Result = elib_pg:query(
        C,
        <<
            "SELECT id FROM user_device WHERE user_id=$1 AND device_id=$2 "
            "AND status=1 FOR SHARE"
        >>,
        [Uid, Did]
    ),
    case Result of
        {ok, [_]} -> Result;
        _ -> fail(invalid_session)
    end.

audit(C, App, Action, Detail) ->
    _ = must(
        enterprise_audit_event_repo:append_tx(C, null, #{
            resource_type => <<"enterprise_application">>,
            resource_id => App,
            action => Action,
            actor_role => <<"application">>,
            detail => Detail#{<<"application_id">> => App}
        })
    ),
    ok.

validate_issue(P) when is_map(P) ->
    Ext = maps:get(<<"external_user_id">>, P, <<>>),
    Intent = maps:get(<<"intent">>, P, <<>>),
    Challenge = maps:get(<<"code_challenge">>, P, <<>>),
    case
        is_binary(Ext) andalso byte_size(Ext) > 0 andalso byte_size(Ext) =< 256 andalso
            lists:member(Intent, [<<"login">>, <<"link">>]) andalso
            maps:get(<<"code_challenge_method">>, P, <<>>) =:= <<"S256">> andalso
            valid_token(Challenge, 43, 43)
    of
        true -> ok;
        false -> {error, invalid_request}
    end;
validate_issue(_) ->
    {error, invalid_request}.

valid_token(V, Min, Max) when is_binary(V), byte_size(V) >= Min, byte_size(V) =< Max ->
    re:run(V, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) =:= match;
valid_token(_, _, _) ->
    false.
pkce(V) -> b64(crypto:hash(sha256, V)).
digest(V) -> crypto:hash(sha256, V).
random_token() -> b64(crypto:strong_rand_bytes(32)).
b64(B) ->
    binary:replace(
        binary:replace(
            binary:replace(base64:encode(B), <<"+">>, <<"-">>, [global]),
            <<"/">>,
            <<"_">>,
            [global]
        ),
        <<"=">>,
        <<>>,
        [global]
    ).
require(true) -> ok;
require(false) -> fail(invalid_request).
must({ok, V}) -> V;
must({error, R}) -> fail(R).
fail(R) -> throw({abort_tx, R}).
transaction(F) ->
    case elib_pg:with_tx(F) of
        {rollback, R} -> {error, R};
        Result -> Result
    end.
