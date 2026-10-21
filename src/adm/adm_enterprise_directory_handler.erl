-module(adm_enterprise_directory_handler).
-export([init/2]).
-include("error_code.hrl").

init(Req, State) ->
    Response =
        case cowboy_req:method(Req) of
            <<"POST">> -> authorize(Req, State);
            _ -> cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req)
        end,
    {ok, Response, State}.

authorize(Req, State) ->
    case adm_acl:ensure_permission(State, <<"enterprise_business:write">>, Req) of
        {error, Response} ->
            Response;
        ok ->
            case maps:get(adm_user_id, State, undefined) of
                AdminId when is_integer(AdminId), AdminId > 0 ->
                    dispatch(Req, maps:get(action, State), #{adm_user_id => AdminId});
                _ ->
                    elib_response:error(Req, <<"unauthorized">>, ?ERR_UNAUTHORIZED)
            end
    end.

dispatch(Req, create_connection, Actor) ->
    Body = elib_param:post(Req),
    Input = #{
        application_key => maps:get(<<"application_key">>, Body, <<>>),
        name => maps:get(<<"name">>, Body, <<>>),
        redirect_uris => maps:get(<<"redirect_uris">>, Body, [])
    },
    case enterprise_directory_source_config_logic:create_connection(Input, Actor) of
        {ok, Row} ->
            elib_response:success(Req, #{
                <<"application_id">> =>
                    integer_to_binary(maps:get(<<"id">>, Row))
            });
        Error ->
            reply(Req, Error)
    end;
dispatch(Req, issue_credential, Actor) ->
    case {positive_id(cowboy_req:binding(application_id, Req)), elib_param:post(Req)} of
        {AppId, Body} when is_integer(AppId), is_map(Body) ->
            case
                enterprise_admin_governance_logic:issue_credential(
                    null, AppId, maps:get(<<"expires_at">>, Body, undefined), Actor
                )
            of
                {ok, Credential} -> elib_response:success(Req, Credential);
                Error -> reply(Req, Error)
            end;
        _ ->
            elib_response:error(Req, <<"invalid_directory_connection">>, ?ERR_BAD_REQUEST)
    end;
dispatch(Req, configure_source, Actor) ->
    Body = elib_param:post(Req),
    case {positive_id(cowboy_req:binding(application_id, Req)), Body} of
        {AppId, #{<<"external_organization_id">> := ExternalId}} when is_integer(AppId) ->
            Spec = maps:from_list([
                {Key, maps:get(atom_to_binary(Key), Body, undefined)}
             || Key <- [
                    confirmed_owner_user_id, enabled, authorization_expires_at, expected_version
                ]
            ]),
            reply(
                Req,
                enterprise_directory_source_config_logic:configure_source(
                    Actor, AppId, ExternalId, Spec
                )
            );
        _ ->
            elib_response:error(Req, <<"invalid_source_configuration">>, ?ERR_BAD_REQUEST)
    end.

positive_id(Value) when is_integer(Value), Value > 0 -> Value;
positive_id(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Id when Id > 0 -> Id;
        _ -> invalid
    catch
        error:badarg -> invalid
    end;
positive_id(_) ->
    invalid.

reply(Req, {ok, Row}) ->
    elib_response:success(
        Req,
        maps:with(
            [
                <<"external_organization_id">>,
                <<"enabled">>,
                <<"configuration_version">>,
                <<"authorization_expires_at">>
            ],
            Row
        )
    );
reply(Req, {error, Reason}) when
    Reason =:= configuration_conflict;
    Reason =:= initial_owner_immutable;
    Reason =:= bootstrap_grant_conflict
->
    elib_response:error(Req, atom_to_binary(Reason), ?ERR_CONFLICT);
reply(Req, {error, Reason}) when Reason =:= invalid_admin ->
    elib_response:error(Req, <<"forbidden">>, ?ERR_FORBIDDEN);
reply(Req, {error, Reason}) when is_atom(Reason) ->
    elib_response:error(Req, atom_to_binary(Reason), ?ERR_BAD_REQUEST);
reply(Req, _) ->
    elib_response:error(Req, <<"internal_error">>, ?ERR_INTERNAL_SERVER_ERROR).
