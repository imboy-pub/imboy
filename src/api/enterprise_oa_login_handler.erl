-module(enterprise_oa_login_handler).
-export([init/2]).

init(Req, State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"POST">> -> handle(maps:get(action, State), Req, State);
            _ -> cowboy_req:reply(405, #{}, <<>>, Req)
        end,
    {ok, Reply, State}.

handle(Action, Req, State) ->
    Params = elib_param:post(Req),
    Result =
        case Action of
            issue ->
                enterprise_oa_login_logic:issue(maps:get(enterprise_internal, State, #{}), Params);
            exchange ->
                enterprise_oa_login_logic:exchange(Params);
            register ->
                enterprise_oa_login_logic:register(Params);
            link ->
                link(Req, State, Params)
        end,
    case Result of
        {ok, Body} ->
            elib_response:success(Req, Body);
        {error, Reason} when Action =:= issue ->
            enterprise_internal_error:reply(Req, machine_error(Reason));
        {error, Reason} ->
            %% Public client endpoints use the standard transformer envelope.
            {Status, Code} = error_code(Reason),
            elib_response:error_with_status(Req, Status, Code, Status)
    end.

machine_error(insufficient_scope) -> <<"insufficient_scope">>;
machine_error(invalid_request) -> <<"invalid_request">>;
machine_error(invalid_ticket) -> <<"invalid_credential">>;
machine_error(_) -> <<"internal_error">>.

link(Req, State, P) ->
    case
        auth_ds:parse_authorization_header(
            cowboy_req:header(<<"authorization">>, Req, <<>>)
        )
    of
        Token when is_binary(Token), Token =/= <<>> ->
            Uid = auth_ds:current_uid(State),
            Did = auth_ds:current_did(State),
            case token_ds:decrypt_token(Token) of
                {ok, Uid, Expiry, <<"tk">>, Did, Epoch} when is_integer(Epoch), Did =/= <<>> ->
                    enterprise_oa_login_logic:link(Uid, Did, {Epoch, Expiry}, P);
                _ ->
                    {error, invalid_session}
            end;
        _ ->
            {error, invalid_session}
    end.

error_code(identity_conflict) -> {409, <<"identity_conflict">>};
error_code(user_already_mapped) -> {409, <<"identity_conflict">>};
error_code(insufficient_scope) -> {403, <<"insufficient_scope">>};
error_code(invalid_session) -> {401, <<"invalid_session">>};
error_code(invalid_ticket) -> {401, <<"invalid_ticket">>};
error_code(invalid_request) -> {400, <<"invalid_request">>};
error_code(explicit_confirmation_required) -> {400, <<"explicit_confirmation_required">>};
error_code(account_quota_exceeded) -> {402, <<"account_quota_exceeded">>};
error_code(_) -> {500, <<"internal_error">>}.
