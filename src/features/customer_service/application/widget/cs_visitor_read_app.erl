%%% 访客确认及坐席阅读事实投影；正文/解密/附件仍只由企业 facade 输出。
-module(cs_visitor_read_app).
-export([confirm/2, list_messages/2]).
confirm(Org, Params) when is_map(Params) ->
    case cs_visitor_read:message_ids(maps:get(message_ids, Params, undefined)) of
        {error, _} = Error -> Error;
        {ok, Ids} -> confirm_scoped(Org, Params, Ids)
    end;
confirm(_, _) ->
    {error, {invalid_argument, message_ids}}.
confirm_scoped(Org, Params, Ids) ->
    case cs_widget_session_app:visitor_session_scope(Org, Params) of
        {error, _} = Error ->
            Error;
        {ok, #{workspace_id := Ws, contact_id := Contact, token := Token, session := Session}} ->
            Scope = #{
                session_id => maps:get(id, Session),
                conversation_id => maps:get(conversation_id, Session),
                contact_id => Contact,
                installation_id => maps:get(widget_installation_id, Token, undefined),
                token_id => maps:get(id, Token),
                origin => maps:get(origin, Params, undefined),
                request_host => maps:get(request_host, Params, undefined)
            },
            case
                cs_widget_support:fetch_installation(Params, Org, maps:get(installation_id, Scope))
            of
                {error, _} = Error ->
                    Error;
                {ok, Installation} ->
                    case
                        cs_widget:origin_allowed(
                            maps:get(origin, Scope),
                            maps:get(allowed_origins, Installation, []),
                            maps:get(request_host, Scope)
                        )
                    of
                        {error, _} = Error ->
                            Error;
                        _ ->
                            cs_app_support:with_store(Params, fun(Store) ->
                                Store:confirm_visitor_message_reads(
                                    Org, Ws, Scope, Ids, maps:get(at, Params)
                                )
                            end)
                    end
            end
    end.
list_messages(Org, Params) when is_map(Params) ->
    case cs_app_support:tenant(Org, Params) of
        {error, _} = Error ->
            Error;
        {ok, Ws} ->
            cs_app_support:with_store(Params, fun(Store) ->
                seat_messages(Store, Org, Ws, Params)
            end)
    end;
list_messages(_, _) ->
    {error, {invalid_argument, list_messages}}.
seat_messages(Store, Org, Ws, Params) ->
    Base = maps:with(
        [workspace_id, conversation_id, after_id, limit, business_identity_id], Params
    ),
    %% Preserve the existing HTTP/Enterprise history authorization before receipt lookup.
    case enterprise_business_facade:list_messages(Org, cs_widget_support:eb_params(Params, Base)) of
        {ok, Messages} when is_list(Messages) ->
            case Store:fetch_conversation_session(Org, Ws, maps:get(conversation_id, Params)) of
                {ok, Session} -> project(Store, Org, Ws, Session, Messages);
                {error, not_found} -> {ok, [maps:remove(read_at, Message) || Message <- Messages]};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error;
        _ ->
            {error, visitor_read_storage_failure}
    end.

project(Store, Org, Ws, Session, Messages) ->
    Scope = #{
        session_id => maps:get(id, Session),
        conversation_id => maps:get(conversation_id, Session),
        contact_id => maps:get(contact_id, Session)
    },
    Ids = [maps:get(id, Message) || Message <- Messages, eligible(Message)],
    case Store:fetch_visitor_message_reads(Org, Ws, Scope, Ids) of
        {error, _} = Error ->
            Error;
        {ok, Receipts} ->
            ById = maps:from_list([{maps:get(message_id, R), maps:get(read_at, R)} || R <- Receipts]),
            {ok, [with_receipt(Message, ById) || Message <- Messages]}
    end.
eligible(Message) ->
    lists:member(maps:get(sender_type, Message, undefined), [
        business_identity, <<"business_identity">>
    ]) andalso
        lists:member(maps:get(visibility, Message, undefined), [visible, <<"visible">>]).
with_receipt(Message, ById) ->
    %% 删除任何非收据来源的 read_at，禁止投递时间或游标伪造此事实。
    Clean = maps:remove(read_at, Message),
    case eligible(Message) andalso maps:is_key(maps:get(id, Message), ById) of
        true -> Clean#{read_at => maps:get(maps:get(id, Message), ById)};
        false -> Clean
    end.
