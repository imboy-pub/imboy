-module(cs_inbox_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

read_and_reply_facts_test() ->
    with_scope(fun(Scope) ->
        Session = insert_session(Scope, <<"active">>),
        First = insert_message(Scope, <<"contact">>, <<"visible">>),
        Last = insert_message(Scope, <<"contact">>, <<"visible">>),
        _Hidden = insert_message(Scope, <<"contact">>, <<"hidden">>),
        Row = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(2, maps:get(unread_count, Row)),
        ?assertEqual(true, maps:get(needs_reply, Row)),
        ?assertEqual(Last, maps:get(last_message_id, Row)),
        insert_read_cursor(Scope, Session, identity(Scope), Last),
        Read = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(0, maps:get(unread_count, Read)),
        ?assertEqual(true, maps:get(needs_reply, Read)),
        Reply = insert_message(Scope, <<"business_identity">>, <<"visible">>),
        Replied = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(false, maps:get(needs_reply, Replied)),
        ?assertEqual(Reply, maps:get(last_message_id, Replied)),
        _FollowUp = insert_message(Scope, <<"contact">>, <<"visible">>),
        ?assertEqual(true, maps:get(needs_reply, only_row(page(Scope, <<"active">>, #{})))),
        ok = ?FIX:exec(
            <<"UPDATE customer_service_session SET status='closed',closed_at=CURRENT_TIMESTAMP WHERE organization_id=$1 AND id=$2">>,
            [org(Scope), Session]
        ),
        ?assertEqual(false, maps:get(needs_reply, only_row(page(Scope, <<"closed">>, #{})))),
        ?assert(Last > First)
    end).

%% CSB-02R：summary_match_ids 面（阶段 B SQL 注入）——命中集合 = 计数集合
%% = 分页集合；空集恒不改变形状；非法项 fail-closed。
summary_match_ids_face_test() ->
    with_scope(fun(Scope) ->
        SessionA = insert_session(Scope, <<"active">>),
        _MsgA = insert_message(Scope, <<"contact">>, <<"visible">>),
        SessionB = insert_session(Scope, <<"active">>),
        _MsgB = insert_message(Scope, <<"contact">>, <<"visible">>),
        Page = page(Scope, <<"active">>, #{summary_match_ids => [SessionA]}),
        ?assertEqual(1, maps:get(total, Page)),
        ?assertEqual([SessionA], [maps:get(id, R) || R <- maps:get(rows, Page)]),
        PageAll = page(Scope, <<"active">>, #{summary_match_ids => []}),
        ?assertEqual(2, maps:get(total, PageAll)),
        ?assertMatch(
            {error, {invalid_param_value, summary_match_ids}},
            cs_pg_store:seat_session_page(
                org(Scope),
                <<"active">>,
                0,
                20,
                ws(Scope),
                filters(Scope, #{summary_match_ids => [bad]})
            )
        )
    end).

server_search_and_cursor_test() ->
    with_scope(fun(Scope) ->
        Sessions = [insert_session(Scope, <<"closed">>) || _Index <- lists:seq(1, 7)],
        [Oldest | _Rest] = Sessions,
        {ok, First} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"closed">>,
            0,
            2,
            ws(Scope),
            filters(Scope, #{})
        ),
        ?assertEqual(7, maps:get(total, First)),
        FirstRows = maps:get(rows, First),
        Cursor = maps:get(id, lists:last(FirstRows)),
        {ok, Second} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"closed">>,
            Cursor,
            2,
            ws(Scope),
            filters(Scope, #{})
        ),
        ?assertEqual([], [
            maps:get(id, Row)
         || Row <- FirstRows,
            lists:any(
                fun(Next) -> maps:get(id, Next) =:= maps:get(id, Row) end,
                maps:get(rows, Second)
            )
        ]),
        Match = page(Scope, <<"closed">>, #{search => integer_to_binary(Oldest)}),
        ?assertEqual(Oldest, maps:get(id, only_row(Match))),
        ?assertEqual(1, maps:get(total, Match)),
        ?assertEqual(false, maps:get(needs_reply, only_row(Match))),
        ?assertEqual(
            0, maps:get(total, page(Scope, <<"closed">>, #{search => <<"%' OR true --">>}))
        )
    end).

mine_and_scope_test() ->
    with_scope(fun(Scope) ->
        Session = insert_session(Scope, <<"active">>),
        ?assertEqual(Session, maps:get(id, only_row(page(Scope, <<"active">>, #{mine => true})))),
        WrongIdentity = filters(Scope, #{
            mine => true, business_identity_id => maps:get(sales_identity_id, Scope)
        }),
        {ok, OtherSeat} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            0,
            20,
            ws(Scope),
            WrongIdentity
        ),
        ?assertEqual([], maps:get(rows, OtherSeat)),
        ?assertEqual(0, maps:get(total, OtherSeat)),
        {ok, OtherWorkspace} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            0,
            20,
            maps:get(other_workspace_id, Scope),
            filters(Scope, #{})
        ),
        ?assertEqual([], maps:get(rows, OtherWorkspace)),
        {ok, OtherOrg} = cs_pg_store:seat_session_page(
            maps:get(other_org_id, Scope),
            <<"active">>,
            0,
            20,
            ws(Scope),
            filters(Scope, #{search => integer_to_binary(Session)})
        ),
        ?assertEqual([], maps:get(rows, OtherOrg))
    end).

read_cursor_is_identity_local_test() ->
    with_scope(fun(Scope) ->
        Session = insert_session(Scope, <<"active">>),
        First = insert_message(Scope, <<"contact">>, <<"visible">>),
        Last = insert_message(Scope, <<"contact">>, <<"visible">>),
        OtherIdentity = insert_other_seat(Scope),
        Unread = only_row(page(Scope, <<"active">>, #{business_identity_id => OtherIdentity})),
        ?assertEqual(2, maps:get(unread_count, Unread)),
        insert_read_cursor(Scope, Session, OtherIdentity, First),
        insert_read_cursor(Scope, Session, identity(Scope), Last),
        ?assertEqual(0, maps:get(unread_count, only_row(page(Scope, <<"active">>, #{})))),
        Other = only_row(
            page(
                Scope,
                <<"active">>,
                #{business_identity_id => OtherIdentity}
            )
        ),
        ?assertEqual(1, maps:get(unread_count, Other)),
        ?assertEqual(
            First,
            ?FIX:scalar(
                <<
                    "SELECT last_read_message_id FROM customer_service_read_cursor"
                    " WHERE organization_id=$1 AND session_id=$2 AND business_identity_id=$3"
                >>,
                [org(Scope), Session, OtherIdentity],
                0
            )
        ),
        ?assertEqual(true, maps:get(needs_reply, Other)),
        ?assertEqual(Last, maps:get(last_message_id, Other))
    end).

hidden_business_reply_does_not_clear_waiting_test() ->
    with_scope(fun(Scope) ->
        _Session = insert_session(Scope, <<"active">>),
        Contact = insert_message(Scope, <<"contact">>, <<"visible">>),
        HiddenReply = insert_message(Scope, <<"business_identity">>, <<"hidden">>),
        ?assert(HiddenReply > Contact),
        Waiting = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(true, maps:get(needs_reply, Waiting)),
        ?assertEqual(1, maps:get(unread_count, Waiting)),
        ?assertEqual(Contact, maps:get(last_message_id, Waiting)),
        Reply = insert_message(Scope, <<"business_identity">>, <<"visible">>),
        Replied = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(false, maps:get(needs_reply, Replied)),
        ?assertEqual(Reply, maps:get(last_message_id, Replied)),
        ?assertEqual(1, maps:get(unread_count, Replied))
    end).

masked_search_does_not_expose_name_test() ->
    with_scope(fun(Scope) ->
        _Session = insert_session(Scope, <<"queued">>),
        ok = ?FIX:exec(
            <<"UPDATE enterprise_contact SET display_name=$1 WHERE organization_id=$2 AND id=$3">>,
            [unicode:characters_to_binary("王秘密明"), org(Scope), maps:get(contact_id, Scope)]
        ),
        ?assertEqual(
            0,
            maps:get(
                total, page(Scope, <<"queued">>, #{search => unicode:characters_to_binary("秘密")})
            )
        ),
        ?assertEqual(
            1,
            maps:get(
                total, page(Scope, <<"queued">>, #{search => unicode:characters_to_binary("王***明")})
            )
        )
    end).

with_scope(Check) ->
    Scope = ?FIX:new_scope(),
    try
        Check(Scope)
    after
        ?FIX:cleanup(Scope)
    end.

page(Scope, Status, Extra) ->
    {ok, Page} = cs_pg_store:seat_session_page(
        org(Scope), Status, 0, 20, ws(Scope), filters(Scope, Extra)
    ),
    Page.

filters(Scope, Extra) ->
    maps:merge(#{business_identity_id => identity(Scope), mine => false, search => <<>>}, Extra).

only_row(#{rows := [Row]}) -> Row.
org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).
identity(Scope) -> maps:get(service_identity_id, Scope).

insert_other_seat(Scope) ->
    Identity = ?FIX:id(),
    Owner = maps:get(owner_user_id, Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
            " VALUES ($1,$2,'customer_service',$3,'active',1,$4)"
        >>,
        [Identity, org(Scope), integer_to_binary(Identity), Owner]
    ),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_seat"
            " (organization_id,business_identity_id,function_key,enabled,max_concurrent,created_by_user_id)"
            " VALUES ($1,$2,'customer_service',true,1,$3)"
        >>,
        [org(Scope), Identity, Owner]
    ),
    Identity.

insert_read_cursor(Scope, Session, Identity, LastMessage) ->
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_read_cursor"
            " (id,organization_id,workspace_id,session_id,business_identity_id,last_read_message_id)"
            " VALUES ($1,$2,$3,$4,$5,$6)"
        >>,
        [?FIX:id(), org(Scope), ws(Scope), Session, Identity, LastMessage]
    ).

insert_session(Scope, Status) ->
    Session = ?FIX:id(),
    Identity =
        case Status of
            <<"queued">> -> null;
            _Other -> identity(Scope)
        end,
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_session"
            " (id,organization_id,workspace_id,contact_id,conversation_id,business_identity_id,status,closed_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,CASE WHEN $7='closed' THEN CURRENT_TIMESTAMP END)"
        >>,
        [
            Session,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(conversation_id, Scope),
            Identity,
            Status
        ]
    ),
    Session.

insert_message(Scope, Sender, Visibility) ->
    Message = ?FIX:id(),
    {Contact, Identity, Actor} =
        case Sender of
            <<"contact">> -> {maps:get(contact_id, Scope), null, null};
            <<"business_identity">> -> {null, identity(Scope), maps:get(actor_user_id, Scope)}
        end,
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,"
            " sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until,visibility)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,1095,CURRENT_TIMESTAMP + interval '1095 days',$10)"
        >>,
        [
            Message,
            org(Scope),
            ws(Scope),
            maps:get(conversation_id, Scope),
            Sender,
            Contact,
            Identity,
            Actor,
            integer_to_binary(Message),
            Visibility
        ]
    ),
    Message.
