-module(cs_inbox_contract_tests).

-include_lib("eunit/include/eunit.hrl").

filters_fail_closed_test() ->
    Base = #{business_identity_id => 41},
    ?assertEqual({ok, Base#{mine => false, search => <<>>}}, cs_pg_inbox:filters(Base)),
    ?assertEqual(
        {ok, Base#{mine => true, search => <<"王***明"/utf8>>}},
        cs_pg_inbox:filters(Base#{mine => <<"true">>, search => <<" 王***明 "/utf8>>})
    ),
    lists:foreach(
        fun(Invalid) ->
            ?assertEqual(
                {error, {invalid_param_value, business_identity_id}},
                cs_pg_inbox:filters(Base#{business_identity_id => Invalid})
            )
        end,
        [undefined, 0, -1, <<"41">>, 9223372036854775808]
    ),
    ?assertEqual({error, {invalid_param_value, business_identity_id}}, cs_pg_inbox:filters(#{})),
    lists:foreach(
        fun(Invalid) ->
            ?assertEqual(
                {error, {invalid_param_value, mine}},
                cs_pg_inbox:filters(Base#{mine => Invalid})
            )
        end,
        [undefined, <<>>, <<"1">>, <<"yes">>, 1]
    ),
    lists:foreach(
        fun(Invalid) ->
            ?assertEqual(
                {error, {invalid_param_value, search}},
                cs_pg_inbox:filters(Base#{search => Invalid})
            )
        end,
        [undefined, 1, <<255>>, binary:copy(<<"a">>, 129)]
    ),
    ?assertEqual(
        {error, {unexpected_argument, preview}},
        cs_pg_inbox:filters(Base#{preview => <<"secret">>})
    ).

fake_read_and_reply_test() ->
    cs_fake_store:init(),
    Session = session(101, active, 41),
    ok = cs_fake_store:put_session_for_list(Session),
    seed(501, contact, visible),
    seed(502, contact, visible),
    seed(503, contact, hidden),
    First = only_row(page(active, 0, 10, #{})),
    ?assertEqual(2, maps:get(unread_count, First)),
    ?assertEqual(true, maps:get(needs_reply, First)),
    ?assertEqual(502, maps:get(last_message_id, First)),
    {ok, _} = cs_fake_store:ack_session_read(7, 11, 101, 41, 502, 100),
    Read = only_row(page(active, 0, 10, #{})),
    ?assertEqual(0, maps:get(unread_count, Read)),
    ?assertEqual(true, maps:get(needs_reply, Read)),
    Other = only_row(page(active, 0, 10, #{business_identity_id => 42})),
    ?assertEqual(2, maps:get(unread_count, Other)),
    seed(504, business_identity, visible),
    Replied = only_row(page(active, 0, 10, #{})),
    ?assertEqual(false, maps:get(needs_reply, Replied)),
    ?assertEqual(504, maps:get(last_message_id, Replied)),
    seed(505, contact, visible),
    ?assertEqual(true, maps:get(needs_reply, only_row(page(active, 0, 10, #{})))),
    _ = cs_fake_store:seed_message(8, 11, 17, #{
        id => 900,
        sender_type => contact,
        visibility => visible
    }),
    _ = cs_fake_store:seed_message(7, 12, 17, #{
        id => 901,
        sender_type => contact,
        visibility => visible
    }),
    ?assertEqual(1, maps:get(unread_count, only_row(page(active, 0, 10, #{})))),
    lists:foreach(
        fun(ClosedStatus) ->
            ok = cs_fake_store:put_session_for_list(Session#{status => ClosedStatus}),
            Closed = only_row(page(closed, 0, 10, #{})),
            ?assertEqual(false, maps:get(needs_reply, Closed)),
            ?assertEqual(1, maps:get(unread_count, Closed))
        end,
        [closed, <<"closed">>]
    ).

%% CX-04 合同（合同层镜像；真库行为由 cs04_ack_cross_conversation_reject_
%% red_tests 守卫）：候选 id 是别的 conversation 的真实消息 ⇒ {error,
%% cross_conversation_message}，游标不动；不存在的 id 仍按 CS-DEC-02 clamp。
cross_conversation_ack_rejected_test() ->
    cs_fake_store:init(),
    Session = session(101, active, 41),
    ok = cs_fake_store:put_session_for_list(Session),
    seed(601, contact, visible),
    {ok, _} = cs_fake_store:ack_session_read(7, 11, 101, 41, 601, 100),
    %% 同 org/ws、别的 conversation（18）的真实消息 id。
    _ = cs_fake_store:seed_message(7, 11, 18, #{
        id => 700,
        sender_type => contact,
        visibility => visible,
        created_at => 100
    }),
    ?assertEqual(
        {error, {cross_conversation_message, 700}},
        cs_fake_store:ack_session_read(7, 11, 101, 41, 700, 101)
    ),
    %% 不存在的 id（未来/已 purge）仍 clamp：收敛回本会话上界 601。
    {ok, Clamped} = cs_fake_store:ack_session_read(7, 11, 101, 41, 999999, 102),
    ?assertEqual(601, maps:get(last_read_message_id, Clamped)),
    %% 拒绝之后游标仍停在 601（幂等回读），unread 只计本 scope
    %% （锁住 read_state_of 显式谓词化：外来 conversation 消息不计入）。
    {ok, After} = cs_fake_store:ack_session_read(7, 11, 101, 41, 601, 103),
    ?assertEqual(601, maps:get(last_read_message_id, After)),
    ?assertEqual(0, maps:get(unread_count, After)).

sql_closed_reply_guard_test() ->
    [Sql] = cs_pg_inbox:sql_statements(),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Sql,
            <<"(p.status <> 'closed' AND COALESCE(facts.contact_id, 0)>COALESCE(facts.business_id, 0)) AS needs_reply">>
        )
    ).

fake_reply_status_guard_test() ->
    cs_fake_store:init(),
    seed(501, contact, visible),
    lists:foreach(
        fun({Status, NeedsReply}) ->
            ok = cs_fake_store:put_session_for_list(session(101, Status, 41)),
            Row = only_row(page(Status, 0, 10, #{})),
            ?assertEqual(NeedsReply, maps:get(needs_reply, Row)),
            ?assertEqual(1, maps:get(unread_count, Row))
        end,
        [
            {queued, true},
            {<<"queued">>, true},
            {active, true},
            {<<"active">>, true},
            {closed, false},
            {<<"closed">>, false}
        ]
    ).

fake_search_counts_and_cursor_test() ->
    cs_fake_store:init(),
    lists:foreach(
        fun(Session) -> ok = cs_fake_store:put_session_for_list(Session) end,
        [
            session(101, active, 41),
            session(102, active, 41),
            session(103, active, 42),
            session(104, closed, 41),
            session(105, queued, undefined),
            (session(201, active, 41))#{organization_id => 8},
            (session(202, active, 41))#{workspace_id => 12}
        ]
    ),
    Mine = page(active, 0, 1, #{mine => true}),
    ?assertEqual(2, maps:get(total, Mine)),
    ?assertEqual(
        #{<<"queued">> => 0, <<"active">> => 2, <<"closed">> => 1},
        maps:get(total_by_status, Mine)
    ),
    ?assertEqual(102, maps:get(id, only_row(Mine))),
    ?assertEqual(101, maps:get(id, only_row(page(active, 102, 1, #{mine => true})))),
    ?assertEqual(2, maps:get(total, page(active, 102, 1, #{mine => true}))),
    ?assertEqual(101, maps:get(id, only_row(page(active, 0, 1, #{search => <<"101">>})))),
    ?assertEqual(0, maps:get(total, page(active, 0, 10, #{search => <<"秘密"/utf8>>}))),
    ?assertEqual(3, maps:get(total, page(active, 0, 10, #{search => <<"王***明"/utf8>>}))),
    ?assertEqual(3, maps:get(total, page(active, 0, 10, #{search => <<"SHOP_KEY">>}))),
    ?assertEqual(0, maps:get(total, page(active, 0, 10, #{search => <<"%' OR true --">>}))),
    ?assertEqual([], maps:get(rows, page(active, 0, 10, #{mine => true, search => <<"103">>}))).

store_autoload_test() ->
    cs_fake_store:init(),
    ok = cs_fake_store:put_session_for_list(session(101, active, 41)),
    _ = code:purge(cs_fake_store),
    true = code:delete(cs_fake_store),
    ?assertNot(erlang:function_exported(cs_fake_store, seat_session_page, 6)),
    {ok, #{sessions := [Row]}} = cs_session_app:seat_session_page(
        7,
        #{
            store => cs_fake_store,
            status => <<"active">>,
            workspace_id => 11,
            business_identity_id => 41,
            mine => true
        }
    ),
    ?assertEqual(101, maps:get(id, Row)),
    ?assertEqual(0, maps:get(unread_count, Row)).

application_compatibility_and_exports_test() ->
    cs_fake_store:init(),
    ok = cs_fake_store:put_session_for_list(session(101, active, 41)),
    Params = #{store => cs_fake_store, status => <<"active">>, workspace_id => 11},
    {ok, #{sessions := [Old]}} = cs_session_app:seat_session_page(7, Params),
    ?assertNot(maps:is_key(unread_count, Old)),
    {ok, #{sessions := [New]}} = cs_session_app:seat_session_page(
        7,
        Params#{business_identity_id => 41, mine => <<"true">>}
    ),
    ?assertEqual(0, maps:get(unread_count, New)),
    ?assertEqual(false, maps:get(needs_reply, New)),
    ?assertEqual(
        {error, {invalid_param_value, business_identity_id}},
        cs_session_app:seat_session_page(7, Params#{mine => false})
    ),
    ?assertEqual(
        {error, {invalid_param_value, business_identity_id}},
        cs_session_app:seat_session_page(7, Params#{business_identity_id => undefined})
    ),
    ?assertEqual(
        {error, invalid_workspace_id},
        cs_session_app:seat_session_page(
            7,
            Params#{business_identity_id => 41, workspace_id => -1}
        )
    ),
    ?assertEqual(
        {error, {unimplemented_port, {cs_store_port, seat_session_page, 6}}},
        cs_session_app:seat_session_page(
            7,
            Params#{store => cs_store_port, business_identity_id => 41}
        )
    ),
    lists:foreach(
        fun(Arity) ->
            ?assert(
                lists:member({seat_session_page, Arity}, cs_store_port:behaviour_info(callbacks))
            ),
            ?assert(
                lists:member(
                    {seat_session_page, Arity}, maps:get(cs_store_port, cs_ports:contracts())
                )
            ),
            lists:foreach(
                fun(Module) ->
                    ?assert(lists:member({seat_session_page, Arity}, Module:module_info(exports)))
                end,
                [cs_pg_session, cs_pg_store, cs_fake_store]
            )
        end,
        [5, 6]
    ),
    {ok, Queue} = cs_actions:find(tenant, session_queue),
    ?assert(lists:member(business_identity_id, maps:get(client_forbidden, Queue))),
    {ok, GetQueue} = cs_actions:case_for(Queue, <<"GET">>),
    ?assert(lists:member({mine, binary, optional}, maps:get(params, GetQueue))),
    ?assert(lists:member({search, binary, optional}, maps:get(params, GetQueue))),
    ?assertEqual(
        {error, {server_derived_key_rejected, business_identity_id}},
        cs_http:check_forbidden(
            #{},
            [{<<"business_identity_id">>, <<"42">>}],
            maps:get(client_forbidden, Queue)
        )
    ).

session(SessionId, Status, IdentityId) ->
    #{
        id => SessionId,
        organization_id => 7,
        workspace_id => 11,
        contact_id => 13,
        conversation_id => 17,
        business_identity_id => IdentityId,
        status => Status,
        version => 1,
        queued_at => 100,
        contact_display_name => <<"王秘密明"/utf8>>
    }.

seed(MessageId, Sender, Visibility) ->
    cs_fake_store:seed_message(7, 11, 17, #{
        id => MessageId,
        sender_type => Sender,
        visibility => Visibility,
        created_at => 100
    }).

page(Status, Cursor, Limit, Extra) ->
    Filters = maps:merge(#{business_identity_id => 41}, Extra),
    {ok, Page} = cs_fake_store:seat_session_page(7, Status, Cursor, Limit, 11, Filters),
    Page.

only_row(#{rows := [Row]}) -> Row.
