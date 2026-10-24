-module(micro_community_http_integration_tests).
-export([run/0, run_cursor/0]).
-import(micro_community_owned_http_fixture, [
    assert/2,
    rows/3,
    exec/3,
    request/5,
    set_profile/1,
    business_snapshot/1,
    base_payload/1,
    body_content/0
]).

-define(CONFIG,
    "/Users/leeyi/.codex/micro-community-runs/mc-20261008T083109Z/private/pg-owned.config"
).
-define(OWNER_COMMENT,
    <<"Owned by MC run mc-20261008T083109Z; synthetic micro-community test data only">>
).
-define(LISTENER, mc_owned_http_listener).
-define(BASE, 9007199254744000).
-define(U, (?BASE + 1)).
-define(MANAGER, (?BASE + 2)).
-define(SUB, (?BASE + 3)).
-define(OUTSIDER, (?BASE + 4)).
-define(ADMIN, (?BASE + 5)).
-define(DENIED_ADMIN, (?BASE + 6)).
-define(G1, (?BASE + 10)).
-define(G2, (?BASE + 11)).
-define(C1, (?BASE + 20)).
-define(C2, (?BASE + 21)).
-define(C3, (?BASE + 22)).
-define(C4, (?BASE + 23)).
-define(BODY, (?BASE + 30)).
-define(TAG_ONLY, (?BASE + 31)).
-define(REVOKED, (?BASE + 32)).
-define(DELETED, (?BASE + 33)).
-define(FOREIGN, (?BASE + 34)).
-define(ADMIN_MSG, (?BASE + 35)).
-define(PKG, <<"mc.run20261008">>).
-define(SIGN_KEY, <<"mc.run20261008_android_1">>).
-define(NEEDLE, <<"mconlybodytoken">>).
-define(CN_NEEDLE, <<"微社群独有正文词"/utf8>>).

run() -> micro_community_owned_http_fixture:run(fun cases/1).

cases(Context) ->
    authentication_cases(Context),
    binding_cases(Context),
    admin_cases(Context),
    join_cases(Context),
    content_cases(Context),
    revocation_cases(Context),
    phase1_cases(Context),
    io:format("MC_HTTP_CASE_GROUPS_COMPLETED count=22~n"),
    ok.

authentication_cases(Ctx) ->
    unchanged(Ctx, h01, fun() ->
        {Status, Body} = request(Ctx, none, get, discussion(?C1), none),
        evidence(h01, Status, maps:get(<<"code">>, Body, invalid)),
        assert(Status =:= 401 andalso maps:get(<<"code">>, Body) =/= 0, {case_failed, h01}),
        ok
    end),
    unchanged(Ctx, h02, fun() ->
        expect(Ctx, bad_signature, get, discussion(?C1), none, 902, h02)
    end).

binding_cases(Ctx = #{conn := Conn}) ->
    expect(Ctx, ?U, put, discussion(?C1), binding_body(?G1), 0, h03),
    assert(
        binding(Conn, ?C1) =:= ?G1 andalso audit_count(Conn, ?G1, 301) =:= 1, {case_failed, h03}
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT option_uid,body::jsonb->>'channel_id',"
                "body::jsonb->>'old_group_id',body::jsonb->>'new_group_id' FROM public.group_log "
                "WHERE group_id=$1 AND type=301"
            >>,
            [?G1]
        ) =:= [{?U, bin(?C1), null, bin(?G1)}],
        {case_failed, h03}
    ),
    unchanged(Ctx, h04, fun() ->
        expect(Ctx, ?U, put, discussion(?C1), binding_body(?G1), 0, h04)
    end),
    unchanged(Ctx, h05, fun() ->
        expect(Ctx, ?SUB, put, discussion(?C1), binding_body(?G2), 403, h05),
        expect(Ctx, ?SUB, delete, discussion(?C1), none, 403, h05)
    end),
    unchanged(Ctx, h06, fun() ->
        expect(Ctx, ?U, put, discussion(?C2), binding_body(?G1), 409, h06)
    end).

admin_cases(Ctx = #{conn := Conn}) ->
    expect(Ctx, {admin, ?ADMIN}, put, admin_discussion(?C3), binding_body(?G2), 0, a01),
    assert(
        binding(Conn, ?C3) =:= ?G2 andalso audit_count(Conn, ?G2, 302) =:= 1, {case_failed, a01}
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT option_uid,body::jsonb->>'actor_kind',body::jsonb->>'admin_id' "
                "FROM public.group_log WHERE group_id=$1 AND type=302"
            >>,
            [?G2]
        ) =:=
            [{?ADMIN, <<"adm_user">>, bin(?ADMIN)}],
        {case_failed, a01}
    ),
    unchanged(Ctx, a02, fun() ->
        expect(Ctx, {admin, ?DENIED_ADMIN}, delete, admin_discussion(?C3), none, 403, a02)
    end),
    exec(Conn, <<"UPDATE public.adm_user SET role_id='{}' WHERE id=$1">>, [?ADMIN]),
    ok = adm_acl:flush_cache(?ADMIN),
    unchanged(Ctx, a02, fun() ->
        expect(Ctx, {admin, ?ADMIN}, delete, admin_discussion(?C3), none, 403, a02)
    end),
    exec(Conn, <<"UPDATE public.adm_user SET role_id=ARRAY[1::bigint] WHERE id=$1">>, [?ADMIN]),
    ok = adm_acl:flush_cache(?ADMIN),
    admin_essence_cases(Ctx).

admin_essence_cases(Ctx = #{conn := Conn}) ->
    admin_read_flags(Ctx, false),
    expect(Ctx, {admin, ?ADMIN}, put, admin_essence(?C3, ?ADMIN_MSG), essence_body(true), 0, a03),
    assert(message_flags(Conn, ?ADMIN_MSG) =:= {true, true}, {case_failed, a03}),
    admin_read_flags(Ctx, true),
    assert(
        rows(
            Conn,
            <<
                "SELECT body::jsonb->>'actor_kind',body::jsonb->>'admin_id',"
                "body::jsonb->>'message_id' FROM public.user_log WHERE uid=$1 AND type=901"
            >>,
            [?ADMIN]
        ) =:=
            [{<<"adm_user">>, bin(?ADMIN), bin(?ADMIN_MSG)}],
        {case_failed, a03}
    ),
    expect(Ctx, {admin, ?ADMIN}, put, admin_essence(?C3, ?ADMIN_MSG), essence_body(false), 0, a03),
    admin_read_flags(Ctx, false),
    unchanged(Ctx, a03, fun() ->
        expect(
            Ctx, {admin, ?ADMIN}, put, admin_essence(?C3, ?FOREIGN), essence_body(true), 404, a03
        )
    end).

admin_read_flags(Ctx = #{conn := Conn}, Essence) ->
    Path = <<"/api/adm/channel/", (bin(?C3))/binary, "/messages">>,
    Result = expect(Ctx, {admin, ?ADMIN}, get, Path, none, 0, a03),
    [Message] = [
        M
     || M <- maps:get(<<"list">>, payload(Result)), maps:get(<<"id">>, M) =:= bin(?ADMIN_MSG)
    ],
    assert(
        maps:get(<<"is_essence">>, Message, missing) =:= Essence andalso
            maps:get(<<"is_pinned">>, Message, missing) =:= true,
        {case_failed, a03}
    ),
    assert(message_flags(Conn, ?ADMIN_MSG) =:= {Essence, true}, {case_failed, a03}).

join_cases(Ctx = #{conn := Conn}) ->
    unchanged(Ctx, h07, fun() ->
        Body = expect(Ctx, ?SUB, get, discussion(?C1), none, 0, h07),
        assert(maps:get(<<"discussion_group_id">>, payload(Body)) =:= ?G1, {case_failed, h07})
    end),
    assert(member_generation(Conn, ?SUB) =:= {0, 0}, {case_failed, h07}),
    expect(Ctx, ?SUB, post, join_path(), #{}, 0, h08),
    assert(member_generation(Conn, ?SUB) =:= {1, 1}, {case_failed, h08}),
    assert(
        rows(
            Conn,
            <<
                "SELECT role,status,is_join,join_mode FROM public.group_member "
                "WHERE group_id=$1 AND user_id=$2"
            >>,
            [?G1, ?SUB]
        ) =:=
            [{1, 1, true, <<"channel_discussion">>}],
        {case_failed, h08}
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT generation_no,start_seq,end_seq FROM public.group_member_generation "
                "WHERE group_id=$1 AND user_id=$2"
            >>,
            [?G1, ?SUB]
        ) =:= [{1, 1, null}],
        {case_failed, h08}
    ),
    assert(
        rows(Conn, <<"SELECT member_count FROM public.\"group\" WHERE id=$1">>, [?G1]) =:=
            [{2}],
        {case_failed, h08}
    ),
    unchanged(Ctx, h09, fun() -> expect(Ctx, ?SUB, post, join_path(), #{}, 0, h09) end),
    unchanged(Ctx, h10, fun() -> expect(Ctx, ?OUTSIDER, post, join_path(), #{}, 403, h10) end),
    assert(member_generation(Conn, ?OUTSIDER) =:= {0, 0}, {case_failed, h10}),
    unchanged(Ctx, h11, fun() -> expect(Ctx, ?U, delete, discussion(?C1), none, 409, h11) end).

content_cases(Ctx = #{conn := Conn}) ->
    Body = expect(Ctx, ?SUB, get, posts(?C1, <<"q=mconlybodytoken">>), none, 0, h12),
    assert(
        result_ids(Body) =:= [?BODY] andalso maps:get(<<"total">>, payload(Body)) =:= 1,
        {case_failed, h12}
    ),
    assert(search_sql_ids(Conn, ?NEEDLE) =:= [?BODY], {case_failed, h12}),
    ChineseQs = iolist_to_binary(uri_string:compose_query([{<<"q">>, ?CN_NEEDLE}])),
    assert(binary:match(ChineseQs, <<"%">>) =/= nomatch, {case_failed, h12}),
    Chinese = expect(Ctx, ?SUB, get, posts(?C1, ChineseQs), none, 0, h12),
    assert(
        result_ids(Chinese) =:= [?BODY] andalso maps:get(<<"total">>, payload(Chinese)) =:= 1,
        {case_failed, h12}
    ),
    assert(search_sql_ids(Conn, ?CN_NEEDLE) =:= [?BODY], {case_failed, h12}),
    Tags = [<<" 公告 "/utf8>>, <<"公告"/utf8>>],
    expect(Ctx, ?MANAGER, put, message_path(?C1, ?BODY, tags), #{<<"tags">> => Tags}, 0, h13),
    Expected = (base_payload(?BODY))#{<<"tags">> => [<<"公告"/utf8>>]},
    assert(message_payload(Conn, ?BODY) =:= Expected, {case_failed, h13}),
    assert(
        rows(Conn, <<"SELECT content FROM public.channel_message WHERE id=$1">>, [?BODY]) =:=
            [{body_content()}],
        {case_failed, h13}
    ),
    unchanged(Ctx, h14, fun() ->
        expect(
            Ctx, ?MANAGER, put, message_path(?C1, ?BODY, tags), #{<<"tags">> => [<<>>]}, 400, h14
        )
    end),
    expect(Ctx, ?MANAGER, put, message_path(?C1, ?BODY, essence), essence_body(true), 0, h15),
    assert(message_flags(Conn, ?BODY) =:= {true, true}, {case_failed, h15}),
    search_filters(Ctx),
    unchanged(Ctx, h17, fun() ->
        expect(
            Ctx, ?U, put, message_path(?C1, ?FOREIGN, tags), #{<<"tags">> => [<<"x">>]}, 404, h17
        ),
        expect(Ctx, ?U, put, message_path(?C1, ?FOREIGN, essence), essence_body(true), 404, h17)
    end).

search_filters(Ctx = #{conn := Conn}) ->
    Qs = <<"tag=%E5%85%AC%E5%91%8A&essence=true&page=1&size=1">>,
    Body = expect(Ctx, ?SUB, get, posts(?C1, Qs), none, 0, h16),
    P = payload(Body),
    assert(
        result_ids(Body) =:= [?BODY] andalso maps:get(<<"total">>, P) =:= 1 andalso
            maps:get(<<"page">>, P) =:= 1 andalso maps:get(<<"size">>, P) =:= 1,
        {case_failed, h16}
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT id FROM public.channel_message WHERE channel_id=$1 "
                "AND status=1 AND NOT revoked AND is_essence AND payload->'tags' @> $2::jsonb "
                "ORDER BY id DESC"
            >>,
            [?C1, jsone:encode([<<"公告"/utf8>>])]
        ) =:= [{?BODY}],
        {case_failed, h16}
    ),
    Page = expect(Ctx, ?SUB, get, posts(?C1, <<"page=1&size=1">>), none, 0, h16),
    assert(
        result_ids(Page) =:= [?TAG_ONLY] andalso maps:get(<<"total">>, payload(Page)) =:= 2,
        {case_failed, h16}
    ).

revocation_cases(Ctx = #{conn := Conn}) ->
    exec(Conn, <<"DELETE FROM public.channel_admin WHERE id=$1">>, [?BASE + 50]),
    unchanged(Ctx, h18, fun() ->
        expect(Ctx, ?MANAGER, put, message_path(?C1, ?BODY, essence), essence_body(false), 403, h18)
    end),
    exec(Conn, <<"UPDATE public.channel_subscription SET status=0 WHERE id=$1">>, [?BASE + 51]),
    unchanged(Ctx, h18, fun() ->
        expect(Ctx, ?SUB, get, posts(?C1, <<>>), none, 403, h18),
        expect(Ctx, ?SUB, get, discussion(?C1), none, 403, h18),
        expect(Ctx, ?SUB, post, join_path(), #{}, 403, h18)
    end).

phase1_cases(Ctx) ->
    set_profile(phase1_delivery),
    Calls = [
        {?U, get, discussion(?C1), none},
        {?U, put, discussion(?C1), binding_body(?G1)},
        {?U, delete, discussion(?C1), none},
        {?U, get, append(discussion(?C1), <<"/candidates">>), none},
        {?U, post, join_path(), #{}},
        {?U, get, posts(?C1, <<>>), none},
        {?U, put, message_path(?C1, ?BODY, tags), #{<<"tags">> => [<<"x">>]}},
        {?U, put, message_path(?C1, ?BODY, essence), essence_body(false)},
        {{admin, ?ADMIN}, get, admin_discussion(?C3), none},
        {{admin, ?ADMIN}, put, admin_discussion(?C3), binding_body(?G2)},
        {{admin, ?ADMIN}, delete, admin_discussion(?C3), none},
        {{admin, ?ADMIN}, get, append(admin_discussion(?C3), <<"/candidates">>), none},
        {{admin, ?ADMIN}, put, admin_essence(?C3, ?ADMIN_MSG), essence_body(false)}
    ],
    [
        unchanged(Ctx, p01, fun() ->
            expect(Ctx, Identity, Method, Path, Body, 403, p01)
        end)
     || {Identity, Method, Path, Body} <- Calls
    ],
    ok.

unchanged(#{conn := Conn}, Case, Fun) ->
    Before = business_snapshot(Conn),
    Fun(),
    assert(business_snapshot(Conn) =:= Before, {case_failed, Case}).

expect(Ctx, Identity, Method, Path, Body, Expected, Case) ->
    {Status, Result} = request(Ctx, Identity, Method, Path, Body),
    Code = maps:get(<<"code">>, Result, invalid),
    evidence(Case, Status, Code),
    assert(Status =:= 200 andalso Code =:= Expected, {case_failed, Case}),
    Result.

evidence(Case, Status, Code) ->
    io:format(
        "MC_HTTP_CASE id=~p status=~p business_code=~p~n",
        [Case, safe_number(Status, 100, 599), safe_number(Code, 0, 9999)]
    ).

safe_number(Value, Low, High) when is_integer(Value), Value >= Low, Value =< High -> Value;
safe_number(_, _, _) -> invalid.

result_ids(Result) ->
    [maps:get(<<"id">>, R) || R <- maps:get(<<"list">>, payload(Result))].
payload(Result) -> maps:get(<<"payload">>, Result).
binding_body(Gid) -> #{<<"discussion_group_id">> => Gid}.
essence_body(Value) -> #{<<"is_essence">> => Value}.
discussion(Cid) -> <<"/api/v1/channel/", (bin(Cid))/binary, "/discussion">>.
admin_discussion(Cid) -> <<"/api/adm/channel/", (bin(Cid))/binary, "/discussion">>.
join_path() -> append(discussion(?C1), <<"/join">>).
posts(Cid, <<>>) -> <<"/api/v1/channel/", (bin(Cid))/binary, "/posts">>;
posts(Cid, Qs) -> append(posts(Cid, <<>>), <<"?", Qs/binary>>).
message_path(Cid, Mid, Kind) ->
    <<"/api/v1/channel/", (bin(Cid))/binary, "/posts/", (bin(Mid))/binary, "/",
        (atom_to_binary(Kind))/binary>>.
admin_essence(Cid, Mid) ->
    <<"/api/adm/channel/", (bin(Cid))/binary, "/message/", (bin(Mid))/binary, "/essence">>.
append(A, B) -> <<A/binary, B/binary>>.
bin(N) -> integer_to_binary(N).
binding(Conn, Cid) ->
    [{Value}] = rows(Conn, <<"SELECT discussion_group_id FROM public.channel WHERE id=$1">>, [Cid]),
    Value.
audit_count(Conn, Gid, Type) ->
    [{Count}] = rows(
        Conn, <<"SELECT count(*) FROM public.group_log WHERE group_id=$1 AND type=$2">>, [Gid, Type]
    ),
    Count.
member_generation(Conn, Uid) ->
    [{Members}] = rows(
        Conn, <<"SELECT count(*) FROM public.group_member WHERE group_id=$1 AND user_id=$2">>, [
            ?G1, Uid
        ]
    ),
    [{Generations}] = rows(
        Conn,
        <<"SELECT count(*) FROM public.group_member_generation WHERE group_id=$1 AND user_id=$2">>,
        [?G1, Uid]
    ),
    {Members, Generations}.
message_flags(Conn, Mid) ->
    [Flags] = rows(
        Conn, <<"SELECT is_essence,is_pinned FROM public.channel_message WHERE id=$1">>, [Mid]
    ),
    Flags.
message_payload(Conn, Mid) ->
    [{Json}] = rows(Conn, <<"SELECT payload::text FROM public.channel_message WHERE id=$1">>, [Mid]),
    jsone:decode(Json, [{object_format, map}]).
search_sql_ids(Conn, Needle) ->
    [
        Id
     || {Id} <- rows(
            Conn,
            <<
                "SELECT id FROM public.channel_message WHERE channel_id=$1 "
                "AND status=1 AND NOT revoked AND to_tsvector('jiebacfg'::regconfig,coalesce(content,'')) "
                "@@ plainto_tsquery('jiebacfg'::regconfig,$2) ORDER BY id DESC"
            >>,
            [?C1, Needle]
        )
    ].

%% Explicit opt-in, separate fresh fixture/VM admission from the 22-group run/0.
run_cursor() -> micro_community_owned_http_fixture:run_cursor(fun cursor_cases/1).

cursor_cases(Ctx = #{conn := Conn}) ->
    micro_community_owned_http_fixture:committed(Conn, fun cursor_seed_ties/1),
    unchanged(Ctx, tz05_ties, fun() -> cursor_ties(Ctx) end),
    micro_community_owned_http_fixture:committed(Conn, fun cursor_seed_microseconds/1),
    unchanged(Ctx, tz05_microseconds, fun() -> cursor_microseconds(Ctx) end),
    unchanged(Ctx, tz05_legacy, fun() -> cursor_legacy(Ctx) end),
    unchanged(Ctx, tz05_invalid, fun() -> cursor_invalid(Ctx) end),
    unchanged(Ctx, tz05_authorization, fun() -> cursor_authorization(Ctx) end),
    io:format("TZ05_CURSOR_HTTP_GROUPS_COMPLETED count=5~n"),
    ok.

cursor_seed_ties(Conn) ->
    %% C2 is fixture-owned but do not assume its data is empty.
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.channel_message WHERE channel_id=$1">>,
            [?C2]
        ) =:= [{0}],
        cursor_channel_not_empty
    ),
    [
        cursor_insert(Conn, ?BASE + 200 + N, <<"2026-10-24T01:02:03.123456Z">>)
     || N <- lists:seq(0, 24)
    ],
    assert(
        rows(
            Conn,
            <<
                "SELECT count(*),count(DISTINCT created_at) FROM public.channel_message "
                "WHERE channel_id=$1 AND id BETWEEN $2 AND $3"
            >>,
            [?C2, ?BASE + 200, ?BASE + 224]
        ) =:= [{25, 1}],
        cursor_ties_not_exact
    ),
    ok.

cursor_insert(Conn, Id, At) ->
    assert(Id >= ?BASE andalso Id =< ?BASE + 999, cursor_id_outside_owned_range),
    exec(
        Conn,
        <<
            "INSERT INTO public.channel_message(id,channel_id,author_id,content,created_at) "
            "VALUES($1,$2,$3,'TZ05 owned synthetic',$4::timestamptz)"
        >>,
        [Id, ?C2, ?U, At]
    ).

cursor_ties(Ctx) ->
    Expected = lists:seq(?BASE + 224, ?BASE + 200, -1),
    First = cursor_get(Ctx, ?U, undefined, 20, tz05_ties_first),
    assert(result_ids(First) =:= lists:sublist(Expected, 20), cursor_first_ids),
    Next = cursor_next(First),
    assert(is_binary(Next) andalso byte_size(Next) > 0, cursor_first_missing_next),
    assert(
        msg_offline_cursor:decode(Next) =:=
            {ok, {<<"2026-10-24T01:02:03.123456Z">>, bin(?BASE + 205)}},
        cursor_tie_boundary
    ),
    Last = cursor_get(Ctx, ?U, Next, 20, tz05_ties_last),
    assert(result_ids(Last) =:= lists:nthtail(20, Expected), cursor_last_ids),
    All = result_ids(First) ++ result_ids(Last),
    assert(All =:= Expected andalso length(lists:usort(All)) =:= 25, cursor_ids_not_complete),
    assert(cursor_next(Last) =:= null, cursor_last_not_terminal),
    End = msg_offline_cursor:encode({<<"2026-10-24T01:02:03.123456Z">>, bin(?BASE + 200)}),
    Empty = cursor_get(Ctx, ?U, End, 20, tz05_ties_empty),
    assert(result_ids(Empty) =:= [] andalso cursor_next(Empty) =:= null, cursor_end_restarted).

cursor_seed_microseconds(Conn) ->
    Entries = [
        {?BASE + 230, <<"2023-11-13T01:02:03.123458Z">>},
        {?BASE + 231, <<"2023-11-13T01:02:03.123457Z">>},
        {?BASE + 232, <<"2023-11-13T01:02:03.123456Z">>}
    ],
    [cursor_insert(Conn, Id, At) || {Id, At} <- Entries],
    Actual = rows(
        Conn,
        <<
            "SELECT id,to_char(created_at AT TIME ZONE 'UTC',"
            "'YYYY-MM-DD\"T\"HH24:MI:SS.US\"Z\"') FROM public.channel_message "
            "WHERE channel_id=$1 AND id BETWEEN $2 AND $3 ORDER BY created_at DESC,id DESC"
        >>,
        [?C2, ?BASE + 230, ?BASE + 232]
    ),
    assert(Actual =:= Entries, cursor_microseconds_not_persisted),
    ok.

cursor_microseconds(Ctx) ->
    Before = msg_offline_cursor:encode({<<"2023-11-13T01:02:03.123459Z">>, bin(?BASE + 233)}),
    First = cursor_get(Ctx, ?U, Before, 2, tz05_micro_first),
    assert(result_ids(First) =:= [?BASE + 230, ?BASE + 231], cursor_micro_first_ids),
    Next = cursor_next(First),
    assert(
        msg_offline_cursor:decode(Next) =:=
            {ok, {<<"2023-11-13T01:02:03.123457Z">>, bin(?BASE + 231)}},
        cursor_micro_precision
    ),
    Last = cursor_get(Ctx, ?U, Next, 2, tz05_micro_last),
    assert(
        result_ids(Last) =:= [?BASE + 232] andalso cursor_next(Last) =:= null,
        cursor_micro_last_ids
    ).

cursor_legacy(Ctx) ->
    [
        begin
            Result = cursor_get(Ctx, ?U, Token, 20, tz05_legacy_equivalent),
            assert(
                result_ids(Result) =:= [?BASE + 230, ?BASE + 231, ?BASE + 232] andalso
                    cursor_next(Result) =:= null,
                cursor_legacy_wrong_unit
            )
        end
     || Token <- [<<"1700000000">>, <<"1700000000000">>]
    ],
    Maxima = [4102444800, 4102444800000, 4102444800000000, 4102444800000000000],
    [
        begin
            Result = cursor_get(Ctx, ?U, bin(Max), 20, tz05_unit_max),
            assert(
                result_ids(Result) =:= lists:seq(?BASE + 224, ?BASE + 205, -1),
                cursor_unit_max_epoch_fallback
            ),
            assert(is_binary(cursor_next(Result)), cursor_max_missing_next),
            cursor_rejected(Ctx, bin(Max + 1), tz05_unit_overflow)
        end
     || Max <- Maxima
    ],
    ok.

cursor_invalid(Ctx) ->
    At = <<"2026-10-24T01:02:03.123456Z">>,
    Invalid = [
        <<"-1">>,
        <<"garbage">>,
        binary:copy(<<"9">>, 512),
        binary:copy(<<"a">>, 513),
        msg_offline_cursor:encode({At, <<"0">>}),
        msg_offline_cursor:encode({At, <<"-1">>}),
        msg_offline_cursor:encode({At, <<"9223372036854775808">>}),
        <<"1700000000", 10>>,
        msg_offline_cursor:encode({At, <<"1", 10>>}),
        msg_offline_cursor:encode({At, <<"1.0">>}),
        msg_offline_cursor:encode({<<"not-a-date">>, <<"1">>}),
        base64:encode(jsone:encode(#{<<"v">> => 2, <<"at">> => At, <<"id">> => <<"1">>})),
        base64:encode(
            jsone:encode(#{
                <<"v">> => 1,
                <<"at">> => At,
                <<"id">> => <<"1">>,
                <<"extra">> => true
            })
        )
    ],
    [cursor_rejected(Ctx, Token, tz05_bad_cursor) || Token <- Invalid],
    ok.

cursor_rejected(Ctx, Token, Case) ->
    {Status, Result} = request(Ctx, ?U, get, cursor_path(Token, 20), none),
    evidence(Case, Status, maps:get(<<"code">>, Result, invalid)),
    assert(
        Status =:= 200 andalso maps:get(<<"code">>, Result, invalid) =:= 1 andalso
            maps:get(<<"msg">>, Result, invalid) =:= <<"invalid_message_cursor">>,
        cursor_invalid_not_explicit
    ),
    assert(not maps:is_key(<<"list">>, payload(Result)), cursor_invalid_returned_page).

cursor_authorization(Ctx) ->
    Valid = msg_offline_cursor:encode({<<"2026-10-24T01:02:03.123456Z">>, bin(?BASE + 205)}),
    {401, Anon} = request(Ctx, none, get, cursor_path(Valid, 20), none),
    assert(maps:get(<<"code">>, Anon, 0) =/= 0, cursor_anonymous_allowed),
    expect(Ctx, bad_signature, get, cursor_path(Valid, 20), none, 902, tz05_signature),
    {Status, Denied} = request(Ctx, ?OUTSIDER, get, cursor_path(Valid, 20), none),
    evidence(tz05_outsider, Status, maps:get(<<"code">>, Denied, invalid)),
    assert(
        Status =:= 200 andalso maps:get(<<"code">>, Denied, 0) =/= 0 andalso
            maps:get(<<"msg">>, Denied, invalid) =:= <<"私有频道仅限订阅用户访问"/utf8>>,
        cursor_replaced_authorization
    ),
    assert(not maps:is_key(<<"list">>, payload(Denied)), cursor_outsider_received_page).

cursor_get(Ctx, Identity, Token, Limit, Case) ->
    expect(Ctx, Identity, get, cursor_path(Token, Limit), none, 0, Case).
cursor_next(Result) -> maps:get(<<"next_cursor">>, payload(Result)).
cursor_path(Token, Limit) ->
    Query =
        [{<<"limit">>, bin(Limit)}] ++
            case Token of
                undefined -> [];
                _ -> [{<<"cursor">>, Token}]
            end,
    Qs = uri_string:compose_query(Query),
    <<"/api/v1/channel/", (bin(?C2))/binary, "/messages?", Qs/binary>>.
