-module(talk_record_participant_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    ?assertEqual({ok, []}, talk_record_participant_logic:parse(#{<<"participants">> => []})),
    ?assertMatch(
        {ok, [#{user_id := 700001}, #{user_id := null, display_name := <<"guest">>}]},
        talk_record_participant_logic:parse(
            body([user(700001), #{<<"display_name">> => <<" guest ">>}])
        )
    ),
    lists:foreach(
        fun(B) ->
            ?assertEqual({error, invalid_participants}, talk_record_participant_logic:parse(B))
        end,
        [
            #{},
            body([user(1), user(1)]),
            body([#{<<"display_name">> => <<"x">>}, #{<<"display_name">> => <<" x ">>}]),
            body([#{<<"user_id">> => 1.5}]),
            body([#{<<"user_id">> => true}]),
            body([#{<<"display_name">> => <<>>}]),
            body([#{<<"display_name">> => <<"x\ny">>}]),
            body([#{<<"user_id">> => 1, <<"role">> => <<"editor">>}]),
            body(lists:duplicate(101, user(1)))
        ]
    ).

roundtrip_test_() -> db(fun roundtrip/1).
directory_test_() -> db(fun directory/1).
organization_test_() -> db(fun organization/1).
http_contract_test_() -> db(fun http_contract/1).

roundtrip(S) ->
    C = maps:get(conn, S),
    Rid = create(<<"personal">>, 0),
    ?assertEqual(
        {error, precondition_required},
        talk_record_participant_logic:replace(700001, Rid, undefined, body([]))
    ),
    ?assertEqual({error, not_found}, talk_record_participant_logic:list(700007, Rid)),
    Entries = [user(700001), user(700002), #{<<"display_name">> => <<"Guest">>}],
    {ok, P} = talk_record_participant_logic:replace(700001, Rid, <<"1">>, body(Entries)),
    ?assertEqual(2, maps:get(<<"revision">>, P)),
    ?assertEqual(<<"2">>, maps:get(<<"etag">>, P)),
    ?assertEqual(3, length(maps:get(<<"list">>, P))),
    ?assertEqual({ok, P}, talk_record_participant_logic:list(700001, Rid)),
    ?assertEqual({error, not_found}, talk_record_participant_logic:list(700002, Rid)),
    ?assertEqual(
        {error, precondition_failed},
        talk_record_participant_logic:replace(700001, Rid, <<"1">>, body([]))
    ),
    {ok, [#{<<"acl_revision">> := 1, <<"content_revision">> := 1, <<"grants">> := 0}]} = elib_pg:query(
        C,
        <<
            "SELECT acl_revision,content_revision,(SELECT count(*) FROM talk_record_grant WHERE record_id=$1) grants"
            " FROM talk_record WHERE id=$1"
        >>,
        [Rid]
    ),
    grant(C, Rid, 700002, <<"viewer">>),
    ?assertMatch({ok, _}, talk_record_participant_logic:list(700002, Rid)),
    ?assertEqual(
        {error, forbidden}, talk_record_participant_logic:replace(700002, Rid, <<"2">>, body([]))
    ),
    grant(C, Rid, 700003, <<"editor">>),
    {ok, Edit} = talk_record_participant_logic:replace(
        700003, Rid, <<"2">>, body([user(700003), #{<<"display_name">> => <<"guest">>}])
    ),
    ?assertEqual(3, maps:get(<<"revision">>, Edit)),
    Sid = talk_record_t15_support:seed_speaker(C, Rid, <<"speaker_00">>, #{}),
    ?assertMatch({ok, _}, talk_record_content_speakers:claim(700003, Rid, Sid)),
    {ok, _} = talk_record_participant_logic:replace(700001, Rid, <<"3">>, body([])),
    ?assertEqual({ok, false}, talk_record_speaker_repo:participant_exists_tx(C, Rid, 700003)).

directory(S) ->
    C = maps:get(conn, S),
    {ok, _} = elib_pg:execute(C, <<"DELETE FROM user_friend">>, []),
    {ok, _} = elib_pg:execute(
        C,
        <<
            "INSERT INTO user_friend(id,from_user_id,to_user_id,status) VALUES"
            " (970001,700001,700002,1),(970002,700002,700001,1),"
            " (970003,700001,700003,1),(970004,700007,700001,0)"
        >>,
        []
    ),
    {ok, P} = talk_record_grant_candidates:generic(
        700001, #{scope => <<"personal">>}, <<>>, undefined, 100
    ),
    Found = lists:sort([maps:get(<<"user_id">>, E) || E <- maps:get(<<"list">>, P)]),
    ?assertEqual([700001, 700002], Found),
    Row = #{<<"scope">> => <<"personal">>, <<"owner_user_id">> => 700001},
    ?assertEqual({ok, true}, talk_record_speaker_repo:target_in_directory_tx(C, Row, 700002)),
    {ok, _} = elib_pg:execute(C, <<"UPDATE \"user\" SET status=0 WHERE id=700002">>, []),
    ?assertEqual({ok, false}, talk_record_speaker_repo:target_in_directory_tx(C, Row, 700002)),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE \"user\" SET status=1,account_type=1 WHERE id=700002">>, []
    ),
    ?assertEqual({ok, false}, talk_record_speaker_repo:target_in_directory_tx(C, Row, 700002)),
    {ok, _} = elib_pg:execute(C, <<"UPDATE \"user\" SET account_type=0 WHERE id=700002">>, []),
    ?assertEqual({ok, false}, talk_record_speaker_repo:target_in_directory_tx(C, Row, 700003)),
    Rid = create(<<"personal">>, 0),
    ?assertEqual(
        {error, target_not_in_directory},
        talk_record_participant_logic:replace(700001, Rid, <<"1">>, body([user(700007)]))
    ),
    ?assertEqual(
        {ok, #{
            <<"record_id">> => Rid, <<"revision">> => 1, <<"etag">> => <<"1">>, <<"list">> => []
        }},
        talk_record_participant_logic:list(700001, Rid)
    ),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE user_friend SET status=0 WHERE from_user_id=700002">>, []
    ),
    ?assertEqual({ok, false}, talk_record_speaker_repo:target_in_directory_tx(C, Row, 700002)),
    {ok, _} = elib_pg:execute(C, <<"UPDATE \"user\" SET status=0 WHERE id=700001">>, []),
    ?assertEqual({ok, false}, talk_record_speaker_repo:target_in_directory_tx(C, Row, 700001)),
    {ok, _} = elib_pg:execute(C, <<"UPDATE \"user\" SET status=1 WHERE id=700001">>, []),
    ?assertEqual(
        {error, target_not_in_directory},
        talk_record_grant_logic:replace(
            700001,
            Rid,
            <<"1">>,
            #{<<"grants">> => [#{<<"grantee_user_id">> => 700002, <<"role">> => <<"viewer">>}]}
        )
    ).

organization(S) ->
    C = maps:get(conn, S),
    Rid = create(<<"organization">>, 720001),
    ?assertEqual(
        {error, target_not_in_directory},
        talk_record_participant_logic:replace(700001, Rid, <<"1">>, body([user(700004)]))
    ),
    ?assertEqual(
        {error, target_not_in_directory},
        talk_record_participant_logic:replace(700001, Rid, <<"1">>, body([user(700002)]))
    ),
    {ok, _} = elib_pg:execute(
        C,
        <<
            "INSERT INTO workspace_member(workspace_id,user_id,role,status)"
            " VALUES(720001,700002,'member','active')"
        >>,
        []
    ),
    ?assertMatch(
        {ok, _}, talk_record_participant_logic:replace(700001, Rid, <<"1">>, body([user(700002)]))
    ),
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE organization_member SET status='inactive' WHERE organization_id=710001 AND user_id=700001">>,
        []
    ),
    ?assertEqual({error, not_found}, talk_record_participant_logic:list(700001, Rid)),
    ?assertEqual(
        {error, not_found}, talk_record_participant_logic:replace(700001, Rid, <<"2">>, body([]))
    ).

http_contract(_S) ->
    Rid = create(<<"personal">>, 0),
    ok = meck:new(elib_response, [passthrough, unstick, no_link]),
    ok = meck:new(elib_param, [passthrough, unstick, no_link]),
    try
        meck:expect(elib_param, post, fun(_) -> body([user(700001)]) end),
        meck:expect(elib_response, success, fun(R, P) -> R#{status => 200, payload => P} end),
        meck:expect(elib_response, error_with_status, fun(R, Status, _Msg, _Code) ->
            R#{status => Status}
        end),
        Req = #{
            method => <<"PUT">>,
            bindings => #{id => integer_to_binary(Rid)},
            headers => #{<<"if-match">> => <<"1">>}
        },
        {ok, Resp, _} = talk_record_participant_handler:init(Req, #{current_uid => 700001}),
        ?assertEqual(200, maps:get(status, Resp)),
        ?assertEqual(<<"\"2\"">>, maps:get(<<"etag">>, maps:get(resp_headers, Resp))),
        {ok, Missing, _} = talk_record_participant_handler:init(Req#{headers => #{}}, #{
            current_uid => 700001
        }),
        ?assertEqual(428, maps:get(status, Missing)),
        {ok, Denied, _} = talk_record_participant_handler:init(Req#{method => <<"GET">>}, #{
            current_uid => 700007
        }),
        ?assertEqual(404, maps:get(status, Denied))
    after
        meck:unload(elib_param),
        meck:unload(elib_response)
    end.

body(L) -> #{<<"participants">> => L}.
user(U) -> #{<<"user_id">> => U}.
grant(C, Rid, Uid, Role) ->
    Id = elib_tsid:generate(talk_record),
    {ok, 1} = elib_pg:execute(
        C,
        <<"INSERT INTO talk_record_grant(id,record_id,grantee_user_id,role,granted_by) VALUES($1,$2,$3,$4,700001)">>,
        [Id, Rid, Uid, Role]
    ).
create(Scope, Ws) ->
    Base = #{
        <<"client_recording_id">> => <<"57000000-0000-4000-8000-000000000001">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => Scope
    },
    Body =
        case Scope of
            <<"organization">> -> Base#{<<"organization_id">> => 710001, <<"workspace_id">> => Ws};
            _ -> Base
        end,
    {ok, V} = talk_record_logic:parse_create(Body),
    {ok, D} = talk_record_logic:request_digest(Body),
    {ok, created, P, _} = talk_record_ds:create(700001, V, <<"participants-create">>, D),
    maps:get(<<"id">>, P).
db(F) ->
    {timeout, 120,
        {setup, fun setup/0, fun talk_record_t15_support:teardown_all/1, fun(S) -> ?_test(F(S)) end}}.
setup() ->
    S = talk_record_t15_support:setup_all(),
    C = maps:get(conn, S),
    talk_record_t05_support:sql(
        C, "ALTER TABLE \"user\" ADD COLUMN IF NOT EXISTS status smallint NOT NULL DEFAULT 1"
    ),
    talk_record_t05_support:sql(
        C, "ALTER TABLE \"user\" ADD COLUMN IF NOT EXISTS account_type smallint NOT NULL DEFAULT 0"
    ),
    talk_record_t05_support:sql(
        C, "ALTER TABLE \"user\" ADD COLUMN IF NOT EXISTS avatar text NOT NULL DEFAULT ''"
    ),
    talk_record_t05_support:sql(
        C,
        "CREATE TABLE IF NOT EXISTS user_friend (id bigint PRIMARY KEY,from_user_id bigint NOT NULL,to_user_id bigint NOT NULL,status smallint NOT NULL DEFAULT 1)"
    ),
    talk_record_t05_support:sql(C, "DELETE FROM user_friend"),
    talk_record_t05_support:seed_fixtures(C),
    talk_record_t05_support:sql(
        C,
        "INSERT INTO user_friend(id,from_user_id,to_user_id,status) VALUES (960001,700001,700002,1),(960002,700002,700001,1),(960003,700001,700003,1),(960004,700003,700001,1)"
    ),
    S.
