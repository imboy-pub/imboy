%% Real facade/application read chain; only the store boundary is synthetic.
-module(cs_seat_rating_detail_tests).
-include_lib("eunit/include/eunit.hrl").

rating_detail_test_() ->
    {foreach, fun setup/0, fun(_) -> cs_fake_store:destroy() end, [
        fun rated/0,
        fun unrated/0,
        fun missing/0,
        fun foreign_organization/0,
        fun foreign_workspace/0,
        fun disabled/0
    ]}.

setup() ->
    ok = cs_fake_store:init(),
    ok = cs_fake_store:put_seat_for_list(11, #{business_identity_id => 15, enabled => true}),
    ok.

row() ->
    #{
        id => 17,
        organization_id => 11,
        workspace_id => 12,
        status => closed,
        rating => 4,
        rating_at => 1700000000
    }.

read(Org, Ws) ->
    customer_service_facade:seat_session_detail(
        Org,
        #{store => cs_fake_store, business_identity_id => 15, session_id => 17, workspace_id => Ws}
    ).

rated() ->
    Row = row(),
    ok = cs_fake_store:put_session_for_list(Row),
    ?assertEqual({ok, Row}, read(11, 12)).

unrated() ->
    Row = (row())#{rating => undefined, rating_at => undefined},
    ok = cs_fake_store:put_session_for_list(Row),
    ?assertEqual({ok, Row}, read(11, 12)).

missing() ->
    Row = maps:without([rating, rating_at], row()),
    ok = cs_fake_store:put_session_for_list(Row),
    ?assertEqual({ok, Row}, read(11, 12)).

foreign_organization() ->
    ok = cs_fake_store:put_seat_for_list(21, #{business_identity_id => 15, enabled => true}),
    ok = cs_fake_store:put_session_for_list(row()),
    ?assertEqual({error, not_found}, read(21, 12)).

foreign_workspace() ->
    ok = cs_fake_store:put_session_for_list(row()),
    ?assertEqual({error, not_found}, read(11, 22)).

disabled() ->
    ok = cs_fake_store:put_seat_for_list(11, #{business_identity_id => 15, enabled => false}),
    ok = cs_fake_store:put_session_for_list(row()),
    ?assertEqual({error, seat_disabled}, read(11, 12)).
