-module(eb_asset_v5_put_unit_tests).
-include_lib("eunit/include/eunit.hrl").

put_test_() -> {foreach,fun setup/0,fun cleanup/1,[
    fun registered_put_reuses_reserved_row_and_context/0,
    fun rejected_reservation_never_writes/0]}.
setup() ->
    Prior = application:get_env(imboy,eb_asset_object_store),
    application:set_env(imboy,eb_asset_object_store,stub),
    meck:new(eb_pg_asset_meta,[non_strict,no_link]),
    meck:new(eb_asset_object_stub,[passthrough,no_link]),
    meck:expect(eb_pg_asset_meta,insert_asset,fun(_,_,_) -> {error,conflict} end),
    Prior.
cleanup(Prior) ->
    meck:unload(eb_pg_asset_meta),meck:unload(eb_asset_object_stub),
    case Prior of {ok,V} -> application:set_env(imboy,eb_asset_object_store,V); undefined -> application:unset_env(imboy,eb_asset_object_store) end.
descriptor() -> #{id => 3,object_hash => <<"hash">>,mime => <<"text/plain">>,size_bytes => 4,payload => <<"test">>,
    upload_subject => #{<<"kind">> => <<"seat">>,<<"principal">> => <<"7">>,<<"org_id">> => <<"1">>},
    signature_context => #{issued_at => {{2026,10,15},{1,0,0}},expires_at => {{2026,10,15},{1,5,0}}}}.
registered_put_reuses_reserved_row_and_context() ->
    D = descriptor(), Ctx = maps:get(signature_context,D),
    meck:expect(eb_pg_asset_meta,reserve_registered_put,fun(1,2,D0) ->
        ?assertEqual(D,D0),{ok,#{id => 3,object_key => <<"key">>,status => pending_confirm,mime => <<"text/plain">>,size_bytes => 4}} end),
    meck:expect(eb_asset_object_stub,put,fun(<<"key">>,<<"test">>,Meta) -> ?assertEqual(Ctx,maps:get(signature_context,Meta)),ok end),
    ?assertMatch({ok,#{asset_id := 3}},eb_asset_store:put_private(1,2,D)),
    ?assertEqual(0,meck:num_calls(eb_pg_asset_meta,insert_asset,'_')).
rejected_reservation_never_writes() ->
    meck:expect(eb_pg_asset_meta,reserve_registered_put,fun(_,_,_) -> {error,session_not_renewable} end),
    meck:expect(eb_asset_object_stub,put,fun(_,_,_) -> error(unexpected_provider_write) end),
    ?assertEqual({error,session_not_renewable},eb_asset_store:put_private(1,2,descriptor())),
    ?assertEqual(0,meck:num_calls(eb_asset_object_stub,put,'_')).
