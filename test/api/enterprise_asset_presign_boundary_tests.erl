%%% Handler boundary only: transaction/provider are doubles, not HTTP acceptance.
-module(enterprise_asset_presign_boundary_tests).
-include_lib("eunit/include/eunit.hrl").

postcommit_test_() ->
    {foreach,fun setup/0,fun cleanup/1,
        [fun absolute_gateway/0,fun missing_base/0,fun invalid_base/0,fun unknown_location/0,
         fun legacy_cached_url/0,fun transaction_failure/0]}.
setup() ->
    Modules=[cowboy_req,elib_param,elib_pg,enterprise_internal_idempotency,
        enterprise_internal_error,attachment_storage,imboy_jwt],
    [meck:new(M,[no_link])||M<-Modules],
    meck:expect(cowboy_req,method,fun(_)-><<"POST">> end),
    meck:expect(cowboy_req,header,fun(_,_) -> <<"idempotency-key">> end),
    meck:expect(cowboy_req,reply,fun(Status,Headers,Body,Req)->Req#{status=>Status,headers=>Headers,body=>Body} end),
    meck:expect(elib_param,post,fun(_) -> #{} end),
    meck:expect(enterprise_internal_idempotency,request_digest,fun(_,_,_)->{ok,<<"digest">>} end),
    meck:expect(enterprise_internal_idempotency,replay_header,fun()->{<<"replayed">>,<<"true">>} end),
    meck:expect(enterprise_internal_error,reply,fun(Req,Code)->Req#{error=>Code} end),
    meck:expect(attachment_storage,session_location,fun(_)->{ok,#{driver=>disk}} end),
    meck:expect(imboy_jwt,issue_upload_capability,fun(_,_,_)->{ok,<<"cap">>} end),
    application:set_env(imboy,base_url,<<"https://files.example.invalid/">>),
    Modules.
cleanup(Ms)->[meck:unload(M)||M<-Ms],application:unset_env(imboy,base_url).
reg()->#{<<"session_state">>=><<"registered">>,<<"client_upload_id">>=><<"00000000-0000-7000-8000-000000000001">>,
    <<"bucket">>=><<"fixture">>,<<"object_key">>=><<"fixture/key">>,<<"expires_at">>=>9999999999}.
replay(Body)->
    meck:expect(elib_pg,with_tx,fun(_)->{ok,replay,#{response_code=>200,response_body=>jsone:encode(Body)}} end),
    invoke().
invoke()->{ok,R,_}=enterprise_asset_handler:init(#{},#{action=>presign,enterprise_internal=>#{}}),R.
absolute_gateway()->
    R=replay(reg()), ?assertEqual(200,maps:get(status,R)),
    #{<<"put_url">>:=Url}=jsone:decode(maps:get(body,R)),
    ?assertMatch(<<"https://files.example.invalid/api/v1/attachment/gateway/",_/binary>>,Url),
    ?assertEqual(<<"no-store">>,maps:get(<<"cache-control">>,maps:get(headers,R))).
missing_base()->
    application:unset_env(imboy,base_url),
    ?assertEqual(<<"service_unavailable">>,maps:get(error,replay(reg()))),
    ?assertEqual(0,meck:num_calls(imboy_jwt,issue_upload_capability,'_')).
invalid_base()->
    lists:foreach(fun(Base) ->
        application:set_env(imboy,base_url,Base),
        ?assertEqual(<<"service_unavailable">>,maps:get(error,replay(reg())))
    end,[<<"files.internal">>,<<"/files">>,<<"ftp://files.invalid">>]),
    ?assertEqual(0,meck:num_calls(imboy_jwt,issue_upload_capability,'_')).
unknown_location()->
    meck:expect(attachment_storage,session_location,fun(_)->{ok,legacy_unknown} end),
    ?assertEqual(<<"conflict">>,maps:get(error,replay(reg()))),
    ?assertEqual(0,meck:num_calls(imboy_jwt,issue_upload_capability,'_')).
legacy_cached_url()->
    ?assertEqual(<<"conflict">>,maps:get(error,replay(#{<<"put_url">>=><<"https://old.invalid/unsafe">>}))),
    ?assertEqual(0,meck:num_calls(imboy_jwt,issue_upload_capability,'_')).
transaction_failure()->
    meck:expect(elib_pg,with_tx,fun(_)->{rollback,fixture_failure} end),
    ?assertEqual(<<"internal_error">>,maps:get(error,invoke())),
    ?assertEqual(0,meck:num_calls(imboy_jwt,issue_upload_capability,'_')).
