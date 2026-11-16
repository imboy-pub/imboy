-module(cors_manifest_preflight_tests).
-include_lib("eunit/include/eunit.hrl").

manifest_preflight_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
        [fun allowed_web_headers/0, fun rejected_origin/0, fun missing_origin/0]}.
setup() ->
    meck:new(cowboy_req,[no_link,non_strict]),
    meck:new(config_ds,[no_link,non_strict]),
    meck:expect(config_ds,env,fun
        (cors_allowed_origins,_) -> [<<"https://web.example.test">>];
        (cors_allow_localhost,_) -> false;
        (_,Default) -> Default
    end),
    meck:expect(cowboy_req,method,fun(R)->maps:get(method,R) end),
    meck:expect(cowboy_req,path,fun(R)->maps:get(path,R) end),
    meck:expect(cowboy_req,header,fun(K,R)->maps:get(K,maps:get(headers,R,#{}),undefined) end),
    meck:expect(cowboy_req,set_resp_header,fun(K,V,R)->R#{resp_headers => (maps:get(resp_headers,R,#{}))#{K=>V}} end),
    meck:expect(cowboy_req,reply,fun(Status,R)->R#{status=>Status} end),
    meck:expect(cowboy_req,reply,fun(Status,Headers,Body,R)->R#{status=>Status,reply_headers=>Headers,body=>Body} end), ok.
cleanup(_)-> meck:unload([cowboy_req,config_ds]).
request(Origin) ->
    #{method => <<"OPTIONS">>, path => <<"/api/v1/app/manifest">>,
      headers => #{<<"origin">> => Origin, <<"access-control-request-method">> => <<"GET">>,
                   <<"access-control-request-headers">> => <<"accept,authorization,content-type,did,dname,pkg,terminology-profile,tz_offset,vsn">>}}.

allowed_web_headers() ->
    {stop, #{status:=204,resp_headers:=H}} = cors_middleware:execute(request(<<"https://web.example.test">>),#{}),
    ?assertEqual(<<"https://web.example.test">>,maps:get(<<"access-control-allow-origin">>,H)),
    ?assertEqual(<<"Origin">>,maps:get(<<"vary">>,H)),
    ?assertEqual(<<"true">>,maps:get(<<"access-control-allow-credentials">>,H)),
    Raw = maps:get(<<"access-control-allow-headers">>,H),
    Allowed = [string:lowercase(string:trim(V)) || V <- binary:split(Raw,<<",">>,[global])],
    lists:foreach(fun(V)-> ?assert(lists:member(V,Allowed)) end,
       [<<"accept">>,<<"authorization">>,<<"content-type">>,<<"did">>,<<"dname">>,<<"pkg">>,<<"terminology-profile">>,<<"tz_offset">>,<<"vsn">>,<<"imboy-refreshtoken">>]),
    ?assertNot(lists:member(<<"*">>,Allowed)).
rejected_origin() ->
    lists:foreach(fun(Origin)->
        {stop,#{status:=403,resp_headers:=H}} = cors_middleware:execute(request(Origin),#{}),
        ?assertNot(maps:is_key(<<"access-control-allow-origin">>,H))
    end,[<<"https://evil.example.test">>,<<"https://web.example.test.evil">>,<<"null">>,<<"http://localhost:9191">>]).
missing_origin() ->
    %% Existing same-origin/non-browser OPTIONS behavior remains unchanged.
    {stop,#{status:=204,resp_headers:=H}} = cors_middleware:execute(request(undefined),#{}),
    ?assertNot(maps:is_key(<<"access-control-allow-origin">>,H)).
