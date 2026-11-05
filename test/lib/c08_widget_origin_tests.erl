-module(c08_widget_origin_tests).
-include_lib("eunit/include/eunit.hrl").
origins_are_independent_test() ->
    Base = application:get_env(imboy, base_url),
    Widget = application:get_env(imboy, widget_base_url),
    Edge = application:get_env(imboy, edge_base_url),
    Domain = application:get_env(imboy, widget_domain),
    try
        application:set_env(imboy, base_url, <<"https://api.example.org">>),
        application:set_env(imboy, widget_base_url, <<"https://cs.example.org">>),
        application:set_env(imboy, widget_domain, <<"cs.example.org">>),
        application:unset_env(imboy, edge_base_url),
        ?assertEqual(<<"https://cs.example.org">>, cs_widget_support:api_base()),
        ?assertEqual(<<"https://api.example.org/edge">>, attachment_edge_redirect:edge_base()),
        Ref = #{upload_ref => <<"test-ref">>},
        Req = #{
            bindings => #{org_id => <<"1">>},
            path => <<"/api/v1/seat/enterprise/organizations/1/assets/presign">>
        },
        {ok, #{upload := #{url := SeatUrl}}} = eb_enterprise_http:with_seat_upload_url(
            Req, 2, {ok, Ref}
        ),
        ?assertMatch(<<"/api/v1/seat/", _/binary>>, SeatUrl),
        ApiReq = Req#{path => <<"/api/v1/enterprise/organizations/1/assets/presign">>},
        {ok, #{upload := #{url := AbsoluteUrl}}} = eb_enterprise_http:with_seat_upload_url(
            ApiReq, 2, {ok, Ref}
        ),
        ?assertMatch(<<"https://api.example.org/api/v1/enterprise/", _/binary>>, AbsoluteUrl),
        lists:foreach(
            fun(Bad) ->
                application:set_env(imboy, widget_base_url, Bad),
                ?assertEqual(<<>>, cs_widget_support:api_base())
            end,
            [
                <<"https://">>,
                <<"https://cs.example.org/path">>,
                <<"https://cs.example.org?x=1">>,
                <<"https://x@cs.example.org">>,
                <<"https://other.example.org">>,
                <<"http://cs.example.org">>
            ]
        ),
        application:set_env(imboy, widget_domain, <<"localhost:8443">>),
        application:set_env(imboy, widget_base_url, <<"https://localhost:8443">>),
        ?assertEqual(<<"https://localhost:8443">>, cs_widget_support:api_base()),
        application:unset_env(imboy, widget_base_url),
        ?assertEqual(<<>>, cs_widget_support:api_base()),
        ?assertEqual({ok, Ref}, eb_enterprise_http:with_seat_upload_url(Req, 2, {ok, Ref})),
        {ok, #{upload := #{url := ApiUrl}}} = eb_enterprise_http:with_seat_upload_url(
            ApiReq, 2, {ok, Ref}
        ),
        ?assertMatch(<<"https://api.example.org/api/v1/enterprise/", _/binary>>, ApiUrl)
    after
        restore(base_url, Base),
        restore(widget_base_url, Widget),
        restore(edge_base_url, Edge),
        restore(widget_domain, Domain)
    end.
restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value);
restore(Key, undefined) -> application:unset_env(imboy, Key).
