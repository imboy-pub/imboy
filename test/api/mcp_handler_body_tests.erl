-module(mcp_handler_body_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MAX_BODY, 1048576).

body_limit_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(chunked_boundary_accepted()) end,
        fun(_) -> ?_test(chunked_final_overflow_rejected()) end,
        fun(_) -> ?_test(chunked_more_overflow_stops_reading()) end,
        fun(_) -> ?_test(single_chunk_overflow_rejected()) end,
        fun(_) -> ?_test(valid_notification_accepted()) end
    ]}.

setup() ->
    ok = meck:new(cowboy_req, [non_strict, no_link]),
    ok = meck:new(mcp_governance_logic, [non_strict, no_link]),
    ok = meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
    ok = meck:expect(cowboy_req, header, fun
        (<<"authorization">>, _) -> <<"Bearer test-secret">>;
        (<<"mcp-session-id">>, _) -> undefined
    end),
    ok = meck:expect(mcp_governance_logic, authenticate_secret, fun(<<"test-secret">>) ->
        {ok, #{owner_uid => 42, client_id => 7, client_key => <<"test-client">>}}
    end),
    ok = meck:expect(cowboy_req, read_body, fun(
        #{chunks := [{Status, Data} | Rest], reads := Reads} = Req, #{length := Length}
    ) ->
        ?assert(Length > 0),
        ?assert(Length =< ?MAX_BODY + 1),
        {Status, Data, Req#{chunks := Rest, reads := Reads + 1}}
    end),
    ok = meck:expect(cowboy_req, reply, fun(Code, Headers, Body, Req) ->
        Req#{response_status => Code, response_headers => Headers, response_body => Body}
    end).

cleanup(_) ->
    meck:unload(cowboy_req),
    meck:unload(mcp_governance_logic).

notification() ->
    <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}">>.

boundary_body() ->
    Body = notification(),
    Padding = binary:copy(<<" ">>, ?MAX_BODY - byte_size(Body)),
    <<Body/binary, Padding/binary>>.

request(Chunks) ->
    {ok, Req, #{}} = mcp_handler:init(#{chunks => Chunks, reads => 0}, #{}),
    Req.

chunked_boundary_accepted() ->
    <<First:524288/binary, Last/binary>> = boundary_body(),
    Req = request([{more, First}, {more, Last}, {ok, <<>>}]),
    ?assertEqual(202, maps:get(response_status, Req)),
    ?assertEqual(<<>>, maps:get(response_body, Req)),
    ?assertEqual(3, maps:get(reads, Req)).

chunked_final_overflow_rejected() ->
    <<First:524288/binary, Last/binary>> = boundary_body(),
    assert_too_large(request([{more, First}, {ok, <<Last/binary, " ">>}]), 2).

chunked_more_overflow_stops_reading() ->
    Req = request([{more, boundary_body()}, {more, <<" ">>}, {ok, <<"unread">>}]),
    assert_too_large(Req, 2),
    ?assertEqual([{ok, <<"unread">>}], maps:get(chunks, Req)).

single_chunk_overflow_rejected() ->
    Body = boundary_body(),
    assert_too_large(request([{ok, <<Body/binary, " ">>}]), 1).

assert_too_large(Req, Reads) ->
    ?assertEqual(413, maps:get(response_status, Req)),
    ?assertEqual(#{<<"error">> => <<"body_too_large">>}, json:decode(maps:get(response_body, Req))),
    ?assertEqual(Reads, maps:get(reads, Req)),
    ?assertNot(meck:called(cowboy_req, header, [<<"mcp-session-id">>, '_'])).

valid_notification_accepted() ->
    Req = request([{ok, notification()}]),
    ?assertEqual(202, maps:get(response_status, Req)),
    ?assertEqual(<<>>, maps:get(response_body, Req)).
