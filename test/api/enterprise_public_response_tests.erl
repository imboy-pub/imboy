%%% @doc HTTP 响应边界回归：真实 JSON 编码与 socket，不连接数据库。
-module(enterprise_public_response_tests).

-include_lib("eunit/include/eunit.hrl").

-export([init/2, assert_accepted_response/1]).

accepted_json_test_() ->
    {timeout, 30, fun accepted_json/0}.

accepted_json() ->
    lists:foreach(
        fun(Publisher) ->
            assert_accepted_response(#{
                accepted => true,
                message_id => 9007199254740993,
                replayed => false,
                audit_id => undefined,
                publisher => Publisher,
                notification => undefined,
                internal_state => {not_public, hidden},
                message => #{
                    id => 9007199254740993,
                    conversation_id => 9007199254740994,
                    sender_type => contact,
                    sender_contact_id => 9007199254740995,
                    actor_user_id => undefined,
                    created_at => 1700000000,
                    retain_until => 1700003600,
                    body_cipher => <<"cipher-base64">>,
                    key_version => 1,
                    content_hash => <<"hash">>,
                    version => 1,
                    assets => [
                        #{
                            id => 9007199254740996,
                            mime => <<"image/png">>,
                            size_bytes => 12,
                            status => ready,
                            storage_ref => {private, hidden}
                        }
                    ],
                    internal_state => {not_public, hidden}
                }
            })
        end,
        [ok, {skipped, replay}, {failed, {publisher_down, <<"private-detail">>}}]
    ).

credential_response_test_() ->
    {timeout, 30, fun credential_response/0}.

credential_response() ->
    lists:foreach(
        fun({Action, DigestKey, Extra}) ->
            View = maps:merge(
                #{
                    id => 9007199254740993,
                    organization_id => 42,
                    workspace_id => 43,
                    secret => <<"synthetic-once-only">>,
                    DigestKey => <<"must-not-leak">>,
                    private_hash => <<"must-not-leak">>,
                    created_at => 1700000000,
                    updated_at => 1700000001,
                    version => 1,
                    revoked_at => undefined
                },
                Extra
            ),
            Payload = response(cs_http, #{action => Action}, View),
            ?assertEqual(<<"synthetic-once-only">>, maps:get(<<"secret">>, Payload)),
            ?assertEqual(<<"9007199254740993">>, maps:get(<<"id">>, Payload)),
            ?assertEqual(1700000000, maps:get(<<"created_at">>, Payload)),
            ?assertEqual(1700000001, maps:get(<<"updated_at">>, Payload)),
            ?assertEqual(1, maps:get(<<"version">>, Payload)),
            ?assertEqual(null, maps:get(<<"revoked_at">>, Payload)),
            ?assertNot(maps:is_key(atom_to_binary(DigestKey), Payload)),
            ?assertNot(maps:is_key(<<"private_hash">>, Payload))
        end,
        [
            {shop_key_list, key_digest, #{display_hint => undefined, status => active}},
            {visit_token_list, token_digest, #{contact_id => 44, expires_at => 1700003600}}
        ]
    ).

%% 供真 canonical/PG 用例调用，检查 application 的真实 accepted 结果。
assert_accepted_response(View) ->
    lists:foreach(
        fun(Module) ->
            Payload = response(Module, #{}, View),
            ?assertEqual(
                [
                    <<"accepted">>,
                    <<"audit_id">>,
                    <<"message">>,
                    <<"message_id">>,
                    <<"replayed">>
                ],
                lists:sort(maps:keys(Payload))
            ),
            ?assertEqual(true, maps:get(<<"accepted">>, Payload)),
            ?assertEqual(
                integer_to_binary(maps:get(message_id, View)),
                maps:get(<<"message_id">>, Payload)
            ),
            ?assertEqual(maps:get(replayed, View), maps:get(<<"replayed">>, Payload)),
            Audit = maps:get(audit_id, View),
            ?assertEqual(
                case Audit of
                    undefined -> null;
                    _ -> integer_to_binary(Audit)
                end,
                maps:get(<<"audit_id">>, Payload)
            ),
            Message = maps:get(message, View),
            PublicMessage = maps:get(<<"message">>, Payload),
            ?assertEqual(
                integer_to_binary(maps:get(id, Message)), maps:get(<<"id">>, PublicMessage)
            ),
            lists:foreach(
                fun(Key) ->
                    case maps:find(Key, Message) of
                        {ok, Value} ->
                            ?assertEqual(Value, maps:get(atom_to_binary(Key), PublicMessage));
                        error ->
                            ok
                    end
                end,
                [created_at, retain_until, body_cipher, key_version, content_hash, version]
            ),
            ?assertNot(maps:is_key(<<"internal_state">>, PublicMessage)),
            case maps:get(actor_user_id, Message, omitted) of
                undefined -> ?assertEqual(null, maps:get(<<"actor_user_id">>, PublicMessage));
                _ -> ok
            end,
            ?assertEqual(nomatch, binary:match(jsone:encode(Payload), <<"storage_ref">>))
        end,
        [eb_enterprise_http, cs_http]
    ).

response(Module, Entry, View) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, Name, Port} = eb_handler_test_support:listener(
        <<"/public-response">>,
        #{module => Module, entry => Entry, view => View},
        ?MODULE
    ),
    try
        Resp = eb_handler_test_support:request(Port, <<"GET">>, <<"/public-response">>),
        ?assertEqual(200, maps:get(status, Resp)),
        ?assertEqual(0, eb_handler_test_support:code(Resp)),
        eb_handler_test_support:payload(Resp)
    after
        eb_handler_test_support:stop(Name)
    end.

init(Req, #{module := Module, entry := Entry, view := View} = State) ->
    {ok, Module:respond(Entry, Req, {ok, View}), State}.
