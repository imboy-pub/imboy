-module(api_v1_conversation_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Kept local (not via error_code.hrl) so the suite builds under
%% TEST_ERLC_OPTS without project include paths; values are the production
%% ERR_OPERATION_FAILED / ERR_TOKEN_MISSING.
-define(ERR_OPERATION_FAILED, 500).
-define(ERR_TOKEN_MISSING, 401).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    conv_001_mine_happy/1,
    conv_002_pin_unpin_cycle/1,
    conv_003_pin_idempotent/1,
    conv_004_mine_missing_token/1,
    conv_005_pin_invalid_conversation_id/1,
    conv_006_pin_nonexistent_conversation/1
]).

-define(MINE_PATH, <<"/api/v1/conversation/mine">>).
-define(PIN_PATH, <<"/api/v1/conversation/pin">>).
-define(UNPIN_PATH, <<"/api/v1/conversation/unpin">>).

all() ->
    [
        conv_001_mine_happy,
        conv_002_pin_unpin_cycle,
        conv_003_pin_idempotent,
        conv_004_mine_missing_token,
        conv_005_pin_invalid_conversation_id,
        conv_006_pin_nonexistent_conversation
    ].

%% Real application through the project's standard CT entry: config load,
%% core dependency apps and the serialized boot (migrations run against the
%% scratch database during that start; Ranch binds an ephemeral port).
%% Device-sign stays on (production default) because every /api/v1 domain
%% route is sign-gated before the JWT condition.
init_per_suite(Config0) ->
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    Port = ranch:get_port(imboy_listener),
    SignKey = rest_fixture:ensure_sign_key(),

    Suffix = rest_fixture:unique_id(<<"conv">>),
    UserA = rest_fixture:login(
        rest_fixture:create_user(#{
            account => <<"rest-conva-", Suffix/binary>>,
            email => <<"rest-conva-", Suffix/binary, "@example.invalid">>,
            nickname => <<"REST Conv A ", Suffix/binary>>
        }),
        SignKey
    ),
    %% The login path writes the user_device row through gen_server:cast
    %% after the HTTP answer; the JWT gate rejects tokens until the row is
    %% active, so every fixture login is followed by the shared wait.
    ok = rest_fixture:await_device_active(uid(UserA), maps:get(did, UserA)),
    UserB = rest_fixture:login(
        rest_fixture:create_user(#{
            account => <<"rest-convb-", Suffix/binary>>,
            email => <<"rest-convb-", Suffix/binary, "@example.invalid">>,
            nickname => <<"REST Conv B ", Suffix/binary>>
        }),
        SignKey
    ),
    ok = rest_fixture:await_device_active(uid(UserB), maps:get(did, UserB)),
    [
        {http_port, Port},
        {user_a, UserA},
        {user_b, UserB},
        {sign_key, SignKey}
        | Config
    ].

end_per_suite(Config) ->
    ct:log("conversation suite done"),
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% Cases
%% ===================================================================

%% CONV-001: GET /api/v1/conversation/mine returns the A/B c2c conversation
%% after one real c2c message row is seeded through msg_c2c_ds:write_msg/6.
conv_001_mine_happy(Config) ->
    UserA = ?config(user_a, Config),
    UserB = ?config(user_b, Config),
    AUid = uid(UserA),
    BUid = uid(UserB),
    {MsgId, _PayloadMap} = seed_c2c_msg(AUid, BUid),

    Response = get(Config, UserA, ?MINE_PATH),
    verify(
        <<"CONV-001">>,
        <<"GET">>,
        ?MINE_PATH,
        #{},
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0}, Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"list">>],
                fun(List) -> is_list(List) end,
                Resp
            ),
            %% The seeded A->B message must surface as a c2c conversation
            %% entry carrying the conversation fields and pin state.
            rest_assert:predicate(
                [<<"payload">>, <<"list">>],
                fun(List) ->
                    lists:any(
                        fun(Entry) ->
                            maps:get(<<"conversation_id">>, Entry, none) =:= BUid andalso
                                maps:get(<<"conversation_type">>, Entry, none) =:= <<"c2c">> andalso
                                maps:get(<<"last_msg_id">>, Entry, none) =:= MsgId andalso
                                is_integer(maps:get(<<"server_ts">>, Entry, none)) andalso
                                is_boolean(maps:get(<<"is_pinned">>, Entry, none))
                        end,
                        List
                    )
                end,
                Resp
            ),
            %% The mirrored entry must be visible from B as well.
            ResponseB = get(Config, UserB, ?MINE_PATH),
            rest_assert:status(200, ResponseB),
            rest_assert:predicate(
                [<<"payload">>, <<"list">>],
                fun(List) ->
                    lists:any(
                        fun(Entry) ->
                            maps:get(<<"conversation_id">>, Entry, none) =:= AUid andalso
                                maps:get(<<"conversation_type">>, Entry, none) =:= <<"c2c">>
                        end,
                        List
                    )
                end,
                ResponseB
            )
        end
    ).

%% CONV-002: pin then unpin the A/B conversation; mine flips is_pinned.
conv_002_pin_unpin_cycle(Config) ->
    UserA = ?config(user_a, Config),
    UserB = ?config(user_b, Config),
    AUid = uid(UserA),
    BUid = uid(UserB),
    {_MsgId, _} = seed_c2c_msg(AUid, BUid),

    PinBody = pin_body(BUid),
    PinResponse = post(Config, UserA, ?PIN_PATH, PinBody),
    MineAfterPin = get(Config, UserA, ?MINE_PATH),
    UnpinResponse = post(Config, UserA, ?UNPIN_PATH, PinBody),
    MineAfterUnpin = get(Config, UserA, ?MINE_PATH),

    verify(
        <<"CONV-002">>,
        <<"POST">>,
        ?PIN_PATH,
        PinBody,
        PinResponse,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Resp),
            %% After pin the conversation is flagged in mine.
            rest_assert:status(200, MineAfterPin),
            pinned_entry_is(MineAfterPin, BUid, true),
            %% Unpin answers with the explicit updated payload.
            rest_assert:status(200, UnpinResponse),
            rest_assert:json_contains(#{<<"code">> => 0, <<"updated">> => true}, UnpinResponse),
            %% And the pin state is cleared again.
            rest_assert:status(200, MineAfterUnpin),
            pinned_entry_is(MineAfterUnpin, BUid, false)
        end
    ).

%% CONV-003: a repeated pin stays successful (already-pinned fast path).
conv_003_pin_idempotent(Config) ->
    UserA = ?config(user_a, Config),
    UserB = ?config(user_b, Config),
    Body = pin_body(uid(UserB)),

    First = post(Config, UserA, ?PIN_PATH, Body),
    Second = post(Config, UserA, ?PIN_PATH, Body),
    verify(
        <<"CONV-003">>,
        <<"POST">>,
        ?PIN_PATH,
        Body,
        First,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Resp),
            rest_assert:status(200, Second),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Second)
        end
    ).

%% CONV-004: signed request without Authorization is stopped at the
%% middleware boundary with the real HTTP 401 + ERR_TOKEN_MISSING envelope.
conv_004_mine_missing_token(Config) ->
    SignKey = ?config(sign_key, Config),
    Did = case_did(<<"004">>),
    Headers = rest_fixture:signed_headers(Did, SignKey),
    Response = rest_client:request(
        ?config(http_port, Config), <<"GET">>, ?MINE_PATH, <<>>, Headers
    ),
    verify(
        <<"CONV-004">>,
        <<"GET">>,
        ?MINE_PATH,
        <<>>,
        Response,
        #{
            <<"http_status">> => 401,
            <<"code">> => ?ERR_TOKEN_MISSING,
            <<"msg">> => <<"未登录，请先登录"/utf8>>
        },
        fun(Resp) ->
            rest_assert:status(401, Resp),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_TOKEN_MISSING, <<"msg">> => <<"未登录，请先登录"/utf8>>},
                Resp
            )
        end
    ).

%% CONV-005: non-integer and <= 0 conversation ids hit the logic guard and
%% are surfaced as the handler's operation-failed envelope (code 500).
conv_005_pin_invalid_conversation_id(Config) ->
    UserA = ?config(user_a, Config),
    NotNumberBody = #{<<"conversation_id">> => <<"not-a-number">>, <<"type">> => <<"c2c">>},
    ZeroBody = #{<<"conversation_id">> => 0, <<"type">> => <<"c2c">>},

    NotNumber = post(Config, UserA, ?PIN_PATH, NotNumberBody),
    Zero = post(Config, UserA, ?PIN_PATH, ZeroBody),
    verify(
        <<"CONV-005">>,
        <<"POST">>,
        ?PIN_PATH,
        NotNumberBody,
        NotNumber,
        #{
            <<"http_status">> => 200,
            <<"code">> => ?ERR_OPERATION_FAILED,
            <<"msg">> => <<"会话ID无效"/utf8>>
        },
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_OPERATION_FAILED, <<"msg">> => <<"会话ID无效"/utf8>>}, Resp
            ),
            rest_assert:status(200, Zero),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_OPERATION_FAILED, <<"msg">> => <<"会话ID无效"/utf8>>}, Zero
            )
        end
    ).

%% CONV-006: pin does not verify conversation existence — any positive
%% integer id is persisted and answered with code 0 (documented server
%% fact; there is no Not Found branch on this endpoint), and unpin of the
%% same id answers {updated: true}.
conv_006_pin_nonexistent_conversation(Config) ->
    UserA = ?config(user_a, Config),
    PhantomId = phantom_conversation_id(),
    Body = #{<<"conversation_id">> => integer_to_binary(PhantomId), <<"type">> => <<"c2c">>},

    PinResponse = post(Config, UserA, ?PIN_PATH, Body),
    UnpinResponse = post(Config, UserA, ?UNPIN_PATH, Body),
    verify(
        <<"CONV-006">>,
        <<"POST">>,
        ?PIN_PATH,
        Body,
        PinResponse,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Resp),
            rest_assert:status(200, UnpinResponse),
            rest_assert:json_contains(#{<<"code">> => 0, <<"updated">> => true}, UnpinResponse)
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

verify(CaseId, Method, Path, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_conversation">>,
        method => Method,
        path => Path,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

%% Assert that the mine list contains the B conversation with the expected
%% is_pinned value.
pinned_entry_is(Response, PeerUid, ExpectedPinned) ->
    rest_assert:predicate(
        [<<"payload">>, <<"list">>],
        fun(List) ->
            lists:any(
                fun(Entry) ->
                    maps:get(<<"conversation_id">>, Entry, none) =:= PeerUid andalso
                        maps:get(<<"conversation_type">>, Entry, none) =:= <<"c2c">> andalso
                        maps:get(<<"is_pinned">>, Entry, none) =:= ExpectedPinned
                end,
                List
            )
        end,
        Response
    ).

%% conversation_id travels as a TSID string per the JSON contract.
pin_body(PeerUid) ->
    #{<<"conversation_id">> => integer_to_binary(PeerUid), <<"type">> => <<"c2c">>}.

%% Seed one real c2c message row through the production write path.
%% Plain text payload, no e2ee key: no real encrypted traffic is produced.
seed_c2c_msg(FromUid, ToUid) ->
    MsgId = rest_fixture:unique_id(<<"msg">>),
    PayloadMap = #{
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"rest-conversation-seed ", MsgId/binary>>
    },
    Now = erlang:system_time(millisecond),
    %% The production repo answers the plain atom ok on success
    %% (msg_c2c_repo: {ok, Count} when Count > 0 -> ok).
    ok = msg_c2c_ds:write_msg(Now, MsgId, PayloadMap, FromUid, ToUid, Now),
    {MsgId, PayloadMap}.

uid(User) ->
    maps:get(uid, User).

get(Config, User, Path) ->
    rest_client:request(
        ?config(http_port, Config),
        <<"GET">>,
        Path,
        <<>>,
        headers(Config, User)
    ).

post(Config, User, Path, Body) ->
    rest_client:post(?config(http_port, Config), Path, Body, headers(Config, User)).

headers(Config, User) ->
    Did = rest_fixture:unique_id(<<"d">>),
    maps:merge(
        rest_fixture:auth_header(User),
        rest_fixture:signed_headers(Did, ?config(sign_key, Config))
    ).

case_did(Tag) ->
    <<"rest-conv-", Tag/binary, "-", (rest_fixture:unique_id(<<"d">>))/binary>>.

%% A positive integer in the TSID range that no fixture ever created.
phantom_conversation_id() ->
    900000000000000000 + rand:uniform(99999999).
