-module(api_v1_channel_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Kept local (not via error_code.hrl) so the suite builds under
%% TEST_ERLC_OPTS without project include paths; the value is the
%% production ERR_TOKEN_MISSING.
-define(ERR_TOKEN_MISSING, 401).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    channel_001_create_happy/1,
    channel_002_show_by_dynamic_id/1,
    channel_003_subscribe_idempotent/1,
    channel_004_messages_empty/1,
    channel_005_nonexistent_channel/1,
    channel_006_create_missing_token/1,
    channel_007_create_empty_name/1
]).

-define(CREATE_PATH, <<"/api/v1/channel/create">>).

all() ->
    [
        channel_001_create_happy,
        channel_002_show_by_dynamic_id,
        channel_003_subscribe_idempotent,
        channel_004_messages_empty,
        channel_005_nonexistent_channel,
        channel_006_create_missing_token,
        channel_007_create_empty_name
    ].

%% Real application through the project's standard CT entry (config load,
%% core dependency apps, serialized boot with scratch-database migrations).
init_per_suite(Config0) ->
    ok = rest_fixture:ensure_ct_priv_alias(),
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    Port = ranch:get_port(imboy_listener),
    SignKey = rest_fixture:ensure_sign_key(),

    Suffix = rest_fixture:unique_id(<<"chan">>),
    UserA = rest_fixture:login(
        rest_fixture:create_user(#{
            account => <<"rest-chana-", Suffix/binary>>,
            email => <<"rest-chana-", Suffix/binary, "@example.invalid">>,
            nickname => <<"REST Chan A ", Suffix/binary>>
        }),
        SignKey
    ),
    %% The login path writes the user_device row through gen_server:cast
    %% after the HTTP answer; the JWT gate rejects tokens until the row is
    %% active, so every fixture login is followed by the shared wait.
    ok = rest_fixture:await_device_active(uid(UserA), maps:get(did, UserA)),
    [
        {http_port, Port},
        {user_a, UserA},
        {sign_key, SignKey}
        | Config
    ].

end_per_suite(Config) ->
    ct:log("channel suite done"),
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% Cases
%% ===================================================================

%% CHANNEL-001: POST /api/v1/channel/create returns the safe-column channel
%% payload with creator and default access policy fields.
channel_001_create_happy(Config) ->
    UserA = ?config(user_a, Config),
    AUid = uid(UserA),
    Name = channel_name(<<"001">>),
    Body = #{<<"name">> => Name, <<"description">> => <<"REST channel happy path">>},

    Response = post(Config, UserA, ?CREATE_PATH, Body),
    verify(
        <<"CHANNEL-001">>,
        <<"POST">>,
        ?CREATE_PATH,
        Body,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0}, Resp),
            rest_assert:json_path([<<"payload">>, <<"name">>], Name, Resp),
            rest_assert:json_path(
                [<<"payload">>, <<"description">>], <<"REST channel happy path">>, Resp
            ),
            rest_assert:json_path([<<"payload">>, <<"creator_uid">>], AUid, Resp),
            rest_assert:json_path([<<"payload">>, <<"visibility">>], 0, Resp),
            rest_assert:json_path([<<"payload">>, <<"access_type">>], 0, Resp),
            rest_assert:json_path([<<"payload">>, <<"join_policy">>], 0, Resp),
            rest_assert:json_path([<<"payload">>, <<"is_verified">>], false, Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"id">>], fun positive_id/1, Resp
            )
        end
    ).

%% CHANNEL-002: GET /api/v1/channel/:channel_id with the id taken from the
%% create response; the creator sees role 3 and the creator-is-subscribed flag.
channel_002_show_by_dynamic_id(Config) ->
    UserA = ?config(user_a, Config),
    Name = channel_name(<<"002">>),
    ChannelId = create_channel(Config, UserA, Name),
    Path = channel_path(ChannelId),

    Response = get(Config, UserA, Path),
    verify(
        <<"CHANNEL-002">>,
        <<"GET">>,
        Path,
        #{},
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_path([<<"payload">>, <<"id">>], ChannelId, Resp),
            rest_assert:json_path([<<"payload">>, <<"name">>], Name, Resp),
            rest_assert:json_path([<<"payload">>, <<"user_role">>], 3, Resp),
            rest_assert:json_path([<<"payload">>, <<"is_subscribed">>], true, Resp),
            rest_assert:json_path([<<"payload">>, <<"has_purchased">>], false, Resp)
        end
    ).

%% CHANNEL-003: subscribe on the default open join_policy is idempotent
%% (upsert-active) and answers with an empty success payload.
channel_003_subscribe_idempotent(Config) ->
    UserA = ?config(user_a, Config),
    ChannelId = create_channel(Config, UserA, channel_name(<<"003">>)),
    Path = subscribe_path(ChannelId),

    First = post(Config, UserA, Path, #{}),
    Second = post(Config, UserA, Path, #{}),
    verify(
        <<"CHANNEL-003">>,
        <<"POST">>,
        Path,
        #{},
        First,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Resp),
            rest_assert:status(200, Second),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Second)
        end
    ).

%% CHANNEL-004: messages of a fresh channel come back as an empty list,
%% including the limit=1 boundary request.
channel_004_messages_empty(Config) ->
    UserA = ?config(user_a, Config),
    ChannelId = create_channel(Config, UserA, channel_name(<<"004">>)),
    Path = <<(channel_path(ChannelId))/binary, "/messages">>,
    LimitedPath = <<Path/binary, "?limit=1">>,

    Response = get(Config, UserA, Path),
    Limited = get(Config, UserA, LimitedPath),
    verify(
        <<"CHANNEL-004">>,
        <<"GET">>,
        Path,
        #{},
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0, <<"list">> => []},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"payload">> => #{<<"list">> => []}}, Resp
            ),
            rest_assert:status(200, Limited),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"payload">> => #{<<"list">> => []}}, Limited
            )
        end
    ).

%% CHANNEL-005: show/subscribe/messages on an unknown positive id collapse
%% into the real business error envelope (code 1, 频道不存在) — the domain
%% has no HTTP 404 / envelope 404 branch.
channel_005_nonexistent_channel(Config) ->
    UserA = ?config(user_a, Config),
    PhantomId = phantom_channel_id(),
    ShowPath = channel_path(PhantomId),

    Show = get(Config, UserA, ShowPath),
    Subscribe = post(Config, UserA, subscribe_path(PhantomId), #{}),
    Messages = get(Config, UserA, <<(channel_path(PhantomId))/binary, "/messages">>),
    verify(
        <<"CHANNEL-005">>,
        <<"GET">>,
        ShowPath,
        #{},
        Show,
        #{<<"http_status">> => 200, <<"code">> => 1, <<"msg">> => <<"频道不存在"/utf8>>},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"频道不存在"/utf8>>}, Resp
            ),
            rest_assert:status(200, Subscribe),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"频道不存在"/utf8>>}, Subscribe
            ),
            rest_assert:status(200, Messages),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"频道不存在"/utf8>>}, Messages
            )
        end
    ).

%% CHANNEL-006: signed create without Authorization is stopped at the
%% middleware boundary with HTTP 401 / ERR_TOKEN_MISSING.
channel_006_create_missing_token(Config) ->
    SignKey = ?config(sign_key, Config),
    Did = rest_fixture:unique_id(<<"d">>),
    Headers = rest_fixture:signed_headers(Did, SignKey),
    Body = #{<<"name">> => channel_name(<<"006">>)},
    Response = rest_client:post(?config(http_port, Config), ?CREATE_PATH, Body, Headers),
    verify(
        <<"CHANNEL-006">>,
        <<"POST">>,
        ?CREATE_PATH,
        Body,
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

%% CHANNEL-007: an empty name is rejected with the real validation message.
channel_007_create_empty_name(Config) ->
    UserA = ?config(user_a, Config),
    Body = #{<<"name">> => <<>>},
    Response = post(Config, UserA, ?CREATE_PATH, Body),
    verify(
        <<"CHANNEL-007">>,
        <<"POST">>,
        ?CREATE_PATH,
        Body,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1, <<"msg">> => <<"频道名称不能为空"/utf8>>},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"频道名称不能为空"/utf8>>}, Resp
            )
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

verify(CaseId, Method, Path, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_channel">>,
        method => Method,
        path => Path,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

positive_id(Value) when is_integer(Value) ->
    Value > 0;
positive_id(Value) when is_binary(Value) ->
    byte_size(Value) > 0;
positive_id(_) ->
    false.

%% Create a channel through the real REST endpoint and return its id.
create_channel(Config, User, Name) ->
    #{body := #{<<"payload">> := #{<<"id">> := ChannelId}}} =
        post(Config, User, ?CREATE_PATH, #{<<"name">> => Name, <<"description">> => Name}),
    true = positive_id(ChannelId) orelse erlang:error({channel_create_failed, Name}),
    ChannelId.

channel_name(Tag) ->
    <<"rest-channel-", Tag/binary, "-", (rest_fixture:unique_id(<<"c">>))/binary>>.

%% Dynamic path ids always come from fixture return values.
channel_path(ChannelId) when is_integer(ChannelId) ->
    <<"/api/v1/channel/", (integer_to_binary(ChannelId))/binary>>;
channel_path(ChannelId) when is_binary(ChannelId) ->
    <<"/api/v1/channel/", ChannelId/binary>>.

subscribe_path(ChannelId) ->
    <<(channel_path(ChannelId))/binary, "/subscribe">>.

%% A positive integer no fixture ever created.
phantom_channel_id() ->
    900000000000000000 + rand:uniform(99999999).

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
