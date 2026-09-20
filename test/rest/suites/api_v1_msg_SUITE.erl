-module(api_v1_msg_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Kept local (not via error_code.hrl) so the suite builds under
%% TEST_ERLC_OPTS without project include paths; values are the production
%% ERR_BAD_REQUEST / ERR_TOKEN_MISSING / ERR_MESSAGE_NOT_FOUND.
-define(ERR_BAD_REQUEST, 400).
-define(ERR_TOKEN_MISSING, 401).
-define(ERR_MESSAGE_NOT_FOUND, 404).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    msg_001_history_happy/1,
    msg_002_history_cursor_pagination/1,
    msg_003_history_param_validation/1,
    msg_004_reaction_add_remove_cycle/1,
    msg_005_reaction_message_not_found/1,
    msg_006_reaction_param_validation/1,
    msg_007_history_missing_token/1
]).

-define(HISTORY_PATH, <<"/api/v1/msg/history">>).
-define(REACTION_ADD_PATH, <<"/api/v1/msg/reaction/add">>).
-define(REACTION_REMOVE_PATH, <<"/api/v1/msg/reaction/remove">>).

all() ->
    [
        msg_001_history_happy,
        msg_002_history_cursor_pagination,
        msg_003_history_param_validation,
        msg_004_reaction_add_remove_cycle,
        msg_005_reaction_message_not_found,
        msg_006_reaction_param_validation,
        msg_007_history_missing_token
    ].

%% Real application through the project's standard CT entry (config load,
%% core dependency apps, serialized boot with scratch-database migrations).
%% msg_archive_enabled is injected explicitly below (same style as
%% api_auth_switch): msg_store-backed history is required by MSG-001/002
%% and must not depend on the ambient test config carrying the flag.
init_per_suite(Config0) ->
    ok = rest_fixture:ensure_ct_priv_alias(),
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    %% The archive switch is read per call through application:get_env
    %% (msg_store_worker:maybe_archive/1), so a post-boot injection is
    %% effective without a restart.
    ok = application:set_env(imboy, msg_archive_enabled, true),
    Port = ranch:get_port(imboy_listener),
    SignKey = rest_fixture:ensure_sign_key(),

    Suffix = rest_fixture:unique_id(<<"msg">>),
    UserA = rest_fixture:login(
        rest_fixture:create_user(#{
            account => <<"rest-msga-", Suffix/binary>>,
            email => <<"rest-msga-", Suffix/binary, "@example.invalid">>,
            nickname => <<"REST Msg A ", Suffix/binary>>
        }),
        SignKey
    ),
    %% The login path writes the user_device row through gen_server:cast
    %% after the HTTP answer; the JWT gate rejects tokens until the row is
    %% active, so every fixture login is followed by the shared wait.
    ok = rest_fixture:await_device_active(uid(UserA), maps:get(did, UserA)),
    UserB = rest_fixture:login(
        rest_fixture:create_user(#{
            account => <<"rest-msgb-", Suffix/binary>>,
            email => <<"rest-msgb-", Suffix/binary, "@example.invalid">>,
            nickname => <<"REST Msg B ", Suffix/binary>>
        }),
        SignKey
    ),
    ok = rest_fixture:await_device_active(uid(UserB), maps:get(did, UserB)),
    %% Common Test writes init_per_suite's return value into every suite
    %% log page (review P1): full login maps carry password/token/
    %% refreshtoken/authorization, so the config carries sanitized handles
    %% and auth_header/1 rehydrates through the fixture session store.
    ok = rest_fixture:store_session(user_a, UserA),
    ok = rest_fixture:store_session(user_b, UserB),
    [
        {http_port, Port},
        {user_a, rest_fixture:sanitize_user(UserA, user_a)},
        {user_b, rest_fixture:sanitize_user(UserB, user_b)}
        | Config
    ].

end_per_suite(Config) ->
    ct:log("msg suite done"),
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% Cases
%% ===================================================================

%% MSG-001: GET /api/v1/msg/history returns the three seeded archive rows
%% with conv_key / cursor fields intact.
msg_001_history_happy(Config) ->
    UserA = ?config(user_a, Config),
    UserB = ?config(user_b, Config),
    AUid = uid(UserA),
    BUid = uid(UserB),
    MsgIds = seed_archive_msgs(AUid, BUid, 3),

    Path = history_path(BUid, 0, 50),
    Response = get(Config, UserA, Path),
    verify(
        <<"MSG-001">>,
        <<"GET">>,
        Path,
        <<>>,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0}, Resp),
            rest_assert:json_path([<<"payload">>, <<"conv_key">>], conv_key(AUid, BUid), Resp),
            rest_assert:json_path([<<"payload">>, <<"has_more">>], false, Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"next_seq">>], fun is_integer/1, Resp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"messages">>],
                fun(Messages) -> is_list(Messages) andalso length(Messages) =:= 3 end,
                Resp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"messages">>],
                fun(Messages) ->
                    lists:all(
                        fun(M) ->
                            is_integer(maps:get(<<"conv_seq">>, M, none)) andalso
                                maps:get(<<"conv_seq">>, M, none) > 0 andalso
                                maps:get(<<"chat_type">>, M, none) =:= <<"c2c">> andalso
                                lists:member(maps:get(<<"msg_id">>, M, none), MsgIds) andalso
                                is_map(maps:get(<<"payload">>, M, none))
                        end,
                        Messages
                    )
                end,
                Resp
            )
        end
    ).

%% MSG-002: limit clamps the page and the returned next_seq cursor pages
%% through the remaining rows without overlap.
msg_002_history_cursor_pagination(Config) ->
    %% MSG-001 already archived rows into the suite-level A/B conversation
    %% (c2c:<min>:<max> is per-pair, direction-agnostic, and nothing cleans
    %% it between cases), so reusing that pair here would make the cursor
    %% assertions depend on case order: 6 accumulated rows -> page 2 returns
    %% 4 messages after after_seq=2 (probe2 MSG-002 failure). This case
    %% therefore logs in a dedicated peer, so the conversation holds only
    %% its own three seeds and the pagination contract is asserted in
    %% isolation against the real product semantics (messaging_logic:history/6
    %% fetches Limit+1 rows and sublist/2-clamps to Limit; limit IS honoured).
    SignKey = rest_fixture:sign_key(),
    Peer = rest_fixture:login(rest_fixture:create_user(#{}), SignKey),
    ok = rest_fixture:await_device_active(uid(Peer), maps:get(did, Peer)),
    UserA = ?config(user_a, Config),
    AUid = uid(UserA),
    BUid = uid(Peer),
    MsgIds = seed_archive_msgs(AUid, BUid, 3),

    Page1Path = history_path(BUid, 0, 2),
    Page1 = get(Config, UserA, Page1Path),
    NextSeq = body_path(body(Page1), [<<"payload">>, <<"next_seq">>]),
    true = is_integer(NextSeq),
    Page2Path = history_path(BUid, NextSeq, 50),
    Page2 = get(Config, UserA, Page2Path),

    verify(
        <<"MSG-002">>,
        <<"GET">>,
        Page1Path,
        <<>>,
        Page1,
        #{<<"http_status">> => 200, <<"code">> => 0, <<"messages_on_page">> => 2},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"messages">>],
                fun(Messages) -> is_list(Messages) andalso length(Messages) =:= 2 end,
                Resp
            ),
            rest_assert:json_path([<<"payload">>, <<"has_more">>], true, Resp),
            rest_assert:status(200, Page2),
            rest_assert:json_path([<<"payload">>, <<"has_more">>], false, Page2),
            rest_assert:predicate(
                [<<"payload">>, <<"messages">>],
                fun(Messages) ->
                    is_list(Messages) andalso length(Messages) =:= 1 andalso
                        lists:all(
                            fun(M) ->
                                maps:get(<<"conv_seq">>, M, none) > NextSeq andalso
                                    lists:member(maps:get(<<"msg_id">>, M, none), MsgIds)
                            end,
                            Messages
                        )
                end,
                Page2
            )
        end
    ).

%% MSG-003: missing chat_type / peer_id and unknown chat_type are rejected
%% with the real 400 envelope codes and messages.
msg_003_history_param_validation(Config) ->
    UserA = ?config(user_a, Config),
    Port = ?config(http_port, Config),
    Headers = headers(Config, UserA),

    MissingChatType = rest_client:request(
        Port, <<"GET">>, <<"/api/v1/msg/history?peer_id=1">>, <<>>, Headers
    ),
    MissingPeerId = rest_client:request(
        Port, <<"GET">>, <<"/api/v1/msg/history?chat_type=c2c">>, <<>>, Headers
    ),
    BadChatType = rest_client:request(
        Port, <<"GET">>, <<"/api/v1/msg/history?chat_type=xx&peer_id=1">>, <<>>, Headers
    ),

    verify(
        <<"MSG-003">>,
        <<"GET">>,
        <<"/api/v1/msg/history?chat_type=xx&peer_id=1">>,
        <<>>,
        BadChatType,
        #{
            <<"http_status">> => 200,
            <<"code">> => ?ERR_BAD_REQUEST,
            <<"msg">> => <<"不支持的 chat_type: xx"/utf8>>
        },
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_BAD_REQUEST,
                    <<"msg">> => <<"缺少 chat_type 参数"/utf8>>
                },
                MissingChatType
            ),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_BAD_REQUEST, <<"msg">> => <<"缺少 peer_id 参数"/utf8>>},
                MissingPeerId
            ),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_BAD_REQUEST,
                    <<"msg">> => <<"不支持的 chat_type: xx"/utf8>>
                },
                Resp
            )
        end
    ).

%% MSG-004: add then remove one emoji on a seeded c2c message sent by A.
msg_004_reaction_add_remove_cycle(Config) ->
    UserA = ?config(user_a, Config),
    UserB = ?config(user_b, Config),
    AUid = uid(UserA),
    BUid = uid(UserB),
    {MsgId, _} = seed_c2c_msg(AUid, BUid),
    Emoji = <<"👍"/utf8>>,

    AddBody = #{<<"msg_id">> => MsgId, <<"msg_type">> => <<"c2c">>, <<"emoji">> => Emoji},
    AddResponse = post(Config, UserA, ?REACTION_ADD_PATH, AddBody),
    RemoveResponse = post(Config, UserA, ?REACTION_REMOVE_PATH, AddBody),

    verify(
        <<"MSG-004">>,
        <<"POST">>,
        ?REACTION_ADD_PATH,
        AddBody,
        AddResponse,
        #{
            <<"http_status">> => 200,
            <<"code">> => 0,
            <<"msg">> => <<"添加表情成功"/utf8>>
        },
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 0,
                    <<"msg">> => <<"添加表情成功"/utf8>>,
                    <<"payload">> => #{<<"msg_id">> => MsgId, <<"emoji">> => Emoji}
                },
                Resp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"user_id">>], fun(Val) -> Val =:= AUid end, Resp
            ),
            %% reaction/add echoes created_at as an integer millisecond
            %% epoch (probe2 MSG-004: 1789922643788), not a string.
            rest_assert:predicate(
                [<<"payload">>, <<"created_at">>], fun positive_integer/1, Resp
            ),
            rest_assert:status(200, RemoveResponse),
            rest_assert:json_contains(
                #{
                    <<"code">> => 0,
                    <<"msg">> => <<"移除表情成功"/utf8>>,
                    <<"payload">> => #{<<"msg_id">> => MsgId, <<"emoji">> => Emoji}
                },
                RemoveResponse
            )
        end
    ).

%% MSG-005: reactions on a msg id that was never seeded collapse into the
%% real 404 business envelope (HTTP stays 200).
msg_005_reaction_message_not_found(Config) ->
    UserA = ?config(user_a, Config),
    PhantomMsgId = rest_fixture:unique_id(<<"phantom">>),
    Body = #{
        <<"msg_id">> => PhantomMsgId,
        <<"msg_type">> => <<"c2c">>,
        <<"emoji">> => <<"👍"/utf8>>
    },

    AddResponse = post(Config, UserA, ?REACTION_ADD_PATH, Body),
    RemoveResponse = post(Config, UserA, ?REACTION_REMOVE_PATH, Body),
    verify(
        <<"MSG-005">>,
        <<"POST">>,
        ?REACTION_ADD_PATH,
        Body,
        AddResponse,
        #{
            <<"http_status">> => 200,
            <<"code">> => ?ERR_MESSAGE_NOT_FOUND,
            <<"msg">> => <<"消息不存在"/utf8>>
        },
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_MESSAGE_NOT_FOUND,
                    <<"msg">> => <<"消息不存在"/utf8>>
                },
                Resp
            ),
            rest_assert:status(200, RemoveResponse),
            rest_assert:json_contains(
                #{
                    <<"code">> => ?ERR_MESSAGE_NOT_FOUND,
                    <<"msg">> => <<"消息不存在"/utf8>>
                },
                RemoveResponse
            )
        end
    ).

%% MSG-006: missing msg_id / emoji, empty emoji and unsupported msg_type
%% hit the real 400 validation branches.
msg_006_reaction_param_validation(Config) ->
    UserA = ?config(user_a, Config),

    NoMsgId = post(
        Config, UserA, ?REACTION_ADD_PATH, #{<<"emoji">> => <<"👍"/utf8>>}
    ),
    NoEmoji = post(
        Config, UserA, ?REACTION_ADD_PATH, #{<<"msg_id">> => <<"rest-some-msg">>}
    ),
    EmptyEmoji = post(
        Config,
        UserA,
        ?REACTION_ADD_PATH,
        #{<<"msg_id">> => <<"rest-some-msg">>, <<"emoji">> => <<>>}
    ),
    BadMsgType = post(
        Config,
        UserA,
        ?REACTION_ADD_PATH,
        #{
            <<"msg_id">> => <<"rest-some-msg">>,
            <<"msg_type">> => <<"xxx">>,
            <<"emoji">> => <<"👍"/utf8>>
        }
    ),
    verify(
        <<"MSG-006">>,
        <<"POST">>,
        ?REACTION_ADD_PATH,
        #{<<"msg_id">> => <<"missing">>},
        NoMsgId,
        #{
            <<"http_status">> => 200,
            <<"code">> => ?ERR_BAD_REQUEST,
            <<"msg">> => <<"缺少消息ID参数"/utf8>>
        },
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_BAD_REQUEST, <<"msg">> => <<"缺少消息ID参数"/utf8>>}, Resp
            ),
            rest_assert:status(200, NoEmoji),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_BAD_REQUEST, <<"msg">> => <<"缺少emoji参数"/utf8>>}, NoEmoji
            ),
            rest_assert:status(200, EmptyEmoji),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_BAD_REQUEST, <<"msg">> => <<"emoji不能为空"/utf8>>}, EmptyEmoji
            ),
            rest_assert:status(200, BadMsgType),
            rest_assert:json_contains(
                #{<<"code">> => ?ERR_BAD_REQUEST, <<"msg">> => <<"不支持的消息类型"/utf8>>},
                BadMsgType
            )
        end
    ).

%% MSG-007: signed history request without Authorization is stopped with
%% HTTP 401 / ERR_TOKEN_MISSING.
msg_007_history_missing_token(Config) ->
    SignKey = rest_fixture:sign_key(),
    Did = rest_fixture:unique_id(<<"d">>),
    Headers = rest_fixture:signed_headers(Did, SignKey),
    Path = <<"/api/v1/msg/history?chat_type=c2c&peer_id=1">>,
    Response = rest_client:request(?config(http_port, Config), <<"GET">>, Path, <<>>, Headers),
    verify(
        <<"MSG-007">>,
        <<"GET">>,
        Path,
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

%% ===================================================================
%% Helpers
%% ===================================================================

verify(CaseId, Method, Path, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_msg">>,
        method => Method,
        path => Path,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

history_path(PeerUid, AfterSeq, Limit) ->
    <<
        "/api/v1/msg/history?chat_type=c2c&peer_id=",
        (integer_to_binary(PeerUid))/binary,
        "&after_seq=",
        (integer_to_binary(AfterSeq))/binary,
        "&limit=",
        (integer_to_binary(Limit))/binary
    >>.

conv_key(UidA, UidB) ->
    Min = integer_to_binary(erlang:min(UidA, UidB)),
    Max = integer_to_binary(erlang:max(UidA, UidB)),
    <<"c2c:", Min/binary, ":", Max/binary>>.

%% Seed N archive rows into public.msg_store through the production repo
%% (conv_seq is assigned by msg_store_seq exactly like the store worker).
%% Plain text payloads, no e2ee envelope: no real encrypted traffic.
seed_archive_msgs(FromUid, ToUid, N) ->
    [
        begin
            MsgId = rest_fixture:unique_id(<<"amsg">>),
            CreatedAt = elib_dt:to_rfc3339(erlang:system_time(millisecond)),
            Row = #{
                <<"type">> => <<"c2c">>,
                <<"from_id">> => Sender,
                <<"to_id">> => Receiver,
                <<"msg_id">> => MsgId,
                <<"msg_type">> => <<"text">>,
                <<"e2ee">> => null,
                <<"sender_did">> => null,
                <<"payload">> =>
                    jsone:encode(
                        #{
                            <<"msg_type">> => <<"text">>,
                            <<"content">> => <<"rest-history-seed ", MsgId/binary>>
                        },
                        [native_utf8]
                    ),
                <<"created_at">> => CreatedAt,
                <<"server_ts">> => CreatedAt
            },
            ok = msg_archive_repo:archive(Row),
            MsgId
        end
     || {_I, Sender, Receiver} <-
            [
                {I, pick_sender(FromUid, ToUid, I), pick_receiver(FromUid, ToUid, I)}
             || I <- lists:seq(1, N)
            ]
    ].

%% Odd rows travel From->To, even rows To->From, so the seeded history is
%% a real two-way conversation under the direction-agnostic
%% c2c:<min>:<max> conv key. (An earlier draft took both endpoints from
%% the same side, producing self-to-self rows whose conv key never
%% matched the queried one.)
pick_sender(FromUid, _ToUid, I) when I rem 2 =:= 1 -> FromUid;
pick_sender(_FromUid, ToUid, _I) -> ToUid.

pick_receiver(_FromUid, ToUid, I) when I rem 2 =:= 1 -> ToUid;
pick_receiver(FromUid, _ToUid, _I) -> FromUid.

%% Seed one real msg_c2c row through the production write path.
seed_c2c_msg(FromUid, ToUid) ->
    MsgId = rest_fixture:unique_id(<<"msg">>),
    PayloadMap = #{
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"rest-reaction-seed ", MsgId/binary>>
    },
    Now = erlang:system_time(millisecond),
    %% The production repo answers the plain atom ok on success
    %% (msg_c2c_repo: {ok, Count} when Count > 0 -> ok).
    ok = msg_c2c_ds:write_msg(Now, MsgId, PayloadMap, FromUid, ToUid, Now),
    {MsgId, PayloadMap}.

uid(User) ->
    maps:get(uid, User).

body(#{body := Body}) ->
    Body.

body_path(Body, Path) ->
    body_path(Body, Path, none).

body_path(Body, [], _Default) ->
    Body;
body_path(Body, [Key | Rest], Default) when is_map(Body) ->
    body_path(maps:get(Key, Body, Default), Rest, Default);
body_path(_Body, _Rest, Default) ->
    Default.

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

headers(_Config, User) ->
    Did = rest_fixture:unique_id(<<"d">>),
    maps:merge(
        rest_fixture:auth_header(User),
        rest_fixture:signed_headers(Did, rest_fixture:sign_key())
    ).

positive_integer(Value) ->
    is_integer(Value) andalso Value > 0.
