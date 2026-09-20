-module(api_v1_friend_SUITE).

-include_lib("common_test/include/ct.hrl").

%% RTF-07 / A6: first regression batch for the friend domain, executed as a
%% real black-box HTTP flow against the Cowboy listener + scratch PostgreSQL.
%% Every expected envelope here is transcribed from the current code:
%%   src/api/friend_handler.erl + src/logic/friend_logic.erl
%%   (+ friend_agg state machine, friend_repo, elib_response envelope).
%% Business errors are HTTP 200 + envelope code (401 only at the JWT
%% boundary; 902 at the device-sign boundary).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    friend_001_add_confirm_list_delete/1,
    friend_002_missing_token_rejected/1,
    friend_003_confirm_reject_without_pending/1,
    friend_004_add_missing_fields/1,
    friend_005_duplicate_request_and_already_friends/1,
    friend_006_reject_flow/1,
    friend_007_readd_after_delete/1
]).

-define(ADD, <<"/api/v1/friend/add">>).
-define(CONFIRM, <<"/api/v1/friend/confirm">>).
-define(REJECT, <<"/api/v1/friend/reject">>).
-define(DELETE, <<"/api/v1/friend/delete">>).
-define(LIST, <<"/api/v1/friend/list">>).

all() ->
    [
        friend_001_add_confirm_list_delete,
        friend_002_missing_token_rejected,
        friend_003_confirm_reject_without_pending,
        friend_004_add_missing_fields,
        friend_005_duplicate_request_and_already_friends,
        friend_006_reject_flow,
        friend_007_readd_after_delete
    ].

init_per_suite(Config0) ->
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    %% FRIEND cases log in 12 fixture users per run; the passport per-IP
    %% bucket default (10/min) would answer 429 to later logins. Capacity
    %% configuration for the test environment, not a bypass of any
    %% behavior under test (rate limiting is not in this batch's list).
    ok = rest_fixture:ensure_login_throttle_capacity(),
    Port = ranch:get_port(imboy_listener),
    SignKey = rest_fixture:ensure_sign_key(),
    [{http_port, Port}, {sign_key, SignKey} | Config].

end_per_suite(Config) ->
    eunit_runner:ct_suite_cleanup(Config).

%% ===================================================================
%% FRIEND-001 Happy Path: A adds B, B confirms, A's friend/list contains
%% B, A deletes B, B is gone from the list again.
%% ===================================================================
friend_001_add_confirm_list_delete(Config) ->
    {A, B} = friend_pair(Config),

    AddReq = add_request(B),
    AddResp = post(Config, A, ?ADD, AddReq),

    ConfirmReq = confirm_request(A, B),
    ConfirmResp = post(Config, B, ?CONFIRM, ConfirmReq),

    ListResp = get(Config, A, ?LIST),

    DeleteReq = delete_request(B),
    DeleteResp = post(Config, A, ?DELETE, DeleteReq),

    ListAfterResp = get(Config, A, ?LIST),

    verify(
        <<"FRIEND-001">>,
        <<"GET">>,
        ?LIST,
        <<>>,
        ListAfterResp,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(AddResp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success.">>, <<"payload">> => #{}}, AddResp
            ),
            common_assertions(ConfirmResp),
            rest_assert:json_contains(#{<<"code">> => 0}, ConfirmResp),
            %% confirm_friend_resp: friend card of the requester A with
            %% is_friend=1 and peerId (friend_logic:confirm_friend_resp/2).
            rest_assert:json_contains(
                #{
                    <<"payload">> =>
                        #{
                            <<"id">> => uid(A),
                            <<"peerId">> => uid(A),
                            <<"is_friend">> => 1
                        }
                },
                ConfirmResp
            ),
            common_assertions(ListResp),
            rest_assert:predicate(
                [<<"payload">>, <<"friend">>], fun(L) -> list_contains_uid(L, uid(B)) end, ListResp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"mine">>, <<"id">>],
                fun(I) -> ec_cnv:to_integer(I) =:= uid(A) end,
                ListResp
            ),
            common_assertions(DeleteResp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success">>, <<"payload">> => #{}}, DeleteResp
            ),
            common_assertions(Resp),
            rest_assert:predicate(
                [<<"payload">>, <<"friend">>],
                fun(L) -> is_list(L) andalso not list_contains_uid(L, uid(B)) end,
                Resp
            )
        end
    ).

%% ===================================================================
%% FRIEND-002 Authentication: friend/list without any Authorization
%% header is rejected at the JWT boundary (HTTP 401, envelope code 401,
%% ERR_TOKEN_MISSING); a garbage Bearer token is also rejected with 401
%% and envelope code 706 (ERR_TOKEN_MALFORMED).
%% ===================================================================
friend_002_missing_token_rejected(Config) ->
    Did = rest_fixture:unique_id(<<"d">>),
    UnsignedHeaders = rest_fixture:signed_headers(Did, ?config(sign_key, Config)),

    MissingResp = rest_client:request(
        ?config(http_port, Config), <<"GET">>, ?LIST, <<>>, UnsignedHeaders
    ),

    GarbageHeaders = UnsignedHeaders#{<<"authorization">> => <<"Bearer garbage-not-a-jwt">>},
    GarbageResp = rest_client:request(
        ?config(http_port, Config), <<"GET">>, ?LIST, <<>>, GarbageHeaders
    ),

    verify(
        <<"FRIEND-002">>,
        <<"GET">>,
        ?LIST,
        <<>>,
        MissingResp,
        #{<<"http_status">> => 401, <<"code">> => 401},
        fun(Resp) ->
            rest_assert:status(401, Resp),
            rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Resp),
            rest_assert:json_contains(
                #{<<"code">> => 401, <<"msg">> => <<"未登录，请先登录"/utf8>>}, Resp
            ),
            %% The garbage-token variant hits verify_token -> 706 -> real 401.
            rest_assert:status(401, GarbageResp),
            rest_assert:json_contains(#{<<"code">> => 706}, GarbageResp)
        end
    ).

%% ===================================================================
%% FRIEND-003 Not Found: confirm/reject when no pending request exists.
%% The from side is a uid that does not exist in the database at all —
%% friend_logic does not validate user existence, it derives the
%% missing-resource error from the absent pending row
%% (friend_agg:accept/reject -> no_pending_request).
%% ===================================================================
friend_003_confirm_reject_without_pending(Config) ->
    B = login_user(Config),
    OtherUid = uid(login_user(Config)),
    GhostUid = missing_id(),

    ConfirmReq = confirm_request(GhostUid, uid(B)),
    ConfirmResp = post(Config, B, ?CONFIRM, ConfirmReq),

    RejectReq = #{<<"from">> => to_uid_term(OtherUid)},
    RejectResp = post(Config, B, ?REJECT, RejectReq),

    verify(
        <<"FRIEND-003">>,
        <<"POST">>,
        ?CONFIRM,
        ConfirmReq,
        ConfirmResp,
        #{
            <<"http_status">> => 200,
            <<"code">> => 1,
            <<"msg">> => <<"no_pending_request">>
        },
        fun(Resp) ->
            common_assertions(Resp),
            %% error/4 puts the human message in a top-level "field" key.
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"no_pending_request">>,
                    <<"field">> => <<"无待确认的好友申请"/utf8>>
                },
                Resp
            ),
            common_assertions(RejectResp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"no_pending_request">>,
                    <<"field">> => <<"无待拒绝的好友申请"/utf8>>
                },
                RejectResp
            )
        end
    ).

%% ===================================================================
%% FRIEND-004 Validation: friend/add requires to, payload and created_at;
%% each missing field yields "Parameter error" plus a top-level field key
%% naming the first missing parameter.
%% ===================================================================
friend_004_add_missing_fields(Config) ->
    {A, B} = friend_pair(Config),

    NoTo = #{<<"payload">> => #{}, <<"created_at">> => now_ms()},
    NoToResp = post(Config, A, ?ADD, NoTo),

    NoPayload = #{<<"to">> => uid(B), <<"created_at">> => now_ms()},
    NoPayloadResp = post(Config, A, ?ADD, NoPayload),

    NoCreatedAt = #{<<"to">> => uid(B), <<"payload">> => #{}},
    NoCreatedAtResp = post(Config, A, ?ADD, NoCreatedAt),

    verify(
        <<"FRIEND-004">>,
        <<"POST">>,
        ?ADD,
        NoCreatedAt,
        NoCreatedAtResp,
        #{<<"http_status">> => 200, <<"code">> => 1, <<"msg">> => <<"Parameter error">>},
        fun(Resp) ->
            common_assertions(NoToResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"Parameter error">>, <<"field">> => <<"to">>},
                NoToResp
            ),
            common_assertions(NoPayloadResp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"Parameter error">>,
                    <<"field">> => <<"payload">>
                },
                NoPayloadResp
            ),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"Parameter error">>,
                    <<"field">> => <<"created_at">>
                },
                Resp
            )
        end
    ).

%% ===================================================================
%% FRIEND-005 Conflict: a second add while the request is still pending
%% is refused with already_requested; once the request is confirmed an
%% add is refused with already_friends (friend_agg request state machine).
%% ===================================================================
friend_005_duplicate_request_and_already_friends(Config) ->
    {A, B} = friend_pair(Config),

    FirstAdd = post(Config, A, ?ADD, add_request(B)),
    DupAdd = post(Config, A, ?ADD, add_request(B)),
    Confirm = post(Config, B, ?CONFIRM, confirm_request(A, B)),
    ReAddReq = add_request(B),
    AddAfterFriends = post(Config, A, ?ADD, ReAddReq),

    verify(
        <<"FRIEND-005">>,
        <<"POST">>,
        ?ADD,
        ReAddReq,
        AddAfterFriends,
        #{<<"http_status">> => 200, <<"code">> => 1, <<"msg">> => <<"already_friends">>},
        fun(Resp) ->
            common_assertions(FirstAdd),
            rest_assert:json_contains(#{<<"code">> => 0}, FirstAdd),
            common_assertions(DupAdd),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"already_requested">>,
                    <<"field">> => <<"您已发送过好友申请，请等待对方确认"/utf8>>
                },
                DupAdd
            ),
            common_assertions(Confirm),
            rest_assert:json_contains(#{<<"code">> => 0}, Confirm),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"already_friends">>,
                    <<"field">> => <<"对方已是您的好友"/utf8>>
                },
                Resp
            )
        end
    ).

%% ===================================================================
%% FRIEND-006 Reject flow: B rejects A's pending request (pending row
%% removed), a later confirm finds no pending request, and A may send a
%% fresh request (state machine back to none).
%% ===================================================================
friend_006_reject_flow(Config) ->
    {A, B} = friend_pair(Config),

    Add = post(Config, A, ?ADD, add_request(B)),
    RejectReq = #{<<"from">> => uid(A)},
    Reject = post(Config, B, ?REJECT, RejectReq),
    ConfirmAfterReject = post(Config, B, ?CONFIRM, confirm_request(A, B)),
    ReAdd = post(Config, A, ?ADD, add_request(B)),

    verify(
        <<"FRIEND-006">>,
        <<"POST">>,
        ?REJECT,
        RejectReq,
        Reject,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Add),
            rest_assert:json_contains(#{<<"code">> => 0}, Add),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success.">>, <<"payload">> => #{}}, Resp
            ),
            common_assertions(ConfirmAfterReject),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"no_pending_request">>}, ConfirmAfterReject
            ),
            common_assertions(ReAdd),
            rest_assert:json_contains(#{<<"code">> => 0}, ReAdd)
        end
    ).

%% ===================================================================
%% FRIEND-007 Re-add after delete: delete always answers success (even
%% without a relationship) and removes both directions; the business
%% explicitly allows re-adding a deleted friend (add answers code 0
%% again, no Conflict error).
%% ===================================================================
friend_007_readd_after_delete(Config) ->
    {A, B} = friend_pair(Config),

    Add = post(Config, A, ?ADD, add_request(B)),
    Confirm = post(Config, B, ?CONFIRM, confirm_request(A, B)),
    Delete = post(Config, A, ?DELETE, delete_request(B)),
    ReAddReq = add_request(B),
    ReAdd = post(Config, A, ?ADD, ReAddReq),

    verify(
        <<"FRIEND-007">>,
        <<"POST">>,
        ?ADD,
        ReAddReq,
        ReAdd,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Add),
            rest_assert:json_contains(#{<<"code">> => 0}, Add),
            common_assertions(Confirm),
            rest_assert:json_contains(#{<<"code">> => 0}, Confirm),
            common_assertions(Delete),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success">>, <<"payload">> => #{}}, Delete
            ),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"msg">> => <<"success.">>, <<"payload">> => #{}}, Resp
            )
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

friend_pair(Config) ->
    {login_user(Config), login_user(Config)}.

login_user(Config) ->
    LoggedIn = rest_fixture:login(rest_fixture:create_user(#{}), ?config(sign_key, Config)),
    %% The JWT gate needs the asynchronously written user_device row.
    ok = rest_fixture:await_device_active(uid(LoggedIn), maps:get(did, LoggedIn)),
    LoggedIn.

uid(#{uid := Uid}) ->
    Uid.

%% A uid that is guaranteed absent from the scratch database: a fresh
%% random bigint below 2^62 (stays inside the PG bigint range).
missing_id() ->
    rand:uniform(4611686018427387903).

now_ms() ->
    erlang:system_time(millisecond).

add_request(ToUid) ->
    #{
        <<"to">> => to_uid_term(ToUid),
        <<"payload">> => #{<<"source">> => <<"rest-suite">>},
        <<"created_at">> => now_ms()
    }.

confirm_request(FromUid, ToUid) ->
    #{<<"from">> => to_uid_term(FromUid), <<"to">> => to_uid_term(ToUid), <<"payload">> => #{}}.

delete_request(ToUid) ->
    #{<<"user_id">> => to_uid_term(ToUid)}.

%% Fixture uids travel as JSON strings; friend_logic converts both shapes
%% via ec_cnv (add_friend/4 normalises To with ec_cnv:to_binary).
to_uid_term(Uid) when is_integer(Uid) ->
    integer_to_binary(Uid).

post(Config, User, Path, Body) ->
    rest_client:post(?config(http_port, Config), Path, Body, headers(Config, User)).

get(Config, User, Path) ->
    rest_client:request(?config(http_port, Config), <<"GET">>, Path, <<>>, headers(Config, User)).

headers(Config, User) ->
    Did = rest_fixture:unique_id(<<"d">>),
    maps:merge(
        rest_fixture:signed_headers(Did, ?config(sign_key, Config)),
        rest_fixture:auth_header(User)
    ).

verify(CaseId, Method, Path, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_friend">>,
        method => Method,
        path => Path,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

list_contains_uid(FriendList, Uid) when is_list(FriendList) ->
    lists:any(
        fun
            (M) when is_map(M) ->
                case maps:find(<<"id">>, M) of
                    {ok, Id} -> ec_cnv:to_integer(Id) =:= Uid;
                    error -> false
                end;
            (_) ->
                false
        end,
        FriendList
    );
list_contains_uid(_, _) ->
    false.
