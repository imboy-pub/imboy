-module(api_v1_group_SUITE).

-include_lib("common_test/include/ct.hrl").

%% RTF-07 / A6: first regression batch for the group domain, executed as a
%% real black-box HTTP flow against the Cowboy listener + scratch PostgreSQL.
%% Every expected envelope here is transcribed from the current code:
%%   src/api/group_handler.erl, src/api/group_member_handler.erl,
%%   src/logic/group_logic.erl, src/logic/group_member_logic.erl,
%%   src/ds/group_ds.erl, src/ds/group_member_ds.erl, elib_response.
%%
%% Notable real behaviours encoded here (verified in code, not invented):
%%   * group/detail for a MISSING gid answers HTTP 200 + code 0 with an
%%     EMPTY payload: group_repo:find_by_id -> elib_pg:one -> {ok, #{}}
%%     and the handler only maps {error, _} (SQL failure) to an error.
%%   * dissolve uses the same lookup, so a nonexistent gid or a
%%     non-owner both end in "只有拥有者才能够解散该群，或者群已解散".
%%   * group_member/join against a nonexistent gid falls through the
%%     capacity check (member_max defaults to 0 from an empty row) and
%%     answers "群成员已满。" instead of "群组不存在".
%%   * elib_response:error/4 places the human message in a TOP-LEVEL
%%     "field" envelope key, not inside payload.

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    group_001_create_detail_join_leave_dissolve/1,
    group_002_missing_token_rejected/1,
    group_003_dissolve_requires_owner_outider_join_denied/1,
    group_004_missing_gid_semantics/1,
    group_005_role_management_boundaries/1,
    group_006_join_validation_and_rejoin_idempotent/1
]).

-define(ADD, <<"/api/v1/group/add">>).
-define(DETAIL, <<"/api/v1/group/detail">>).
-define(DISSOLVE, <<"/api/v1/group/dissolve">>).
-define(JOIN, <<"/api/v1/group_member/join">>).
-define(LEAVE, <<"/api/v1/group_member/leave">>).
-define(PAGE, <<"/api/v1/group_member/page">>).
-define(ROLE, <<"/api/v1/group_member/role">>).

all() ->
    [
        group_001_create_detail_join_leave_dissolve,
        group_002_missing_token_rejected,
        group_003_dissolve_requires_owner_outider_join_denied,
        group_004_missing_gid_semantics,
        group_005_role_management_boundaries,
        group_006_join_validation_and_rejoin_idempotent
    ].

init_per_suite(Config0) ->
    ok = rest_fixture:ensure_ct_priv_alias(),
    Config = eunit_runner:ct_suite_setup(Config0),
    ok = application:set_env(imboy, api_auth_switch, <<"on">>),
    %% GROUP cases log in 14 fixture users per run; the passport per-IP
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
%% GROUP-001 Happy Path: owner creates a group, reads its detail,
%% invites a member, the member sees the member page, leaves again,
%% and the owner dissolves the group; the detail afterwards reflects
%% the deleted row with an empty payload.
%% ===================================================================
group_001_create_detail_join_leave_dissolve(Config) ->
    Owner = login_user(Config),
    Member = login_user(Config),

    AddResp = group_add(Config, Owner),
    Gid = extract_gid(AddResp),

    DetailResp = group_detail(Config, Owner, Gid),
    JoinResp = group_join(Config, Owner, Gid, Member),
    PageResp = member_page(Config, Member, Gid),
    LeaveResp = group_leave(Config, Member, Gid),
    PageAfterLeaveResp = member_page(Config, Member, Gid),
    DissolveResp = group_dissolve(Config, Owner, Gid),
    DetailAfterResp = group_detail(Config, Owner, Gid),

    verify(
        <<"GROUP-001">>,
        <<"GET">>,
        group_detail_path(Gid),
        <<>>,
        DetailAfterResp,
        #{<<"http_status">> => 200, <<"code">> => 0, <<"payload">> => <<"{} (group deleted)">>},
        fun(Resp) ->
            assert_group_add(AddResp, Owner, Gid),
            assert_group_detail(DetailResp, Gid, Owner),
            assert_member_join(JoinResp, Gid, Member),
            common_assertions(PageResp),
            rest_assert:json_contains(#{<<"code">> => 0}, PageResp),
            rest_assert:predicate(
                [<<"payload">>, <<"list">>],
                fun(L) -> list_contains_user(L, uid(Member)) end,
                PageResp
            ),
            common_assertions(LeaveResp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 0,
                    <<"msg">> => <<"success.">>,
                    <<"payload">> => #{<<"gid">> => Gid}
                },
                LeaveResp
            ),
            common_assertions(PageAfterLeaveResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"你不是群成员"/utf8>>}, PageAfterLeaveResp
            ),
            common_assertions(DissolveResp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 0,
                    <<"msg">> => <<"success.">>,
                    <<"payload">> => #{<<"gid">> => Gid}
                },
                DissolveResp
            ),
            %% The dissolve hard-deletes the group row; detail answers the
            %% empty-success shape of a missing gid (see header comment).
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, Resp)
        end
    ).

%% ===================================================================
%% GROUP-002 Authentication: group/detail without any Authorization
%% header is rejected at the JWT boundary (HTTP 401, envelope code 401,
%% ERR_TOKEN_MISSING); a garbage Bearer token is also rejected with 401
%% and envelope code 706 (ERR_TOKEN_MALFORMED).
%% ===================================================================
group_002_missing_token_rejected(Config) ->
    Did = rest_fixture:unique_id(<<"d">>),
    UnsignedHeaders = rest_fixture:signed_headers(Did, ?config(sign_key, Config)),
    Path = group_detail_path(missing_gid()),

    MissingResp = rest_client:request(
        ?config(http_port, Config), <<"GET">>, Path, <<>>, UnsignedHeaders
    ),
    GarbageHeaders = UnsignedHeaders#{<<"authorization">> => <<"Bearer garbage-not-a-jwt">>},
    GarbageResp = rest_client:request(
        ?config(http_port, Config), <<"GET">>, Path, <<>>, GarbageHeaders
    ),

    verify(
        <<"GROUP-002">>,
        <<"GET">>,
        Path,
        <<>>,
        MissingResp,
        #{<<"http_status">> => 401, <<"code">> => 401},
        fun(Resp) ->
            rest_assert:status(401, Resp),
            rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Resp),
            rest_assert:json_contains(
                #{<<"code">> => 401, <<"msg">> => <<"未登录，请先登录"/utf8>>}, Resp
            ),
            rest_assert:status(401, GarbageResp),
            rest_assert:json_contains(#{<<"code">> => 706}, GarbageResp)
        end
    ).

%% ===================================================================
%% GROUP-003 Authorization: only the owner may dissolve (non-owner gets
%% the owner-only business error and the group survives), and an
%% outsider who is not a group member cannot invite others via
%% group_member/join ("你不是群成员").
%% ===================================================================
group_003_dissolve_requires_owner_outider_join_denied(Config) ->
    Owner = login_user(Config),
    Member = login_user(Config),
    Outsider = login_user(Config),

    AddResp = group_add(Config, Owner),
    Gid = extract_gid(AddResp),
    _ = group_join(Config, Owner, Gid, Member),

    OutsiderJoinReq = #{<<"gid">> => Gid, <<"member_uids">> => [uid_bin(Member)]},
    OutsiderJoinResp = post(Config, Outsider, ?JOIN, OutsiderJoinReq),
    DissolveByMemberReq = #{<<"gid">> => Gid},
    DissolveByMemberResp = post(Config, Member, ?DISSOLVE, DissolveByMemberReq),
    DetailResp = group_detail(Config, Member, Gid),

    verify(
        <<"GROUP-003">>,
        <<"POST">>,
        ?DISSOLVE,
        DissolveByMemberReq,
        DissolveByMemberResp,
        #{
            <<"http_status">> => 200,
            <<"code">> => 1,
            <<"msg">> => <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>
        },
        fun(Resp) ->
            common_assertions(OutsiderJoinResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"你不是群成员"/utf8>>}, OutsiderJoinResp
            ),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>,
                    <<"payload">> => #{}
                },
                Resp
            ),
            %% The denied dissolve must not have removed the group.
            common_assertions(DetailResp),
            rest_assert:json_contains(#{<<"payload">> => #{<<"id">> => Gid}}, DetailResp)
        end
    ).

%% ===================================================================
%% GROUP-004 Not Found semantics for a gid that never existed:
%%   * detail answers success with an EMPTY payload (missing row is an
%%     empty map, not an error),
%%   * dissolve ends in the owner-only error (owner_uid defaults to 0),
%%   * join ends in "群成员已满。" because the empty group row yields
%%     member_max=0 (capacity diff 0).
%% ===================================================================
group_004_missing_gid_semantics(Config) ->
    Owner = login_user(Config),
    Gid = missing_gid(),

    DetailResp = group_detail(Config, Owner, Gid),

    DissolveReq = #{<<"gid">> => Gid},
    DissolveResp = post(Config, Owner, ?DISSOLVE, DissolveReq),

    SelfJoinReq = #{<<"gid">> => Gid, <<"member_uids">> => [uid_bin(Owner)]},
    SelfJoinResp = post(Config, Owner, ?JOIN, SelfJoinReq),

    verify(
        <<"GROUP-004">>,
        <<"POST">>,
        ?DISSOLVE,
        DissolveReq,
        DissolveResp,
        #{
            <<"http_status">> => 200,
            <<"code">> => 1,
            <<"msg">> => <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>
        },
        fun(Resp) ->
            common_assertions(DetailResp),
            rest_assert:json_contains(#{<<"code">> => 0, <<"payload">> => #{}}, DetailResp),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>,
                    <<"payload">> => #{}
                },
                Resp
            ),
            common_assertions(SelfJoinResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"群成员已满。"/utf8>>}, SelfJoinResp
            )
        end
    ).

%% ===================================================================
%% GROUP-005 Role management: a plain member cannot change roles, the
%% owner promotes the member to admin, and an out-of-range role value
%% is rejected by parameter validation before any permission check.
%% ===================================================================
group_005_role_management_boundaries(Config) ->
    Owner = login_user(Config),
    Member = login_user(Config),
    Stranger = login_user(Config),

    AddResp = group_add(Config, Owner),
    Gid = extract_gid(AddResp),
    _ = group_join(Config, Owner, Gid, Member),

    MemberRoleReq = #{<<"gid">> => Gid, <<"user_id">> => uid(Member), <<"role">> => 3},
    MemberRoleResp = post(Config, Member, ?ROLE, MemberRoleReq),

    OwnerRoleReq = #{<<"gid">> => Gid, <<"user_id">> => uid(Member), <<"role">> => 3},
    OwnerRoleResp = post(Config, Owner, ?ROLE, OwnerRoleReq),

    BadRoleReq = #{<<"gid">> => Gid, <<"user_id">> => uid(Member), <<"role">> => 9},
    BadRoleResp = post(Config, Stranger, ?ROLE, BadRoleReq),

    verify(
        <<"GROUP-005">>,
        <<"POST">>,
        ?ROLE,
        OwnerRoleReq,
        OwnerRoleResp,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(MemberRoleResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"你没有权限修改群成员角色"/utf8>>}, MemberRoleResp
            ),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 0,
                    <<"msg">> => <<"success.">>,
                    <<"payload">> => #{<<"gid">> => Gid, <<"user_id">> => uid(Member)}
                },
                Resp
            ),
            %% The range guard runs before the permission gate, so even a
            %% non-member stranger gets the parameter error.
            common_assertions(BadRoleResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"角色值必须在1-3之间"/utf8>>}, BadRoleResp
            )
        end
    ).

%% ===================================================================
%% GROUP-006 Validation + idempotent re-join: parameter errors of
%% group_member/join (each exercised by a distinct fresh caller to stay
%% clear of the per-caller three-second throttle), then owner invites a
%% member and the member re-joins himself — the upsert answers code 0
%% with the unchanged member list (documented idempotency).
%% ===================================================================
group_006_join_validation_and_rejoin_idempotent(Config) ->
    Owner = login_user(Config),
    Member = login_user(Config),
    EmptyCaller = login_user(Config),
    BadListCaller = login_user(Config),
    ZeroGidCaller = login_user(Config),

    EmptyReq = #{<<"gid">> => missing_gid(), <<"member_uids">> => []},
    EmptyResp = post(Config, EmptyCaller, ?JOIN, EmptyReq),

    BadListReq = #{<<"gid">> => missing_gid(), <<"member_uids">> => 42},
    BadListResp = post(Config, BadListCaller, ?JOIN, BadListReq),

    ZeroGidReq = #{<<"gid">> => 0, <<"member_uids">> => [uid_bin(Owner)]},
    ZeroGidResp = post(Config, ZeroGidCaller, ?JOIN, ZeroGidReq),

    AddResp = group_add(Config, Owner),
    Gid = extract_gid(AddResp),
    JoinResp = group_join(Config, Owner, Gid, Member),
    ReJoinReq = #{<<"gid">> => Gid, <<"member_uids">> => [uid_bin(Member)]},
    ReJoinResp = post(Config, Member, ?JOIN, ReJoinReq),

    verify(
        <<"GROUP-006">>,
        <<"POST">>,
        ?JOIN,
        ReJoinReq,
        ReJoinResp,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(EmptyResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"member_uids 不能为空"/utf8>>}, EmptyResp
            ),
            common_assertions(BadListResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"member_uids 必须是list"/utf8>>}, BadListResp
            ),
            common_assertions(ZeroGidResp),
            rest_assert:json_contains(
                #{<<"code">> => 1, <<"msg">> => <<"group id 格式有误"/utf8>>}, ZeroGidResp
            ),
            assert_group_add(AddResp, Owner, Gid),
            assert_member_join(JoinResp, Gid, Member),
            common_assertions(Resp),
            rest_assert:json_contains(
                #{<<"code">> => 0, <<"payload">> => #{<<"gid">> => Gid}}, Resp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"member_list">>],
                fun(L) -> list_contains_user(L, uid(Member)) end,
                Resp
            )
        end
    ).

%% ===================================================================
%% Helpers
%% ===================================================================

login_user(Config) ->
    LoggedIn = rest_fixture:login(rest_fixture:create_user(#{}), ?config(sign_key, Config)),
    %% The JWT gate needs the asynchronously written user_device row.
    ok = rest_fixture:await_device_active(uid(LoggedIn), maps:get(did, LoggedIn)),
    LoggedIn.

uid(#{uid := Uid}) ->
    Uid.

uid_bin(#{uid := Uid}) ->
    integer_to_binary(Uid).

%% A gid that is guaranteed absent from the scratch database: a fresh
%% random bigint below 2^62 (stays inside the PG bigint range).
missing_gid() ->
    rand:uniform(4611686018427387903).

group_add(Config, Owner) ->
    post(Config, Owner, ?ADD, #{}).

group_detail(Config, User, Gid) ->
    get(Config, User, group_detail_path(Gid)).

group_detail_path(Gid) ->
    <<?DETAIL/binary, "?gid=", (integer_to_binary(Gid))/binary>>.

group_dissolve(Config, User, Gid) ->
    post(Config, User, ?DISSOLVE, #{<<"gid">> => Gid}).

group_join(Config, Inviter, Gid, Invitee) ->
    Body = #{<<"gid">> => Gid, <<"member_uids">> => [uid_bin(Invitee)]},
    post(Config, Inviter, ?JOIN, Body).

group_leave(Config, User, Gid) ->
    Body = #{<<"gid">> => Gid, <<"member_uids">> => [uid_bin(User)]},
    post(Config, User, ?LEAVE, Body).

member_page(Config, User, Gid) ->
    Path = <<?PAGE/binary, "?gid=", (integer_to_binary(Gid))/binary>>,
    get(Config, User, Path).

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
        api => <<"api_v1_group">>,
        method => Method,
        path => Path,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

%% group/add answers #{group => #{id => Gid, owner_uid => OwnerUid, ...},
%% member_list => [owner, ...]} (group_logic:add/5 with personal scope).
assert_group_add(AddResp, Owner, Gid) ->
    common_assertions(AddResp),
    rest_assert:json_contains(
        #{
            <<"code">> => 0,
            <<"msg">> => <<"success.">>,
            <<"payload">> =>
                #{<<"group">> => #{<<"id">> => Gid, <<"owner_uid">> => uid(Owner)}}
        },
        AddResp
    ),
    rest_assert:predicate(
        [<<"payload">>, <<"member_list">>], fun(L) -> list_contains_user(L, uid(Owner)) end, AddResp
    ).

assert_group_detail(DetailResp, Gid, Owner) ->
    common_assertions(DetailResp),
    rest_assert:json_contains(
        #{
            <<"code">> => 0,
            <<"payload">> => #{<<"id">> => Gid, <<"owner_uid">> => uid(Owner)}
        },
        DetailResp
    ).

assert_member_join(JoinResp, Gid, Member) ->
    common_assertions(JoinResp),
    rest_assert:json_contains(
        #{<<"code">> => 0, <<"payload">> => #{<<"gid">> => Gid}}, JoinResp
    ),
    rest_assert:predicate(
        [<<"payload">>, <<"member_list">>],
        fun(L) -> list_contains_user(L, uid(Member)) end,
        JoinResp
    ).

extract_gid(AddResp) ->
    #{<<"code">> := 0, <<"payload">> := #{<<"group">> := #{<<"id">> := Gid}}} = maps:get(
        body, AddResp
    ),
    Gid.

list_contains_user(MemberList, Uid) when is_list(MemberList) ->
    lists:any(
        fun
            (M) when is_map(M) ->
                case maps:find(<<"user_id">>, M) of
                    {ok, Id} ->
                        ec_cnv:to_integer(Id) =:= Uid;
                    error ->
                        case maps:find(<<"id">>, M) of
                            {ok, Id2} -> ec_cnv:to_integer(Id2) =:= Uid;
                            error -> false
                        end
                end;
            (_) ->
                false
        end,
        MemberList
    );
list_contains_user(_, _) ->
    false.
