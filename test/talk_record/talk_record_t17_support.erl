%%% @doc T17 talk_record 授权/分享/回放域 eunit 支撑（test-only）。
%%%
%%% 复用 T15/T05 支撑（池/环境/tsid meck/夹具），叠加：
%%%   * jwt_key 注入（playback 票据 HS256 签名；用后恢复）；
%%%   * share-card 严格标题开关注入（两态测试；用后恢复）；
%%%   * 用户目录列（nickname/account/avatar）回填与候选翻页批用户
%%%     （ORG_A 追加 25 成员，验证 cursor 分页 >20 行为）；
%%%   * grants 相关直查（活跃行计数、acl_revision/revision 读取）。
%%%
%%% 环境依赖 runners/T17/parent_stubs.sql（"user" 桩含 nickname/account/
%%% avatar/status/account_type 列，较 T15 桩扩展；隔离库 55433 串行复用）。
-module(talk_record_t17_support).

-export([
    setup_all/0,
    teardown_all/1,
    seed/1,
    ids/0,
    sql/2,
    one/2,
    conn_of/1,
    rec_uuid/1,
    grant_count/2,
    active_grant_count/3,
    acl_revision/2,
    revisions/2,
    with_env/3
]).

-define(JWT_KEY, <<"t17_playback_signing_key_0123456789abcdef">>).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    State = talk_record_t15_support:setup_all(),
    Prev = #{
        jwt_key => application:get_env(imboy, jwt_key),
        strict => application:get_env(imboy, talk_record_share_card_strict_title)
    },
    ok = application:set_env(imboy, jwt_key, ?JWT_KEY),
    State#{t17_prev_env => Prev}.

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    #{t17_prev_env := Prev} = State,
    restore_env(jwt_key, maps:get(jwt_key, Prev)),
    restore_env(talk_record_share_card_strict_title, maps:get(strict, Prev)),
    talk_record_t15_support:teardown_all(State).

-spec restore_env(atom(), undefined | {ok, term()}) -> ok.
restore_env(Key, undefined) ->
    application:unset_env(imboy, Key);
restore_env(Key, {ok, Value}) ->
    application:set_env(imboy, Key, Value).

%% @doc 临时覆写 app env（测试后恢复；非事务性，用于两态开关）。
-spec with_env(atom(), term(), fun(() -> term())) -> term().
with_env(Key, Value, Fun) ->
    Prev = application:get_env(imboy, Key),
    ok = application:set_env(imboy, Key, Value),
    try
        Fun()
    after
        restore_env(Key, Prev)
    end.

%%%===================================================================
%%% 夹具
%%%===================================================================

-spec seed(map()) -> ok.
seed(State) ->
    Conn = maps:get(conn, State),
    ok = talk_record_t15_support:seed(State),
    %% 目录列回填（候选 DTO display_name/account 断言用）
    set_profile(Conn, 700001, <<"owner-a"/utf8>>, <<"ua1">>),
    set_profile(Conn, 700002, <<"viewer-b"/utf8>>, <<"ua2">>),
    set_profile(Conn, 700003, <<"editor-c"/utf8>>, <<"ua3">>),
    set_profile(Conn, 700004, <<"org-b-owner"/utf8>>, <<"ub4">>),
    set_profile(Conn, 700007, <<"stranger"/utf8>>, <<"us7">>),
    %% 候选翻页批：ORG_A 追加 25 成员（730001..730025，昵称含 page 标记）
    lists:foreach(
        fun(N) ->
            Uid = 730000 + N,
            ok = talk_record_parent_fixture:user(Conn, Uid, #{
                <<"nickname">> => <<"page-", (integer_to_binary(N))/binary>>,
                <<"account">> => <<"p", (integer_to_binary(N))/binary>>,
                <<"status">> => 1,
                <<"account_type">> => 0
            }),
            sql(Conn, [
                "INSERT INTO organization_member(organization_id,user_id,role,status)",
                " VALUES (710001, ",
                integer_to_binary(Uid),
                ", 'member', 'active')"
            ])
        end,
        lists:seq(1, 25)
    ).

-spec set_profile(pid() | map(), integer(), binary(), binary()) -> ok.
set_profile(Conn, Uid, Nickname, Account) ->
    sql(Conn, [
        "UPDATE \"user\" SET nickname='",
        Nickname,
        "', account='",
        Account,
        "' WHERE id=",
        integer_to_binary(Uid)
    ]).

-spec ids() -> map().
ids() ->
    talk_record_t15_support:ids().

-spec sql(pid() | map(), iolist()) -> ok.
sql(Conn, SqlIolist) ->
    talk_record_t15_support:sql(Conn, SqlIolist).

-spec one(pid() | map(), iolist()) -> map().
one(Conn, SqlIolist) ->
    talk_record_t15_support:one(Conn, SqlIolist).

-spec conn_of(map()) -> pid().
conn_of(State) ->
    maps:get(conn, State).

-spec rec_uuid(integer()) -> binary().
rec_uuid(N) ->
    talk_record_t15_support:rec_uuid(N).

%%%===================================================================
%%% 直查
%%%===================================================================

%% @doc 指定被授权人的活跃授权行数。
-spec grant_count(pid() | map(), integer()) -> non_neg_integer().
grant_count(Conn, RecordId) ->
    Row = one(Conn, [
        "SELECT count(*) AS n FROM talk_record_grant WHERE record_id=",
        integer_to_binary(RecordId),
        " AND revoked_at IS NULL"
    ]),
    maps:get(<<"n">>, Row, 0).

%% @doc 指定被授权人的活跃授权行数。
-spec active_grant_count(pid() | map(), integer(), integer()) -> non_neg_integer().
active_grant_count(Conn, RecordId, Uid) ->
    Row = one(Conn, [
        "SELECT count(*) AS n FROM talk_record_grant WHERE record_id=",
        integer_to_binary(RecordId),
        " AND grantee_user_id=",
        integer_to_binary(Uid),
        " AND revoked_at IS NULL"
    ]),
    maps:get(<<"n">>, Row, 0).

-spec acl_revision(pid() | map(), integer()) -> integer().
acl_revision(Conn, RecordId) ->
    Row = one(Conn, [
        "SELECT acl_revision FROM talk_record WHERE id=", integer_to_binary(RecordId)
    ]),
    maps:get(<<"acl_revision">>, Row, 0).

%% @doc 三版本域快照（§11.2 纪律断言用）。
-spec revisions(pid() | map(), integer()) -> map().
revisions(Conn, RecordId) ->
    one(Conn, [
        "SELECT revision, content_revision, acl_revision FROM talk_record WHERE id=",
        integer_to_binary(RecordId)
    ]).
