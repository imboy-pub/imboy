-module(group_user_id_sum_p0_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% P0 修复回归：group.user_id_sum int8 溢出根治（终局方案，见
%%% docs/planning/group-user-id-sum-p0-decision-2026-08-29.md +
%%% 迁移 00000079）：
%%% B1 同成员集允许多群：不做创建幂等去重（微信/Telegram 同款行为），
%%%    连续两次同集合建群得到两个不同 Gid。
%%% B2 user_id_sum 列退役：>85 人大群加人不再溢出崩连接，统计仅 COUNT。
%%% B3 载荷字段全删（前后端同步清理）：函数不存在、载荷无该键。
%%% B4 迁移终态：user_id_sum 列与旧索引已删，无 member_set_hash 残留。
%%%===================================================================

%% 96 × TSID ≈ 1.05e19 > int8 上限，旧实现必溢出
-define(N_MEMBERS, 96).

p0_regression_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"B1 同成员集连续建群得到两个不同群（不去重）", {timeout, 120, fun t_same_set_multiple_groups/0}},
        {"B2 大群(96人)加人不再溢出崩连接", {timeout, 300, fun t_large_group_no_overflow/0}},
        {"B3 载荷字段全删：函数不存在，face2face 载荷无该键", {timeout, 30, fun t_payload_field_removed/0}},
        {"B4 迁移 00000079 终态：列/旧索引已删，无 hash 残留", {timeout, 30, fun t_migration_state/0}}
    ]}.

setup() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} -> ok;
        {error, _Reason} -> throw({skip, "Database not available"})
    end,
    _ = (catch elib_tsid:init(#{dc_id => 0, node_id => 0, dc_bits => 3})),
    ok.

cleanup(_Ctx) -> ok.

%% ---- helpers ----

create_user(Tag) ->
    Uid = elib_tsid:generate(),
    Suffix = integer_to_binary(erlang:phash2(Uid, 1000000000)),
    ok = user_repo:create(#{
        <<"uid">> => Uid,
        <<"nickname">> => Tag,
        <<"account">> => <<Tag/binary, "_", Suffix/binary>>,
        <<"mobile">> => list_to_binary(io_lib:format("13~9..0B", [erlang:phash2(Uid, 1000000000)])),
        <<"email">> => <<"p0t_", Suffix/binary, "@example.com">>,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    }),
    Uid.

create_users(Tag, N) ->
    [create_user(<<Tag/binary, (integer_to_binary(I))/binary>>) || I <- lists:seq(1, N)].

%% group_logic:add/4 的 MemberUids 是 binary 列表
to_bin_ids(Uids) -> [ec_cnv:to_binary(U) || U <- Uids].

active_member_count(Gid) ->
    {ok, [#{<<"c">> := C}]} = elib_pg:query(
        <<"SELECT COUNT(*) AS c FROM group_member WHERE group_id = $1 AND status > -1">>,
        [Gid]
    ),
    ec_cnv:to_integer(C).

%% ---- B1：同成员集允许多群（终局产品行为） ----

t_same_set_multiple_groups() ->
    Owner = create_user(<<"p0owner">>),
    Uids = create_users(<<"p0m">>, 3),

    {ok, G1} = group_logic:add(0, Owner, 2, to_bin_ids(Uids)),
    %% 同创建者+同成员集再建 → 第二个群（旧 sum 语义会返回同 Gid）
    {ok, G2} = group_logic:add(0, Owner, 2, to_bin_ids(Uids)),
    ?assert(G1 =/= G2),
    %% 两个群各自成员数正确
    ?assertEqual(4, active_member_count(G1)),
    ?assertEqual(4, active_member_count(G2)),
    ok.

%% ---- B2：大群加人不溢出 ----

t_large_group_no_overflow() ->
    Owner = create_user(<<"p0lgowner">>),
    Members = create_users(<<"p0lg">>, ?N_MEMBERS),

    {ok, Gid} = group_logic:add(0, Owner, 2, to_bin_ids(lists:sublist(Members, ?N_MEMBERS - 1))),

    %% 旧实现：第 96 人入群触发 update_statistics 写回 SUM(>int8)
    %% → {integer_overflow,int8} 崩连接。修复后：列已删、只 COUNT，入群成功。

    %% 建群时 Owner + N-1 成员
    AllActive = ?N_MEMBERS,
    ?assertEqual(AllActive, active_member_count(Gid)),
    Extra = lists:last(Members),
    ok = group_member_ds:add_member(Gid, Extra),
    ?assertEqual(AllActive + 1, active_member_count(Gid)),
    ok.

%% ---- B3：载荷字段全删（前后端同版本清理，无过渡期） ----

t_payload_field_removed() ->
    %% DS/Logic 层 API 已删除
    ?assertNot(erlang:function_exported(group_ds, get_user_id_sum, 1)),
    ?assertNot(erlang:function_exported(group_member_logic, get_user_id_sum, 1)),
    %% face2face 载荷不再携带该键
    ?WITH_MECKS(
        [
            {group_ds, [
                {'member_uids', 1, fun(_Gid) -> [7] end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(_Uid, _Col) ->
                    #{<<"nickname">> => <<"n">>, <<"avatar">> => <<>>, <<"account">> => <<"a">>}
                end}
            ]}
        ],
        fun() ->
            #{payload := Payload} = group_logic:face2face_notify_payload(7, 123),
            ?assertNot(maps:is_key(<<"user_id_sum">>, Payload)),
            ?assertEqual(123, maps:get(<<"gid">>, Payload))
        end
    ),
    ok.

%% ---- B4：迁移终态 ----

t_migration_state() ->
    %% user_id_sum 列已删
    {ok, []} = elib_pg:query(
        <<"SELECT column_name FROM information_schema.columns",
            " WHERE table_name = 'group' AND column_name = 'user_id_sum'">>,
        []
    ),
    %% 旧索引已删
    {ok, []} = elib_pg:query(
        <<"SELECT indexname FROM pg_indexes", " WHERE indexname = 'i_creatorid_memberidsum'">>, []
    ),
    %% 无 member_set_hash 中间方案残留（终局未采用）
    {ok, []} = elib_pg:query(
        <<"SELECT column_name FROM information_schema.columns",
            " WHERE table_name = 'group' AND column_name = 'member_set_hash'">>,
        []
    ),
    {ok, []} = elib_pg:query(
        <<"SELECT indexname FROM pg_indexes",
            " WHERE indexname = 'uq_group_creator_membersethash'">>,
        []
    ),
    ok.
