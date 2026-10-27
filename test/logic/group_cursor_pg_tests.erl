-module(group_cursor_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W07.b（channel-group-ux-v2.2 §6.5 C2 个人群 cursor 协议）：真实
%%% PostgreSQL 上验证签名游标协议的 J20 向量：
%%% - J20.a：多页全扫描（含 q）可达授权交集内全部对象；授权外同名不出现；
%%% - J20.b：关键词换代（q1 游标在 q2/空 q 语境）→ invalid（绑定不符
%%   统一 invalid 不回显差异），不回落；
%%% - J20.c：篡改游标 → invalid；跨账号 → invalid；错 list_kind → foreign；
%%% - J20.d：扫描中并发新增（更高 id）不误入旧扫描，重新首页扫描收敛。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880701).
-define(OTHER_UID, 880702).
%% UID 拥有 5 个「产品群N」；OTHER 拥有外部同名群与 UID 为成员的群。
-define(G1, 880801).
-define(G2, 880802).
-define(G3, 880803).
-define(G4, 880804).
-define(G5, 880805).
-define(G_FOREIGN, 880806).
-define(G_MEMBER, 880807).
-define(G_LATE, 880808).
-define(GIDS, [?G1, ?G2, ?G3, ?G4, ?G5]).
-define(ALL_GIDS, [?G1, ?G2, ?G3, ?G4, ?G5, ?G_FOREIGN, ?G_MEMBER, ?G_LATE]).
-define(ALL_UIDS, [?UID, ?OTHER_UID]).
-define(KEY_CFG, list_cursor_signing_key).
-define(KEY, <<"w07b_list_cursor_signing_key_0123456789abcdef">>).

%%%===================================================================
%%% 夹具
%%%===================================================================

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM group_member WHERE group_id = ANY($1)">>, [?ALL_GIDS]},
            {<<"DELETE FROM \"group\" WHERE id = ANY($1)">>, [?ALL_GIDS]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [?ALL_UIDS]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<
                    "INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES ",
                    "($1,'w07b-pg-uid','w07b-pg-uid','w07b-synthetic','127.0.0.1','w07b-synthetic'),",
                    "($2,'w07b-pg-other','w07b-pg-other','w07b-synthetic','127.0.0.1','w07b-synthetic')"
                >>,
                [?UID, ?OTHER_UID]
            },
            {
                %% ⚠️ 含 CJK 的分段各自 /utf8（低字节截断 → 22021）。
                <<
                    "INSERT INTO \"group\" (id, title, owner_uid, creator_uid, type) VALUES "/utf8,
                    "($1, '产品群一', $8, $8, 2),"/utf8,
                    "($2, '产品群二', $8, $8, 2),"/utf8,
                    "($3, '产品群三', $8, $8, 2),"/utf8,
                    "($4, '产品群四', $8, $8, 2),"/utf8,
                    "($5, '产品群五', $8, $8, 2),"/utf8,
                    "($6, '产品外部群', $9, $9, 2),"/utf8,
                    "($7, '产品成员群', $9, $9, 2)"/utf8
                >>,
                [?G1, ?G2, ?G3, ?G4, ?G5, ?G_FOREIGN, ?G_MEMBER, ?UID, ?OTHER_UID]
            },
            {
                <<"INSERT INTO group_member (id, group_id, user_id, role, status) ",
                    "VALUES (880811, $1, $2, 1, 1)">>,
                [?G_MEMBER, ?UID]
            }
        ]
    ),
    ok.

ctx(Q) -> #{scope => <<"personal">>, workspace_id => 0, q => Q}.

ids(#{list := Rows}) -> [maps:get(<<"id">>, R) || R <- Rows].

%% 沿 next_cursor 全扫描；返回 {页数, 全部 id（按扫描序）, 末页 has_more}。
scan_all(Kind, Uid, Limit, Q) ->
    scan_all(Kind, Uid, Limit, Q, none, 0, [], true).

%% Cursor = none（首页）| 上界 id（非首页）。
scan_all(_Kind, _Uid, _Limit, _Q, _Cursor, Pages, Acc, false) ->
    {Pages, lists:reverse(Acc), false};
scan_all(Kind, Uid, Limit, Q, Cursor, Pages, Acc, _Cont) ->
    BeforeId =
        case Cursor of
            none -> 16#7FFFFFFFFFFFFFFF;
            Upper when is_integer(Upper) -> Upper
        end,
    Fetcher =
        case Kind of
            <<"group_owned">> -> fun group_logic:page_by_owner_cursor/4;
            <<"group_joined">> -> fun group_logic:page_joined_cursor/4;
            <<"group_managed">> -> fun group_logic:page_managed_cursor/4
        end,
    {ok, #{list := Rows, has_more := HasMore, upper := Upper1}} =
        Fetcher(Uid, BeforeId, Limit, Q),
    {NextUpper, Cont} =
        case HasMore of
            true -> {Upper1, true};
            false -> {none, false}
        end,
    scan_all(
        Kind,
        Uid,
        Limit,
        Q,
        NextUpper,
        Pages + 1,
        lists:reverse(ids(#{list => Rows})) ++ Acc,
        Cont
    ).

%% 翻转首字节构造篡改游标（放宏外：epp 宏参数切分不识别 <<>> 内逗号）。
tamper(Token) when is_binary(Token), byte_size(Token) > 0 ->
    <<H, T/binary>> = Token,
    <<(H bxor 16#FF), T/binary>>.

%%%===================================================================
%%% 用例
%%%===================================================================

group_cursor_j20_vectors_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        PrevKey = application:get_env(imboy, ?KEY_CFG),
        application:set_env(imboy, ?KEY_CFG, ?KEY),
        cleanup_test_rows(),
        try
            seed_test_rows(),

            %% ── J20.a：owned 全扫描（limit=2 → 3 页）集合完整；
            %% join 路径只见成员群；manager 为空；q 不放宽授权。
            {P1, IdsOwned, TailMore} = scan_all(<<"group_owned">>, ?UID, 2, <<>>),
            ?assert(P1 >= 3),
            ?assertEqual(lists:sort(?GIDS), lists:sort(IdsOwned)),
            ?assertEqual(false, TailMore),
            {_PJ, IdsJoined, _} = scan_all(<<"group_joined">>, ?UID, 2, <<>>),
            ?assertEqual([?G_MEMBER], IdsJoined),
            %% managed = 群主 ∪ 管理员：UID 的 5 个自有群全部在内（owner
            %% 即管理角色），非成员外部群不出现。
            {_PM, IdsManaged, _} = scan_all(<<"group_managed">>, ?UID, 2, <<>>),
            ?assertEqual(lists:sort(?GIDS), lists:sort(IdsManaged)),
            %% q 在 managed 交集内过滤：命中全部自有「产品群」；外部不出现。
            {_, IdsManagedQ, _} =
                scan_all(<<"group_managed">>, ?UID, 2, <<"产品"/utf8>>),
            ?assertEqual(lists:sort(?GIDS), lists:sort(IdsManagedQ)),
            {_, IdsJoinedQ, _} =
                scan_all(<<"group_joined">>, ?UID, 2, <<"产品"/utf8>>),
            ?assertEqual([?G_MEMBER], IdsJoinedQ),

            %% ── J20.b：关键词换代——q1 游标在空 q/q2 语境 → foreign。
            {ok, #{has_more := true, upper := Upper1}} =
                group_logic:page_by_owner_cursor(
                    ?UID, 16#7FFFFFFFFFFFFFFF, 2, <<"产品"/utf8>>
                ),
            {ok, TokenQ1} =
                list_cursor:issue(
                    ?UID,
                    <<"group_owned">>,
                    (ctx(<<"产品"/utf8>>))#{upper => Upper1},
                    os:system_time(second)
                ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(TokenQ1, ?UID, <<"group_owned">>, ctx(<<>>))
            ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(TokenQ1, ?UID, <<"group_owned">>, ctx(<<"研发"/utf8>>))
            ),

            %% ── J20.c：篡改 → invalid；跨账号 → invalid；错 kind → foreign。
            Tampered = tamper(TokenQ1),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(
                    Tampered,
                    ?UID,
                    <<"group_owned">>,
                    ctx(<<"产品"/utf8>>)
                )
            ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(
                    TokenQ1,
                    ?OTHER_UID,
                    <<"group_owned">>,
                    ctx(<<"产品"/utf8>>)
                )
            ),
            ?assertMatch(
                {error, invalid},
                list_cursor:resolve(TokenQ1, ?UID, <<"group_joined">>, ctx(<<"产品"/utf8>>))
            ),

            %% ── J20.d：扫描中并发新增（更高 id）不误入旧扫描；重新首页
            %% 扫描收敛可见。
            {ok, #{has_more := true, upper := UpperMid}} =
                group_logic:page_by_owner_cursor(?UID, 16#7FFFFFFFFFFFFFFF, 2, <<>>),
            {ok, _} = elib_pg:execute(
                <<"INSERT INTO \"group\" (id, title, owner_uid, creator_uid, type) ",
                    "VALUES ($1, '产品群六', $2, $2, 2)"/utf8>>,
                [?G_LATE, ?UID]
            ),
            {ok, #{list := LateRows}} =
                group_logic:page_by_owner_cursor(?UID, UpperMid, 2, <<>>),
            ?assertNot(lists:member(?G_LATE, ids(#{list => LateRows}))),
            {_, IdsRescan, _} = scan_all(<<"group_owned">>, ?UID, 2, <<>>),
            ?assert(lists:member(?G_LATE, IdsRescan)),

            %% ── 终止页契约：has_more=false 时 upper=0。
            {ok, #{has_more := false, upper := 0}} =
                group_logic:page_by_owner_cursor(?UID, ?G1, 10, <<>>)
        after
            cleanup_test_rows(),
            case PrevKey of
                undefined -> application:unset_env(imboy, ?KEY_CFG);
                {ok, K} -> application:set_env(imboy, ?KEY_CFG, K)
            end
        end
    end).
