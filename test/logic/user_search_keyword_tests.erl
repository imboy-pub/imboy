-module(user_search_keyword_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% user/search find_by_keyword —— 关键词路由真库集成测试（PG 不可用自动 skip）
%%%
%%% 背景（auto_test 批次W2R2 bug）：邀请向导占位符声明支持「按用户名 / 用户 ID
%%% 搜索」，但 find_by_keyword 只路由 email/mobile/account 三个精确匹配分支，
%%% 纯数字（用户 ID）落到 find_by_account 永远空结果。修复：全数字 keyword
%%% 先走 find_by_id（allow_search 门在 logic 层不变，仍生效）。
%%%
%%% 形态说明：meck 单测对 user_ds mock 不生效的原因未查清前，改用真库集成
%%% （同 ZC-04 集成套件模式）——直接验证 logic 层真实路由与 allow_search 门。

-define(SETUP_TAG, <<"W2R2FIX-kw">>).

find_by_keyword_routes_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_world(),
        #{conn := Conn, uid := Uid, account := Account} = Ctx,
        try
            %% 探针：find_by_id 直查应命中（解耦数据层与路由分支）
            Probe = user_ds:find_by_id(Uid, <<"id,account">>),
            ?assertEqual(Uid, maps:get(<<"id">>, Probe, 0), "find_by_id 直查应命中"),
            %% 用例 1：纯数字 keyword 走 find_by_id 命中
            {User, Uid2, Allow1} = user_logic:find_by_keyword(integer_to_binary(Uid)),
            ?assertEqual(Uid, Uid2),
            ?assertEqual(Account, maps:get(<<"account">>, User)),
            ?assertEqual(true, Allow1, "allow_search=1 时应放行"),
            %% 用例 2：纯数字但无此用户 → 空 + Uid2=0 + 不放行
            {_U2, UidMissing, Allow2} = user_logic:find_by_keyword(<<"111111111111111">>),
            ?assertEqual(0, UidMissing),
            ?assertEqual(false, Allow2),
            %% 用例 3：account 精确匹配仍命中（不回归）
            {_U3, Uid3, _A3} = user_logic:find_by_keyword(Account),
            ?assertEqual(Uid, Uid3),
            %% 用例 4：account 前缀不命中（精确匹配语义留证，防误改模糊搜索）
            {_U4, Uid4, _A4} = user_logic:find_by_keyword(<<"at20260831">>),
            ?assertEqual(0, Uid4),
            %% 用例 5：allow_search=2 时行仍返回但 AllowSearch=false
            %%（空列表语义由 handler 据 AllowSearch 决定，见 user_handler:search/2）
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE fts_user SET allow_search = 2 WHERE user_id = $1">>,
                [Uid]
            ),
            {_U5, Uid5, Allow5} = user_logic:find_by_keyword(integer_to_binary(Uid)),
            ?assertEqual(Uid, Uid5),
            ?assertEqual(false, Allow5, "allow_search=2 应返回 AllowSearch=false")
        after
            cleanup_world(Conn, Uid)
        end
    end).

setup_world() ->
    ?debugFmt("~n[user_search_keyword] user_logic loaded from: ~p~n", [code:which(user_logic)]),
    {ok, Conn} = take_conn(),
    Uid = elib_tsid:generate(),
    Account = <<"w2r2fixkw", (integer_to_binary(Uid rem 1000000000000))/binary>>,
    PwdHash = <<"$2b$10$abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLM">>,
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO \"user\" (id, account, nickname, password, mobile, email, region,",
            " avatar, sign, gender, status, created_at, reg_ip, reg_cosv, level_id,",
            " experience, ref_user_id, ref_parent_user_id, source)",
            " VALUES ($1, $2, $3, $4, $2, '', '', '', '', 0, 1, $5, '127.0.0.1',",
            " 'w2r2fix', 1, 0, 0, 0, 'test')">>,
        [Uid, Account, <<"W2R2FIX-搜索"/utf8>>, PwdHash, elib_dt:now()]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"UPDATE fts_user SET allow_search = 1 WHERE user_id = $1">>,
        [Uid]
    ),
    #{conn => Conn, uid => Uid, account => Account}.

cleanup_world(Conn, Uid) ->
    epgsql:equery(Conn, <<"DELETE FROM fts_user WHERE user_id = $1">>, [Uid]),
    epgsql:equery(Conn, <<"DELETE FROM \"user\" WHERE id = $1">>, [Uid]),
    pooler:return_member(pgsql, Conn),
    ok.

-spec take_conn() -> {ok, pid()} | {error, term()}.
take_conn() ->
    case pooler:take_member(pgsql) of
        error_no_members ->
            timer:sleep(200),
            case pooler:take_member(pgsql) of
                error_no_members -> {error, no_connection};
                Conn -> {ok, Conn}
            end;
        Conn ->
            {ok, Conn}
    end.
