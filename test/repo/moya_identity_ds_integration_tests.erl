%% moya_identity_ds_integration_tests
%% 首登自动开户事务编排层真库集成测试（2026-09-20 试点方案 B）。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 MOYA_ITID，全链迁移至
%% 当前 head），每用例 BEGIN ... ROLLBACK，不留数据（模式照
%% moya_learner_bind_integration_tests）。
%%
%% 为什么直调 _in_tx 而不是 provision_and_bind/3：
%%   provision_and_bind/3 走**全局连接池**（elib_pg:with_tx），而 marker 库
%%   夹具只提供一条独立连接、不重定向池 —— 从池里发语句会落到常规本地库而
%%   不是 marker 库。这正是 _in_tx/4 被导出的原因（与生产**同一段代码**，
%%   差别只在谁开事务）。
%%
%% 覆盖：
%%   ① 正向    —— user 行 + sso_identity 行的列值都对（account/source/reg_ip/
%%                 status/随机口令）
%%   ② 幂等    —— 同 subject 二次调用返回同一 uid，不再建 user 行
%%   ③ 隔离    —— 两个 subject 各自独立建号
%%   ④ uk_account —— 同 account 号给两个 subject：第二个必须被 PG 拒（23505）
%%                并经 create_user_and_bind 折叠为 account_conflict
%%   ⑤ 原子性  —— 绑身份失败时**已经写进去的 user 行必须消失**（无孤儿账号）。
%%                用 SAVEPOINT 正面验证：这是本模块存在的主要理由，若只有
%%                「不报错」而没验回滚，等于没验。
%%
%% 供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。

-module(moya_identity_ds_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PROVIDER, <<"wechat_mini">>).
-define(SUBJECT_1, <<"oMOYA_IT_openid_0001">>).
-define(SUBJECT_2, <<"oMOYA_IT_openid_0002">>).
-define(ACCOUNT_1, 700011).
-define(ACCOUNT_2, 700012).
-define(ACCOUNT_DUP, 700013).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    %% 命名生成器必须显式注册：生产由应用启动流程统一注册（并有
    %% elib_tsid_registration_guard_tests 兜底），而单跑本套件的 VM 不带那一步，
    %% 否则 user_repo:create_tx（generate(user)）与 sso_identity_repo:upsert_tx
    %% （generate(sso_identity)）会抛 elib_tsid_generator_not_registered。
    %% register/1 幂等，重复调用安全。
    ok = elib_tsid:register([user, sso_identity]),
    %% ⚠ marker 库夹具默认**不**装 rfc3339 codec，而本套件要验证的写路径会绑定
    %% elib_dt:now() 的产物（RFC3339 字符串，如 2026-09-21T17:41:47.976250+08:00）。
    %% 缺 codec 时 epgsql 会拿默认 datetime 编解码去处理该字符串，
    %% 崩在 epgsql_idatetime:timestamp2i/1（function_clause），整个套件 cancelled。
    %% 只做裸 SQL 断言的既有套件（如 moya_learner_bind）不传也能过，故此处必须显式传。
    inttest_marker_db:provision(#{
        env_prefix => <<"MOYA_ITID">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

with_tx(TestFun) ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            ?_test(begin
                ok = exec(C, <<"BEGIN">>),
                try
                    TestFun(C),
                    ok
                after
                    exec(C, <<"ROLLBACK">>)
                end
            end)
        end}}.

exec(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

q(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

%%%===================================================================
%%% ① 正向：user + sso_identity 同事务落库，列值正确
%%%===================================================================

provision_creates_user_and_mapping_test_() ->
    with_tx(fun(C) ->
        {ok, Uid} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_1, #{
            account => ?ACCOUNT_1,
            ip => <<"203.0.113.9">>
        }),
        ?assert(is_integer(Uid) andalso Uid > 0),
        %% user 行：account 真的是分配号、来源可追溯、reg_ip 是真请求 IP、status 正常
        [
            #{
                <<"account">> := <<"700011">>,
                <<"source">> := <<"moya_wechat_mini">>,
                <<"reg_ip">> := <<"203.0.113.9">>,
                <<"status">> := 1,
                <<"password">> := Pwd
            }
        ] = q(
            C,
            <<
                "SELECT account, source, reg_ip, status, password FROM \"user\" WHERE id = $1"
            >>,
            [Uid]
        ),
        %% SSO 用户不走密码登录：绝不落 "password123" 这类兜底口令
        ?assertNotEqual(<<"password123">>, Pwd),
        ?assert(byte_size(Pwd) >= 32),
        %% sso_identity 行：provider/subject/uid 三角对上，email 不占位
        [#{<<"uid">> := Uid, <<"email">> := <<>>}] = q(
            C,
            <<
                "SELECT uid, email FROM sso_identity WHERE provider = $1 AND subject = $2"
            >>,
            [?PROVIDER, ?SUBJECT_1]
        )
    end).

%%%===================================================================
%%% ② 幂等：同 subject 二次调用复用既有 uid，不再建号
%%%===================================================================

provision_is_idempotent_test_() ->
    with_tx(fun(C) ->
        {ok, Uid1} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_1, #{
            account => ?ACCOUNT_1
        }),
        %% 第二次故意给**另一个** account 号：若实现漏了复查就会用它建出第二行，
        %% 断言同时覆盖「返回同一个 uid」与「没用掉新号」
        {ok, Uid2} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_1, #{
            account => ?ACCOUNT_2
        }),
        ?assertEqual(Uid1, Uid2),
        [#{<<"c">> := 1}] = q(
            C,
            <<"SELECT count(*) AS c FROM sso_identity WHERE provider = $1 AND subject = $2">>,
            [?PROVIDER, ?SUBJECT_1]
        ),
        [#{<<"c">> := 0}] = q(
            C, <<"SELECT count(*) AS c FROM \"user\" WHERE account = $1">>, [<<"700012">>]
        )
    end).

%%%===================================================================
%%% ③ 不同 subject 各自独立建号
%%%===================================================================

distinct_subjects_get_distinct_users_test_() ->
    with_tx(fun(C) ->
        {ok, Uid1} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_1, #{
            account => ?ACCOUNT_1
        }),
        {ok, Uid2} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_2, #{
            account => ?ACCOUNT_2
        }),
        ?assertNotEqual(Uid1, Uid2),
        [#{<<"c">> := 2}] = q(C, <<"SELECT count(*) AS c FROM sso_identity">>, [])
    end).

%%%===================================================================
%%% ④ uk_account：同 account 号给两个 subject，第二个必须被 PG 拒
%%%===================================================================

account_uniqueness_enforced_test_() ->
    with_tx(fun(C) ->
        {ok, _} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_1, #{
            account => ?ACCOUNT_DUP
        }),
        %% 23505 会被折叠为 account_conflict（上层据此让家长重试换号）
        ?assertThrow(
            {rollback, account_conflict},
            moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_2, #{
                account => ?ACCOUNT_DUP
            })
        )
    %% 注：23505 后本事务已 abort，故断言到此为止（同仓 moya_learner_bind 同款规避）
    end).

%%%===================================================================
%%% ⑤ 原子性：绑身份失败 ⇒ 已写入的 user 行必须回滚消失（无孤儿账号）
%%%
%%% 制造绑定失败的手段：subject 超过 varchar(255) ⇒ INSERT 报 22001。
%%% 用 SAVEPOINT 包住，回滚到保存点后外层事务仍可继续查询（规避 25P02）。
%%%===================================================================

bind_failure_rolls_back_user_row_test_() ->
    with_tx(fun(C) ->
        TooLong = binary:copy(<<"x">>, 300),
        ok = exec(C, <<"SAVEPOINT sp_bindfail">>),
        ?assertThrow(
            {rollback, db_error},
            moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, TooLong, #{
                account => ?ACCOUNT_1
            })
        ),
        ok = exec(C, <<"ROLLBACK TO SAVEPOINT sp_bindfail">>),
        %% 关键断言：建号已执行过、绑定失败 ⇒ 整笔必须回滚，不能留下孤儿 user
        [#{<<"c">> := 0}] = q(
            C, <<"SELECT count(*) AS c FROM \"user\" WHERE account = $1">>, [<<"700011">>]
        ),
        [#{<<"c">> := 0}] = q(C, <<"SELECT count(*) AS c FROM sso_identity">>, [])
    end).

%%%===================================================================
%%% 附带：advisory 锁路径在同事务内可用（xact 锁随事务结束自动释放）
%%%===================================================================

advisory_lock_runs_in_tx_test_() ->
    with_tx(fun(C) ->
        %% 同 subject 连开两次锁（本事务内可重入），不得报错
        ok = exec(C, <<"SELECT pg_advisory_xact_lock(hashtext($1))">>, [?SUBJECT_1]),
        ok = exec(C, <<"SELECT pg_advisory_xact_lock(hashtext($1))">>, [?SUBJECT_1]),
        {ok, _} = moya_identity_ds:provision_and_bind_in_tx(C, ?PROVIDER, ?SUBJECT_1, #{
            account => ?ACCOUNT_1
        })
    end).

%% 带参数的 exec（上面两个用例需要）
exec(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.
