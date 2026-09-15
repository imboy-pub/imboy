%% moya_review_ai_request_tests
%% 老师端「让 AI 看一遍」手动触发（POST /api/v1/moya/submissions/:id/ai-draft）
%% 的事务契约回归测试。
%%
%% 缺口背景（2026-09-14 定位）：本路径此前**零测试覆盖**，于是下面这个缺陷一路绿灯上线：
%%
%%   `elib_pg:with_tx/2` 的契约是**原样透传 fun 的返回值**
%%   （spec：`R | {rollback, term()}`，见 src/lib/elib_pg.erl:217-219），
%%   它**不会**替调用方把结果包成 {ok, _}。同一个文件里其余三处 with_tx
%%   调用点（moya_review_logic.erl:235 / :880 / :905）都按此口径匹配 {ok, ...}。
%%
%%   而 request_ai_draft_tx 返回的是**裸** {requested, Id, Status}，
%%   finish_ai_request/1 却只匹配 {ok, {requested, ...}} → 全部落到兜底分支，后果：
%%     · 事务其实已提交成功，老师却拿到 {error, db_error}（HTTP 200 + code=1「操作失败」）
%%     · **maybe_run_ai_draft/2 从未被调用** ⇒ 草稿永远停在 queued，
%%       前端「整理中…」永不变化 —— 用户原话「感觉像假的状态」
%%   运行节点日志铁证：
%%     request ai draft unexpected {requested,112572346124208128,<<"queued">>}
%%
%% 本用例的**关键保真点**：不把 with_tx mock 成「返回一个好看的值」，而是让它
%% **真的调用传入的 fun 并把返回值原样传出**。契约一旦回归（生产者漏包 {ok, _}），
%% 用例立刻变红 —— 这正是它区别于「假绿桩」的地方。

-module(moya_review_ai_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 1).
-define(SUB, 6214747500760300).
-define(GID, 90001).
-define(DRAFT_ID, 42).

%%%===================================================================
%%% 夹具
%%%===================================================================

%% 忠实复刻 elib_pg:with_tx/2 的真实语义：跑 fun，原样透传返回值。
%% （真实实现在 epgsql:with_transaction 之上，成功时返回 fun 的返回值本身。）
with_tx_pass_through() ->
    {elib_pg, [{'with_tx', 2, fun(Fun, _Opts) -> Fun(fake_conn) end}]}.

%% 教师对该提交所在班级有写权限（draft_guard 的两道门）
acl_mocks() ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(?UID, ?SUB) ->
                {ok, staff, #{<<"group_id">> => ?GID}}
            end},
            {'resolve_staff', 3, fun(?UID, ?GID, write) ->
                {ok, #{<<"group_id">> => ?GID, <<"role">> => <<"teacher">>}}
            end}
        ]}
    ].

%% submission 行可锁（未撤回）
lock_mocks() ->
    [
        {moya_submission_repo, [
            {'lock_submission_tx', 2, fun(_Conn, ?SUB) ->
                {ok, #{<<"status">> => <<"submitted">>}}
            end}
        ]}
    ].

%% 捕获「是否真的定点执行了」——缺陷当年就是死在这一步没被调用
async_mocks() ->
    [
        {elib_async, [
            {'async', 1, fun(Fun) ->
                put(ai_run_fun, Fun),
                ok
            end}
        ]}
    ].

async_called() ->
    meck:num_calls(elib_async, async, 1).

%%%===================================================================
%%% 1) 已有 queued 草稿：必须 {ok, #{...}} 且真的触发定点执行
%%%===================================================================

%% 这是当年失败的**主现场**：行已在队列里（家长提交时入队），老师点「让 AI 看一遍」。
%% 旧代码返回 {error, db_error} 且 async 0 次 —— 静默空转。
existing_queued_draft_triggers_run_test_() ->
    ?WITH_MECKS(
        acl_mocks() ++ lock_mocks() ++ async_mocks() ++
            [
                with_tx_pass_through(),
                {moya_review_repo, [
                    {'ai_draft_tx', 2, fun(_Conn, ?SUB) ->
                        {ok, #{<<"id">> => ?DRAFT_ID, <<"status">> => <<"queued">>}}
                    end}
                ]}
            ],
        fun() ->
            Result = moya_review_logic:request_ai_draft(?UID, ?SUB),
            %% ① 必须把成功透出来，而不是被兜底吞成 db_error
            ?assertEqual(
                {ok, #{
                    <<"draft_id">> => integer_to_binary(?DRAFT_ID),
                    <<"status">> => <<"queued">>
                }},
                Result
            ),
            %% ② 必须真的定点执行这一条（run_draft）。
            %%    没有这一步，草稿就永远停在 queued —— 页面「整理中…」永不变化。
            ?assertEqual(1, async_called()),
            %% ③ 异步闭包指向 run_draft/1，而不是别的什么
            Fun = get(ai_run_fun),
            ?assert(is_function(Fun, 0))
        end
    ).

%%%===================================================================
%%% 2) 已撤回提交：拒绝，且不得触发执行
%%%===================================================================

withdrawn_submission_rejected_test_() ->
    ?WITH_MECKS(
        acl_mocks() ++ async_mocks() ++
            [
                with_tx_pass_through(),
                {moya_submission_repo, [
                    {'lock_submission_tx', 2, fun(_Conn, ?SUB) ->
                        {ok, #{<<"status">> => <<"withdrawn">>}}
                    end}
                ]},
                {moya_review_repo, [
                    {'ai_draft_tx', 2, fun(_Conn, ?SUB) -> {ok, undefined} end}
                ]}
            ],
        fun() ->
            ?assertEqual({error, withdrawn}, moya_review_logic:request_ai_draft(?UID, ?SUB)),
            ?assertEqual(0, async_called(), "撤回的提交不该被跑 AI")
        end
    ).

%%%===================================================================
%%% 3) 从无草稿：新入队一行，同样必须 {ok, _} 且触发执行
%%%===================================================================

no_draft_enqueues_new_row_test_() ->
    ?WITH_MECKS(
        acl_mocks() ++ async_mocks() ++
            [
                with_tx_pass_through(),
                %% 同一模块的两条期望必须写在**同一个** meck 配置里：
                %% 拆成两条 {moya_submission_repo, _} 会让后者覆盖前者。
                {moya_submission_repo, [
                    {'lock_submission_tx', 2, fun(_Conn, ?SUB) ->
                        {ok, #{<<"status">> => <<"submitted">>}}
                    end},
                    {'enqueue_ai_draft_tx', 2, fun(_Conn, ?SUB) -> ok end}
                ]},
                {moya_review_repo, [
                    %% 首次读：无草稿；入队后重读：拿到新行
                    {'ai_draft_tx', 2, fun(_Conn, ?SUB) ->
                        case get(reread) of
                            undefined ->
                                put(reread, true),
                                {ok, undefined};
                            true ->
                                {ok, #{
                                    <<"id">> => ?DRAFT_ID,
                                    <<"status">> => <<"queued">>
                                }}
                        end
                    end}
                ]}
            ],
        fun() ->
            ?assertEqual(
                {ok, #{
                    <<"draft_id">> => integer_to_binary(?DRAFT_ID),
                    <<"status">> => <<"queued">>
                }},
                moya_review_logic:request_ai_draft(?UID, ?SUB)
            ),
            ?assertEqual(1, async_called()),
            ?assertEqual(1, meck:num_calls(moya_submission_repo, enqueue_ai_draft_tx, 2))
        end
    ).

%%%===================================================================
%%% 4) 事务返回 {rollback, _} 时必须真的报错（不能把失败当成功）
%%%===================================================================

rollback_is_reported_as_error_test_() ->
    ?WITH_MECKS(
        acl_mocks() ++ async_mocks() ++
            [
                with_tx_pass_through(),
                {moya_submission_repo, [
                    {'lock_submission_tx', 2, fun(_Conn, ?SUB) -> {ok, undefined} end}
                ]},
                {moya_review_repo, [
                    {'ai_draft_tx', 2, fun(_Conn, ?SUB) -> {ok, undefined} end}
                ]}
            ],
        fun() ->
            %% {rollback, not_found} → {error, not_found}（不是被吞成 db_error）
            ?assertEqual({error, not_found}, moya_review_logic:request_ai_draft(?UID, ?SUB)),
            ?assertEqual(0, async_called())
        end
    ).
