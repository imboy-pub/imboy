-module(enterprise_oa_sso_tests).
%% EPGZ-05 W1 RED 测试骨架（OA one-time SSO 合同负例矩阵）。
%%
%% 权威合同：docs/architecture/2026-09-21-epgz05-oa-sso-contract.md
%% 上游：plan-gz §7.2 / §6 INT-14 / §4 + control/internal-api-manifest.yaml
%%
%% 纪律：
%%   * 本文件在 W1 全部为 {skip, "RED pending EPGZ-05 impl (<NEG-id>)"} 占位——
%%     生产模块（oa_sso_handler / enterprise_oa_sso_logic 等）尚未实现，
%%     不允许为了让用例变绿写任何假实现。
%%   * skip 形态跟随仓内既定惯例（test/integration/*_tests.erl 的
%%     {"desc", fun() -> {skip, Reason} end}）。注意：本仓 eunit 2.11 无通用
%%     skip 原语，该返回值不会计入 Skipped 计数——因此额外提供
%%     red_canary_impl_absent_test_/0 哨兵：只要实现模块尚未落地，哨兵为真；
%%     W2 翻绿时哨兵必然翻转，迫使其与占位一起清理，防止占位被误读为真绿。
%%   * W2 实现落地后：逐条删除 ?RED_PENDING、按注释中的断言草案接真断言
%%     （fixture 口径见 e2ee_safety_contract_tests / ?TEST_WITH_DB_TIMEOUT）。
%%     合同 §8 明确：骨架中出现的函数签名是建议形态、非冻结项，可按
%%     Handler→Logic→DS→Repo 分层惯例调整；但 §6 期望值（错误码/状态机/红线）
%%     不可调。
%%   * 每个用例的 skip 标注都携带合同负例编号（NEG-*），与合同 §6 一一对应。
%%     翻绿进度盘点：grep -o '?RED_PENDING[(]' 本文件 | wc -l，W1 基线 = 26
%%     处占位 + 1 处哨兵（注释里一律用 [(] 代替裸左括号书写该宏名，避免
%%     盘点命令自匹配）。

-include_lib("eunit/include/eunit.hrl").

%% W1 RED 占位宏：唯一允许的「未实现」表达形态。
%% Clause = 合同负例编号（如 "NEG-01"）；Desc = 一句话场景描述。
-define(RED_PENDING(Clause, Desc),
    {Desc ++ " [RED pending EPGZ-05 impl (" ++ Clause ++ ")]",
        fun() -> {skip, "RED pending EPGZ-05 impl (" ++ Clause ++ ")"} end}
).

%% ===================================================================
%% RED 哨兵（W2 翻绿时必须随占位一并删除或反转）
%% ===================================================================

%% W1 时点实现模块必然缺席（src/ 零 oa/sso 生产代码）；若实现已落地而占位
%% 未清理，本哨兵翻转失败。模块名 enterprise_oa_sso_logic 是合同 §8 的建议
%% 形态——W2 若改名，请同步更新哨兵目标名。
red_canary_impl_absent_test_() ->
    [{"RED 哨兵：enterprise_oa_sso_logic 尚未落地（占位仍在生效）",
        fun() -> ?assertEqual(non_existing, code:which(enterprise_oa_sso_logic)) end}].

%% ===================================================================
%% INT-14 面：POST /api/internal/v1/oa/sso/exchange（合同 §4/§6）
%% ===================================================================

%% NEG-01：未知 code（伪造 256-bit 随机）→ resource_not_found（统一不透明拒绝，合同 §4.4/§7）。
neg01_exchange_unknown_code_test_() ->
    ?RED_PENDING("NEG-01", "exchange 未知 code 被拒").
%% W2 断言草案：
%%   {error, resource_not_found} =
%%       enterprise_oa_sso_logic:exchange(FakeCode, RedirectUri, Nonce, CallerCtx).
%%   （HTTP 层另断言 internal stable 码 resource_not_found，信封形态以 A2 冻结为准。）

%% NEG-02：code 签发后 >60s 才 exchange → resource_not_found；TTL 固定 60s 不可放大
%% （合同 §3.3 expires_in / §5 expired 派生态）。
neg02_exchange_expired_code_test_() ->
    ?RED_PENDING("NEG-02", "exchange 过期 code（>60s）被拒").
%% W2 断言草案：签发后将时钟推进 61s（meck elib_dt 口径），exchange 必须
%%   {error, resource_not_found}。

%% NEG-03：同 code 第二次 exchange（重放）→ resource_not_found 且不回放首次成功响应
%% （合同 §4.1 single_use_code 幂等豁免 / §6 NEG-03）。
neg03_exchange_replay_rejected_test_() ->
    ?RED_PENDING("NEG-03", "exchange 重放被拒且不回放原响应").

%% NEG-04：同 code 并发双 exchange 竞态 → 恰好 1 成功 1 拒绝；无双重成功
%% （合同 §5 CAS：WHERE code_digest=$1 AND consumed_at IS NULL AND expires_at>now()，
%%  影响行数==1 才算赢）。
neg04_exchange_concurrent_cas_single_winner_test_() ->
    ?RED_PENDING("NEG-04", "并发双 exchange 恰好一个赢家").
%% W2 断言草案：?TEST_WITH_DB_TIMEOUT 真库并发口径，两进程并发 exchange 同一
%%   code，收集结果断言 [{ok,_},{error,resource_not_found}] 恰好其一成功。

%% NEG-05：跨 org——org B credential 换 org A 签发的 code → resource_not_found
%% （统一隐藏，不暴露 organization_boundary_violation，合同 §4.4/§7）。
neg05_exchange_cross_org_test_() ->
    ?RED_PENDING("NEG-05", "exchange 跨 org code 被拒").

%% NEG-06：跨 app 同 org——app B credential 换 app A code → resource_not_found。
neg06_exchange_cross_application_test_() ->
    ?RED_PENDING("NEG-06", "exchange 跨 app（同 org）code 被拒").

%% NEG-07：redirect_uri 不匹配（尾斜杠/大小写/query/子路径差异）→ resource_not_found
%% （exchange 侧 exact-match；签发侧 allowlist 同源，合同 §3.2/§4.2）。
neg07_exchange_redirect_uri_mismatch_test_() ->
    ?RED_PENDING("NEG-07", "exchange redirect_uri 不匹配被拒").

%% NEG-08：nonce 不匹配（digest 不等）→ resource_not_found
%% （nonce 只存 digest + constant-time 比较，合同 §2/§4.4）。
neg08_exchange_nonce_mismatch_test_() ->
    ?RED_PENDING("NEG-08", "exchange nonce 不匹配被拒").

%% NEG-09：credential 缺 sso:exchange scope → insufficient_scope（A2 认证链，合同 §4.1）。
neg09_exchange_insufficient_scope_test_() ->
    ?RED_PENDING("NEG-09", "exchange 缺 sso:exchange scope 被拒").

%% NEG-10：credential 无效/过期、application/organization 停用 →
%% invalid_credential / credential_expired / application_disabled / organization_disabled
%% （A2 链，SSO 不豁免；本用例在 SSO 层验证链路透传不吞错，合同 §4.1/§7）。
neg10_exchange_credential_chain_errors_test_() ->
    ?RED_PENDING("NEG-10", "exchange 走 A2 认证链四类错误透传").

%% NEG-11：绑定五元组全过但 mapping 缺失 / user 非 active（签发后竞态）→
%% identity_not_mapped，且 code 不被消费（CAS+解析同事务整体回滚，合同 §4.4/§5）。
neg11_exchange_identity_not_mapped_keeps_code_issued_test_() ->
    ?RED_PENDING("NEG-11", "identity_not_mapped 且 code 不被消费").
%% W2 断言草案：删除 mapping 后 exchange 得 {error, identity_not_mapped}；
%%   恢复 mapping + TTL 内重试可成功，证明 code 停留 issued 而非被消费。

%% NEG-12：请求字段语法非法（code 前缀/长度/字符集；redirect_uri 非 HTTPS/fragment；
%% nonce 长度）→ invalid_request（语法与语义分离：语法 invalid_request、绑定
%% resource_not_found，合同 §4.2/§4.4）。
neg12_exchange_malformed_request_test_() ->
    ?RED_PENDING("NEG-12", "exchange 语法非法请求 invalid_request").

%% NEG-13：internal_sso 限流配置缺失 → fail-closed 拒绝（INV-9 / security_gate_closed，
%% 合同 §4.1/§7）。
neg13_exchange_rate_limit_fail_closed_test_() ->
    ?RED_PENDING("NEG-13", "internal_sso 限流配置缺失 fail-closed").

%% NEG-14：exchange 成功响应不得携带任何 IMBoy token/JWT/session（合同 §4.3 R-6）。
neg14_exchange_response_no_imboy_credential_test_() ->
    ?RED_PENDING("NEG-14", "exchange 响应无任何 IMBoy 凭证").
%% W2 断言草案：成功 payload 键集合 == {organization_id, application_id, user_id,
%%   external_user_id, consumed_at}；TSID 字段为 JSON integer（INV-8）。

%% NEG-15：code/nonce 明文零落日志、零审计（红线，合同 §0/§2）。
%% 口径对齐 e2ee_safety_contract_tests：哨兵字符串 + lager 全量捕获无命中。
neg15_code_and_nonce_plaintext_never_logged_test_() ->
    ?RED_PENDING("NEG-15", "code/nonce 明文零落日志与审计").

%% ===================================================================
%% HUMAN-SSO-01 面：POST /api/v1/oa/sso/code（合同 §3/§6）
%% ===================================================================

%% NEG-H01：无 JWT / JWT 无效 → 401（现有 /api/v1 中间件链，合同 §3.1）。
neg_h01_issue_without_human_jwt_test_() ->
    ?RED_PENDING("NEG-H01", "签发缺 Human JWT 401").

%% NEG-H02：application_key 未知或 application 停用 → 404 ?ERR_NOT_FOUND
%% （human 面整数信封，合同 §3.5）。
neg_h02_issue_unknown_application_test_() ->
    ?RED_PENDING("NEG-H02", "签发未知/停用 application 404").

%% NEG-H03：请求者非目标 org active 成员 → 403 ?ERR_FORBIDDEN。
neg_h03_issue_not_active_member_test_() ->
    ?RED_PENDING("NEG-H03", "签发非目标 org active 成员 403").

%% NEG-H04：无 active identity mapping → 403 ?ERR_FORBIDDEN（fail early，
%% 签发即拦截，exchange 侧 identity_not_mapped 只兜底竞态，合同 §3.4/§3.5）。
neg_h04_issue_without_identity_mapping_test_() ->
    ?RED_PENDING("NEG-H04", "签发无 identity mapping 403 fail-early").

%% NEG-H05：redirect_uri 未注册/非 HTTPS/带 fragment/exact 不匹配 →
%% 400 ?ERR_INVALID_PARAM（exact-match allowlist，合同 §3.2/§3.4/§8-A1）。
neg_h05_issue_redirect_uri_not_allowlisted_test_() ->
    ?RED_PENDING("NEG-H05", "签发 redirect_uri 不在 allowlist 400").

%% NEG-H06：nonce 格式非法（长度/字符集）、body 不可解析 → 400 ?ERR_INVALID_PARAM。
neg_h06_issue_malformed_request_test_() ->
    ?RED_PENDING("NEG-H06", "签发 nonce/body 格式非法 400").

%% NEG-H07：同用户连续签发两个 code → 各自独立 TTL + 单次消费、互不失效；
%% expires_in 恒 60（合同 §3.3/§3.4/§5）。
neg_h07_issue_repeated_codes_independent_test_() ->
    ?RED_PENDING("NEG-H07", "重复签发 code 相互独立且 TTL 恒 60").

%% ===================================================================
%% 双向隔离（manifest INV-2/INV-3，合同 §3.1/§1）
%% ===================================================================

%% NEG-X01：Application Credential 调 /api/v1/oa/sso/code → 401/403。
neg_x01_application_credential_cannot_issue_test_() ->
    ?RED_PENDING("NEG-X01", "Application Credential 不能调签发端点").

%% NEG-X02：Human JWT 调 /api/internal/v1/oa/sso/exchange → 401/403。
neg_x02_human_jwt_cannot_exchange_test_() ->
    ?RED_PENDING("NEG-X02", "Human JWT 不能调 exchange 端点").

%% ===================================================================
%% 存储合同（合同 §2：digest-only）
%% ===================================================================

%% STORAGE-01：code/nonce 只存 SHA-256 digest（code_digest 唯一 + nonce_digest），
%% 明文零落库。形态对齐 bot_repo:digest_hex/1 先例。
storage01_digest_only_persistence_test_() ->
    ?RED_PENDING("STORAGE-01", "code/nonce 只存 digest").
%% W2 断言草案：签发后直查 enterprise_oa_sso_code 行，
%%   digest 均为 64 字符 hex（SHA-256），code 明文不出现在行任何列。

%% STORAGE-02：签发响应明文 code 仅出现一次；expires_in == 60；redirect_uri 原样回显。
storage02_issue_response_contract_test_() ->
    ?RED_PENDING("STORAGE-02", "签发响应字段合同（code 一次性/expires_in=60/回显）").
