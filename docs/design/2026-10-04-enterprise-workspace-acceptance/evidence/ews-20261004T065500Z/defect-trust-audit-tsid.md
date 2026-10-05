# 缺陷卡 EWS-BE-D1：trust_audit TSID 生成器未注册（main 既有）

- 发现：本 run 全量 eunit（R5 确定性复现；单模块同样失败）
- 严重级：P2（非企业域阻断；trust 审计/Olm 域运行时风险）
- 机械证明为 main 既有：本 run 候选的 `git diff 0be001e3 -- src/ test/` = 0 行（Erlang 源与 main 逐字节一致），行为不可能由本 run 引入。

## 事实

- 调用点：`src/logic/olm_identity_logic.erl:350` — `event_id => integer_to_binary(elib_tsid:generate(trust_audit))`
- 注册真源：`imboy_app:tsid_generator_names()`（src/imboy_app.erl:200 起）**不含** `trust_audit`
- 目录文件 `src/lib/elib_tsid_catalog.erl:283` 有 `{trust_audit, id}`，但守护测试以 `imboy_app:tsid_generator_names()` 为准（历史两次生产事故的防线，见该测试头注）

## 影响

该代码路径运行时将 `{elib_tsid_generator_not_registered, trust_audit}` 崩溃（bot_ds 同类曾进生产）。当前 olm_identity_logic 该分支在生产是否可达需 Olm/trust 域 owner 评估。

## 建议修复（交后端 owner，非本 run 范围）

`imboy_app:tsid_generator_names()` 清单补 `trust_audit`（一行）；或若 catalog 与 registration 双真源是设计缺陷，收敛为单一真源。修复后 guard 测试自然转绿。

## 证据

- /tmp/ews_full_eunit_r5.log（已归档 evidence/full-eunit/）
- /tmp/ews_arb_elib_tsid_registration_guard_tests.log
