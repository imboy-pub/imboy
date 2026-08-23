# 质量债务总账 / Quality Debt Ledger（Golden Gates §6.2 P4-Q2）

<!--
维护机制（谁/何时快照）：
- 快照频率：每周一行（建议每周五，nightly qa-issue job 只在有 NEW 时提醒，
  周快照由人工执行——各字段来源分散在后端 CI / imboyapp analyze / imboyadmin
  lint / golden 脚本，暂无单点自动汇总）。
- 数据来源：
    contract violations   contract-gate.yml / imboyapp contract.yml 结果数
    fast gate failures    backend-ci.yml + quality.yml 当周失败 job 数（GitHub Actions）
    nightly new           nightly.yml qa-issue job 的 total_new（QA_CLASSIFY 汇总行）
    elvis baseline        quality.yml erlang-lint 实测值（violations / FAIL 文件）
    app warnings          imboyapp `flutter analyze`（warning + info 分开记）
    untested modules      src 模块无同名 *_tests 计数（§6.3 口径）
    golden install 耗时   release.yml golden-install-nightly（≤900s 门禁）实测秒数
    golden upgrade        release.yml golden-upgrade PASS/FAIL（及耗时）
- FIXED 棘轮收紧时同步更新：.qa/known-failures.yaml 人工移除已知失败项后，
  在最近快照行的备注或下一行记录收紧量（如 elvis known 308→290）。
- 规则：数字只准向好或持平解释；恶化必须在新行如实记录，不覆盖历史行。
-->

## 周快照

| 周次 | contract violations | fast gate failures | nightly new | elvis baseline | app warnings | untested modules | golden install 耗时 | golden upgrade |
|---|---|---|---|---|---|---|---|---|
| 2026-08-22（基线 §6.3） | —（P3-C1/C2 未建，待 contract gate 首跑） | —（基线时无 CI 数据，GitHub Actions 2026-08-02 才首跑） | —（nightly.yml 当日建成，首跑未发生） | 10352 violations / 298 FAIL 文件 | 830 warn + 703 info | 148/509（29%） | —（待 CI 首跑；golden-install-nightly vars 默认关） | —（待首次 release CI 实测） |

## 备注（基线日的补充实测，P4-Q1 盘点 2026-08-22）

- elvis：同日本机实测（elvis 5.0.4 homebrew）已达 **10397 violations / 308 FAIL 文件**，
  超 §6.3 基线（10352/298）——即 quality.yml 的 PR 数字 ratchet 当前应为红。
  known-failures.yaml 首版按实测 308 文件入册；quality.yml 上限是否随实测更新属
  棘轮放宽决策，留人工裁定（P4-Q1 报告已列）。
- gradualizer：全仓 audit 实测 507 模块 failing **277**，超 §6.3 基线 247（入仓
  .gradualizer-baseline.txt 为 2026-07-25 重建）——一个月净增 30（+31 新增 / -1 修复）。
  known 首版按实测 277 入册，差异明细见 .qa/known-failures.yaml gradualizer 段注释。
- eqwalizer：lib 层实测 75 模块 failing **31**（预算 32 内；§6.3 时为 failing≈30、
  模块 70——lib 层期间扩了 5 个模块）。known 首版按实测 31 入册。
- dialyze / full-eunit：本机无 PLT / 45min 超出实测边界，known 留空待首跑 nightly
  盘点回填（首跑红属预期流程，见 nightly.yml 头注释 SOP）。
