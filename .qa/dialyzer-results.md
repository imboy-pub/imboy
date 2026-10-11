# Dialyzer Nightly result contract / Dialyzer Nightly 结果契约

Three separate results / 三层结果：

1. `make dialyze` 原始输出写入 `dialyze-last.log`，包装的 make 退出码由
   `DIALYZER_ANALYSIS_EXIT` 记录。make 的 2 不等同于分析器自身退出码，不能据此
   区分告警与执行失败。Raw make output and exit status are diagnostic evidence.
2. `check_dialyzer_baseline.sh` 校验分析段完整性和支持的格式，再按现有指纹规则
   比较 `dialyzer.baseline`。新增返回 1，完整且无新增返回 0，不完整/格式漂移返回 3。
   Baseline comparison remains fingerprint-level; no budget or baseline expansion.
3. `classify.sh dialyze` 只消费基线裁决，不读取模块级 known 清单，不解析原始告警。
   The classifier consumes the baseline decision, not raw warning text.

机器协议是 stdout 上的 tab 分隔记录（下文 `<TAB>` 表示实际制表符）：

```text
DIALYZER_RESULT<TAB>1<TAB>BEGIN
DIALYZER_RESULT<TAB>1<TAB>NEW<TAB>filename.erl|normalized warning
DIALYZER_RESULT<TAB>1<TAB>KNOWN<TAB>filename.erl|normalized warning
DIALYZER_RESULT<TAB>1<TAB>FIXED<TAB>filename.erl|normalized warning
DIALYZER_RESULT<TAB>1<TAB>END<TAB>new-count<TAB>known-count<TAB>fixed-count
```

只有一个完整 frame；记录不截断，集合互斥且无重复；结束计数必须与明细相等。
人类 NEW/GONE 预览的 50/20 行上限不作用于机器记录。
One complete frame, disjoint unique fingerprints, exact trailer counts; no preview limits.

无法比较时只输出 `DIALYZER_RESULT<TAB>1<TAB>ERROR<TAB>PARSE_ERROR` 或
`INFRA_FAILURE`。INFRA 使用既有保守环境特征，不代表环境根因已被证明。
缺失/未知版本/截断/重复/计数不符及与 gate 退出状态矛盾均为 PARSE_ERROR，FIXED=0。
Only supported infrastructure signatures permit INFRA attribution; incomplete results
never establish FIXED. Raw logs are available for manual diagnosis.

正常映射：NEW>0 → NEW_FAILURE；否则 FIXED>0 → RATCHET；否则 PASS。
KNOWN 是仍存在的历史指纹，FIXED 是已消失指纹，不表示业务缺陷已修复。
重试仍失败以第二次分类为准；重试成功必须有完整 PASS/RATCHET frame。
首跑有效 NEW 可转为 FLAKY（仅首跑 NEW 数，不把历史 KNOWN 算成 flaky），FIXED=0；
首跑解析失败仍为 PARSE_ERROR，首跑基础设施失败仍为 INFRA_FAILURE。
持续 INFRA 虽然 classifier 返回 0，nightly 包装仍保留失败退出码。
Retries cannot turn incomplete analysis into a successful quality result.

Artifacts: `dialyze.log[.retry]` 保存基线记录，`dialyze.log[.retry].raw` 保存各次原始分析；
`summary`/`new_items` 供 Nightly 汇总，告警正文通过环境变量传入 issue 脚本。

回归 / Regression: `bash scripts/test/dialyzer_nightly_test.sh`。
这些是传递契约测试，不能替代真实 Dialyzer 分析或数据库事务验收。
