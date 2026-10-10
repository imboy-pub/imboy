# V2.2.1 W0 计划与审查报告

本轮（CACHE_V2_2_1_W0_CLOSEOUT）只完善与验收 W0；结论见 report.md，机器字段（含 W0-01～W0-10 验收 ledger 与双候选绑定）见 decision.json。
key-registry.md 为全部已识别通用缓存命名空间/动态形态的迁移策略矩阵，
key-registry.json 保留 owner、TTL、authority、敏感性、失效钩子状态（mutation_hook/sites）、源码位置与尚未闭合字段。
other-stores.json 显式列出自有 ETS/persistent_term/ACK/进程字典等非 depcache store；
unresolved-forms.json 记录动态/不可静态解析形态（forwarded_key、derived_string_key、random_binary_key、
runtime_keys_via_syn、legacy_set_broadcast、anonymous_memo_mfa），扫描器不静默遗漏。

存在两个独立候选绑定（历史与当前结果不互相冒称）：`hist-578d83ac`（轮次 1/2）与
`closeout-34858496`（轮次 3/4，2026-10-10 用户重新授权基线；TSID 提交为外来上下文，
缓存调用行零漂移）。

复跑压测（需本地 Erlang、现有 deps 的已编译 beam；不启动业务和数据库）：

```sh
cd /Users/leeyi/project/imboy.pub/imboy
python3 scripts/run_cache_w0.py --operations 10000
python3 scripts/check_cache_w0_docs.py          # 文档/证据/ledger 一致性
python3 scripts/check_cache_w0_docs.py --self-test  # 校验器 fail-closed 自检
```

压测脚本输出独立目录（建议传入仓外持久 RUN_ROOT）；最终退出 2 代表 W0_BLOCKED。
baseline 的 compile_exit/run_exit 和 performance 的 three_node.status 是独立技术结果。
文档检查退出 0 只表示交付文档/证据一致，不表示 W0/生产/UAT 通过。
plan-v2.2.1.md 是工作区 .Codex/erlang_cache_plan.md 的仓内快照，以相同 SHA256 校验。

原始执行证据按仓库规则保留在仓外持久目录 `/Users/leeyi/project/imboy.pub/.Codex/runs/cache-v2.2.1-w0/`；
decision.json 的 candidates[].runs 给出精确目录与摘要入口。
本目录只保存计划、策略矩阵与审查报告，不绕过 .gitignore 强制入仓原始日志。
不要拿 publish 返回、合成热命中、同机三节点、微秒零值或测试数量证明生产接受。
