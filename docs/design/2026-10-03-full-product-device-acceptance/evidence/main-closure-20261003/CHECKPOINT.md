# 两计划主分支整合与续跑检查点

本轮已将后端和 App 集成候选快进到 main，并额外合入原候选遗漏的 C11 旅程骨架。
所有 worker 的补丁经 git cherry 确认已等价进入 main 后，保留其提交祖先历史；仅保留历史的合并前后 tree 完全一致。
21 个相关 worktree 和 21 个分支均已删除；未提交 C12 运维手册及全产品准备产物已保存并本地提交。

公开 MLS 向量触发 generic-api-key 告警的六个文件已与 mlswg/mls-implementations 上游原文逐字节核对。仅新增具体 file/rule/line 指纹豁免，扫描门禁保留。
全产品准备 TSV 的 CRLF 已规范为 LF。新增 C11 目标导致 catalog 过时，已重新生成并复验。

此处是 PREPARATION 检查点，未完成两计划的最终验收。原 E2EE 36 ID、全产品 61 ID 以及全部 required case/platform 集合仍是原合同；本地编译和域回归不得代替真库、真机、跨端、独立安全复核或用户 UX 评审。
逐项检查、候选基线、下一步与所需决策见 continuation-state.json。

原始 run-20261003-094804 账本和证据保持历史快照，不把它们重签为新 main 的 PASS。

## 客服治理本地竞态修复

App c9d70843 修复旧组织读取和写入结果晚到覆盖新组织额度，以及刷新时旧游标分页。8ffa8c41 完成测试格式检查。保留三个修改前真实失败日志及28项本地回归成功日志；最初两次未触达竞态的异步屏障失败单独标记 INVALID_TEST_SETUP，不作为业务缺陷证明。当前静态分析无问题，独立 Flutter 评审通过。

上述测试使用内存路由，不证明真实后端、Android/macOS、F12整卡或全产品验收通过；原97项最终验收状态不变。详见 cs-governance-local-closure.json。
