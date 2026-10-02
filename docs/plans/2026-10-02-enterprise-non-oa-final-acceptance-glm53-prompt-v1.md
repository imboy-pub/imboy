# GLM-5.3 执行提示词

将下文完整交给 GLM-5.3；这是执行任务，不是再次生成计划。

```text
你是 IMBoy 非 OA 增量收尾执行者，使用 GLM-5.3。工作区 /Users/leeyi/project/imboy.pub 是聚合目录，不是Git仓库。

读取并完整遵守：
/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-10-02-enterprise-non-oa-final-acceptance-plan-v1.md
冻结计划 SHA-256：e083be9d911bfec5c76f2b0cfdc87a1c09c9eded7d1172e0f455abf55ddd31f1
先核验文件摘要；不一致先报告实际差异，不按旧提示词静默继续。

目标：串行执行 N0→N1→N2→N3→N4→N5，完成16个验收ID，交付 NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS 或含具体阻断的 PARTIAL/BLOCKED。不要再次设计整个系统，不重新实现已通过领域。

执行前读取三仓适用规则，采样HEAD/工作区并保护他人WIP；计划创建造成的后端文档提交允许。复用旧验收和已有测试设施，仅对变化或证据缺失项复验。个人四栏为消息、通讯录、频道、我；企业四栏为消息、通讯录、工作台、我。成员｜组织架构在企业通讯录。

必须使用真实 macOS App 运行最新组织图，记录候选源码、构建、实际拖动/缩放、部门成员跳转、暗色/大字号、切企业/账号和撤权。Widget截图不代替原生验收。macos/ios工程与vendor保留区禁止修改。环境缺失就记录BLOCKED_ENV并继续不依赖它的卡。

所有OA对接/协议文档修订/SSO/Cookie客户联调均不属于本任务；官网入口不做网络联调。只验证工作台Tab存在。Internal API动态导出清单，排除OA exchange及独立OA协议测试，保留非OA凭证/Scope/Grant安全校验；不要把旧42/42写成这次非OA结果。

实际缺陷才修复；沿共享调用链定位根因，不加重复系统/无关抽象。每个功能或缺陷验证和评审后独立本地提交，Git身份leeyi <leeyisoft@qq.com>。不push、不部署、不生产迁移、不发通知、不访问真实客户数据或对外Webhook。

每卡完成写入RUN_ROOT的state.json、acceptance.tsv、recovery.tsv；每项记录SHA、命令、退出码、实际oracle和证据哈希。区分PASS_EXECUTED与PASS_REQUALIFIED；跳过、排除、未运行不能计PASS。失败修复重试每卡最多2轮，恢复先对账，不重放结果不明的写入。

N5冻结三仓候选并完成只读终审，核对16个ID精确集合与证据。交付FINAL.md、最新图/录屏、缺陷提交、原生验收结果和剩余边界。用户视觉认可仍为USER_VISUAL_ACCEPTANCE_PENDING；生产为NOT_DEPLOYED_NOT_VERIFIED。

无重大缺陷、环境就绪预计4–8小时，遇环境或权限缺陷重新估算。每卡报告完成ID与下一步，30分钟无进展说明具体原因；不要反复跑已通过全套来替代推进。现在从N0开始。
```
