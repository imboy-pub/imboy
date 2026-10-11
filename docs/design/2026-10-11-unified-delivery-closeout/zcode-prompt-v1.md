# ZCODE 无人值守统一收口提示词 / Unattended closeout prompt

将下面代码块完整作为一次任务交给 ZCODE。用户发送此提示词即授权其中限定的本地实施；当前编制会话不启动实施。允许最多10个并发agent，包含协调者与verifier。

```text
你是本任务的ZCODE总协调者。请执行，不要停留在评估或建议；一次启动持续完成全部授权内可执行工作，不需要我逐卡指挥、批准测试或确认常规实现方案。

唯一执行计划：
/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-11-unified-delivery-closeout/plan-v1.md
固定SHA256：58f7e2bdafd69c3c15292b457b0544170f2fba7c982930d586ae58a0220602a8

先读取适用AGENTS.md，机械复算计划文件hash，并核对同目录plan-v1.md.sha256。二者均须等于上述固定hash；不一致则PLAN_HASH_MISMATCH，禁止静默接受新版本。读取三个source-contracts快照并核对计划所列原文hash。旧三个计划已冻结，不继续修改旧台账或历史verdict。

我授权你按本计划完成imboy、imboyapp、imboyadmin内所需源码、测试、runner、文档的本地修复与验收，建立或复用隔离worktree/执行分支、锁定开发依赖、本run合成数据测试库/服务，在已连接且可合法使用的用户测试设备上执行原合同旅程，验证后按独立功能本地提交。提交命令级身份leeyi <leeyisoft@qq.com>，不改全局配置。所有成果集成到本run分支；不得自行合并共享main、push、部署、发布、生产迁移、设置联系方式、通知第三方、改变签名身份或代签UAT。

最多10个agent总并发，按计划A0–A9角色与U00–U12依赖调度。先本地完成U00基线和隔离门，再并行推进，不等待我批准U00报告。worker不是独占代码库，不得回滚其他会话改动；每卡明确唯一文件owner。共享router/权限/迁移/配置/ebin/数据库/设备/浏览器/全量窗口单owner或串行租约，不要为满10个名额制造冲突。

缓存及CX由其他会话执行，只读发现其候选与交接证据，不重复实施或抢资源。OA getAuthCode默认已经完成，只核验继承；企业OA一次码/撤权等其他原义务继续。E2EE专项、L4/relay专项、AC31–34明确后期；现有IM回归与保留/授权负例不得删除。

采用原合同全部子场景为底线。历史PASS不能直接继承；缺少正式豁免来源时默认补原要求，不向我询问要不要放宽。父项全部required子项通过才PASS。phase1缺口不止AC15/28，必须包含审计指出的AC05/10/12/14/16/23/25/28/29/30和其余父项覆盖复核；微社群按26项全子场景，EO展开原EWS与required cells，不拿任务卡数量当验收集合。

先复现再修，已有通过行为不重写。MC22先确认实际VIEW发送和Flutter路由接收，不预设需要新原生桥接；AC29必须恢复后实际业务读取与PG/Garage联合对拍，marker恢复不替代；MC21每轮不可覆盖日志，绑定dirty字节/源码/build；全量run2九败/run3三败分别核对；dialyze差分及225 skips逐项解释，不关闭检查来求绿。

环境失败先自恢复：本地构建真实hird、本runPATH、独立PG/Garage/端口/连接预算、合法设备与签名诊断。不重启共享服务，不接生产库，不用模拟器替代真机，不删除测试或吞exit。两轮无进展转独立诊断，有新证据继续修复，不能为了省事提前BLOCKED。真实外部条件不可用才保留阻断。

RUN_ROOT置于/Users/leeyi/.zcode-runs/下，使用唯一unified-delivery-closeout时间戳目录。完整记录三仓HEAD/tree/patch/untracked内容hash、构建、锁文件、runner/config/fixture指纹及资源归属。每个子场景写命令/cwd/时间/exit/oracle/日志hash及候选。按计划原子checkpoint与追加events；进度定期更新。重启/压缩读取checkpoint继续，不重新实施已有效通过项。

依赖未交接时推进其他卡，按计划第10.3节有限非阻塞检查，禁止无限等待或把缺交接算PASS。已提交且归属明确的依赖可在本run分支集成并重验，不吸收他人dirty补丁。任何候选/runner/config变化使影响门失效，重新冻结并重跑影响闭包。

U00冻结final-gates.tsv，逐门明确原合同定位、命令、最终候选/build绑定、轮次、exit/警告/skip规则及前置条件，显式包含最终候选dialyze。EUnit连续两轮，其余按原合同轮次且至少一次，不把所有门强加两轮。可执行修改收束并冻结候选后进入U11，即使设备或CX依赖阻断也运行其他前置齐备门；受阻门逐项登记BLOCKED。释放并发名额，启动未参与本run实现及requirements/ledger编制的新只读verifier；U12独立从冻结源复核叶子全集，再验台账。全部原required闭合走FULL签UNIFIED；仅有可核实正式授权修订且修订集合全闭合时签REVISED_SCOPE_LOCAL_PASS，保留原合同差异；其余走PARTIAL成果/阻断/续跑点审查，不能因U07/U09受阻跳过终审。

最终门必须显式列Backend EUnit，两轮同固定候选实际exit均0；required叶子skip/cancel/未执行不能记PASS。正式授权修订单独记账并保留原合同差异；有真实授权且修订集合全闭合可经U12签REVISED_SCOPE_LOCAL_PASS，不能签原合同统一PASS。dialyze实际在最终候选运行默认RC0，原更严格警告标准保留；警告差分、历史结果、基线解释不得将非零质量门改判PASS。

U12逐类验证预期拒绝，每类独立case/receipt：缺失叶子、多余叶子、重复叶子、错误SHA/hash、日志复用、正常质量门非零却标PASS、子项未全通过却签父PASS、错误设备或平台、dirty字节漏绑、外来WIP混入。正常完整包与如实受阻部分包分别审查，包准确性通过不等价功能PASS。负例receipt标kind=negative_probe，记录expected_exit与命中拒绝规则；预期非零拒绝不属于正常质量门失败，不能用该receipt充当质量门通过。

读取同目录materials-index.json：MISSING/UNVERIFIED源保留阻断且继续可定位合同任务，不编造祖先叶子，不用历史requirements替代原文。现有微社群verifier为NO_FULL_EUNIT策略，不能原样终审；U02最小适配，U12新独立审查者运行。历史一期runner固定端口/config须在本run适配，dialyze-local/check棘轮不能替代原始dialyze RC0。

U00将每个验收叶子绑定(source_plan,source_id,subcase,platform,role)稳定唯一键及冻结原合同定位，映射责任卡。登记共享租约共同入口/授予凭证、命令实际超时/进程身份/心跳与诊断机制；本run自行写leases.json不代表已取得跨会话租约，无共同协议则隔离替代或BLOCKED_RESOURCE。超时触发诊断，不能跳过质量门。会话结束后无既有持久调度器则记录RESUME_REQUIRES_RELAUNCH，不声称自行唤醒。

不要问“是否继续、是否跑测试、是否提交”。持续调度直至全部原本地required通过U12并签UNIFIED_LOCAL_CANDIDATE_PASS；存在真实正式范围修订且修订required全部通过U12时，可按REVISED_SCOPE_LOCAL_PASS结束并保留原合同PARTIAL与被修订项，不新增豁免。若所有授权内可执行任务及U11可执行门已穷尽而仅剩经U12部分终审复核的外部/设备/合同/来源/依赖阻断，则签AUTONOMOUS_RUN_EXHAUSTED且总体PARTIAL/BLOCKED，逐项说明解锁条件，绝不称功能已干完。确定隔离失效或计划hash错误按计划停止并保全。

最终给我：三仓本地提交与分支、最终候选与产物hash、源父项/子项精确ledger、测试命令/exit/断言与证据、U12结果、每个原合同状态、未解决项和NEXT_UNLOCK、可直接再次发送的resume-prompt.md。production/UAT/共享main合并/push实际没做均如实写NOT_PERFORMED。完成前不能以“建议下一步”代替实施。
```
