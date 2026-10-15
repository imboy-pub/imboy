# IMBoy 三端 GitHub CI 修复与极简微社群验收收口计划 V2

版本：V2，合同日期 2026-10-14；2026-10-15 根据原规划会话恢复完整执行边界。
**状态：IN_PROGRESS / NOT_ACCEPTED。合并不是验收；本文件不签署 A/B/C/D 通过。**

## 1. 权威关系与边界

本计划是 `docs/design/2026-10-08-micro-community/plan-v1.md` 的增量工程收口，不重启已冻结旧执行入口，不替代统一交付计划的其他范围或门禁。V1 全部26项和子场景原样继承；新 CI 验收使用独立编号。

前一执行会话未收到原正文，自行起草缩减版，仅执行部分修复；其结果和失败历史保存在仓外 `~/.zcode-runs/v2-closeout-20261014/EXECUTION-LEDGER.md`。该历史台账不可改写为通过。此前“MC22 两端深链未实现”的归因错误：候选已有 Android VIEW/MethodChannel 和 Flutter bridge，保存的负例脚本却未发送 VIEW，也未驱动 control barriers。失败仍有效，须修编排并重验，不能移出范围。

产品边界：personal channel 内容区与 private/invite-only group 讨论区一对一绑定，复用消息/评论/反应/附件/成员/角色；绑定、精华、标签、正文搜索、持续订阅/付费授权。禁止新增 community 表族、多群、多对多、企业扩展、新支付或加密协议。原 `ios/*`、`macos/*`、`plugin/r_upgrade`、`erlang.mk` 保留区不变。

授权：本轮本地修复和隔离验证。commit、push、PR、Actions、真机及外部服务操作须有各自明确授权；合并、部署、发布、真实支付、生产迁移不随修 CI 获授权。默认串行，不占用其他会话数据库/端口/工作树，不覆盖 WIP。

## 2. 基线与证据原则

2026-10-15 本轮起点（实施前须复采）：Backend `4c48e666e97217a6966455d986b20362a3070dcc`；App `a392995acac28f9c281026f8ac4449948a535ec3`；Admin `e0ce62c1f50bfc06f1bca84dd0595fbc9be8d55d`。均来自本地 main，GitHub 分别为 imboy-pub/imboy、imboy-pub/imboy-flutter、imboy-pub/imboy-admin-frontend，不能把 origin/Gitee 当 GitHub。

使用隔离工作树；主树允许继续演进。实际代码证明实现存在，raw log/退出码/DB及投递断言证明行为。历史 PASS 不自动继承；既存失败能帮助归因，不能豁免完整门禁。V1 INTEGRITY_FAIL 和14文件漂移历史保留，不换哈希洗白。

最终清单固定三仓各自 SHA/tree、干净索引及工作树、依赖锁/工具链、PG镜像digest/扩展/schema、产品配置、fixtures/执行器、App构建、设备及运行资源。PR merge SHA/head SHA/实际 checkout SHA 分开记录。计划合同与仓外可变执行台账分离，禁止自引用哈希。

每份证据包含 command argv/cwd/env指纹、开始结束时间、原始exit、raw log及摘要、对应叶项的具体期望/实际值、设备/资源身份。同候选同配置同范围可复用；候选或相关环境变化使受影响证据失效。两次全量必须属于同一最终候选，合并前证据不自动证明合并后。

## 3. CI全矩阵（不得缩成三个主CI）

| CI-AC | 必须覆盖 | 硬断言 |
|---|---|---|
| 01 | 三仓 workflow/事件/checkout/配对SHA/lock/缓存/工具链 | 当前支持且适用的检查全集；Required skipped/cancelled不算PASS |
| 02 | Backend Nightly PG上下文、Moment PG初始化；App PATH/目录/依赖 | 冷环境可重建，缺扩展/文件/工具明确失败 |
| 03 | nightly/classifier/Dialyzer、Admin Quality | raw非零、缺/坏报告、工具崩溃不能归零；业务失败不受KNOWN/FLAKY豁免 |
| 04 | Backend backend-ci/quality/nightly/contract/docs/SBOM/Sonar及适用widget门 | 已批准静态基线只减不增，不豁免业务断言 |
| 05 | App ci/quality/contract/integration/Sonar及适用手动门 | format/analyze/unit/widget/路由/SQLite/E2EE等真实运行，逐条解释skip |
| 06 | Admin ci/quality/Sonar/setup-E2E及适用widget门 | test/lint/typecheck/deadcode/build；setup绿色不能替MC23 |
| 07 | API/C2SAction/error-code/protobuf/EntityId | 固定跨仓组合一致，生成器/源路径错误也必须红 |
| 08 | 远端同候选最终矩阵与外部服务 | Secret/项目绑定问题由人工配置，不关闭服务；无未授权发布通知 |

发布、签名、部署、机器人专用事件可按原条件N/A，不把必要测试临时改N/A。Nightly手动检查默认不发Gitee issue；docs手动默认只构建不部署。远端绿色是独立终态，不能用本地预期代替。

已知调查入口（以当前复现决定修复，旧run不是新候选证据）：Backend Nightly38484883815的Docker COPY、quality38386859258的错owner/卫生/分层/第三方清单、backend-ci38386859133的xref/Moment/Dialyzer/E2EE取消、contract38386859106、docs38386859089、Sonar38386859097。App ci38474654509/38387660864、contract38387660822、quality38387660819、integration38387660815、Sonar38387660890。Admin ci38387523146、quality38387523065、Sonar38387523091、E2E38387523063。所有实际job/attempt/checkout/raw退出码写入仓外台账，不假定旧首因仍存在。

## 4. 原26项差距与执行合同

以下原要求逐字继承；分号分句在 `docs/reference/contracts/micro-community-v2-policy.json` 固定叶项。同一分句内的角色、接口、状态、边界组合仍须全部验证，不能仅跑一个代表样本。

| ID | 原场景与行为 oracle（全部子场景） | 必要证据 |
|---|---|---|
| MC-AC-01 | 三仓基线、候选可追溯；受保护 WIP 内容未改变、未混入交付 | HEAD/tree/WIP 摘要与最终差异 |
| MC-AC-02 | 空库和旧 schema 升级成功；旧频道绑定 NULL、旧帖子精华 false，旧内容/订单/成员不丢 | 真实 PG schema、迁移日志、升级前后断言 |
| MC-AC-03 | 非存在群 FK 拒绝；同群第二频道冲突；多个 NULL 合法；超过 2^53 的 ID 跨三端不变 | 真实约束错误、API/DTO 往返、DB 行 |
| MC-AC-04 | 双端拥有者能绑定；普通成员、单端管理员、其他圈主不能绑定，失败零副作用 | API业务码、关系/审计前后查询 |
| MC-AC-05 | workspace、跨域、禁用/删除群、公开群、非空旧群、非兼容 E2EE 组合明确拒绝 | 参数矩阵及 DB 不变断言 |
| MC-AC-06 | 两频道并发绑定同群仅一胜；同值重试不重复写/审计；故障注入全部回滚 | 并发 barrier、事务日志、唯一关系 |
| MC-AC-07 | 空绑定可解绑；非空解绑/换群及直接删除受限；停用不变免费；转移不破坏双端权限 | 生命周期 API、DB、有效访问验证 |
| MC-AC-08 | 开启包入口正常；锁定包 UI/API/深链/缓存均 fail-closed；旧频道/群不被自动绑定 | 两类包配置、端侧和 API 证据 |
| MC-AC-09 | 现有文本/图片/附件发布、评论、反应、置顶、撤回正常，不另建内容数据源 | 频道现有接口及 DB回归；媒体授权 |
| MC-AC-10 | 精华与置顶四种组合独立；普通作者无权管理精华；撤回/删除后精华列表无泄露 | 接口、列表、DB两列断言 |
| MC-AC-11 | tags 边界、去重、删除标签、部分编辑成立；其他 payload 字段不丢；非法输入零写入 | 参数化用例与完整 payload 比较 |
| MC-AC-12 | 只在帖子正文出现的中文词可检索；频道名称匹配不充当正文命中；标签/精华组合有效 | 真实 jiebacfg/PG结果与查询计划 |
| MC-AC-13 | 检索/内容/精华分页无重复遗漏；篡改 message/channel/cursor ID、撤回和私有数据不泄露正文/片段/总数 | 多页 oracle、跨频道IDOR矩阵 |
| MC-AC-14 | 免费订阅者显式入群；私有频道邀请接受前拒绝、接受后允许；邀请/扫码/旧入群 API 均无绕过 | 所有入群入口清单、API/DB成员行 |
| MC-AC-15 | paid未到期可进；pending/refunded/cancelled/expired拒绝；仅脏订阅行仍拒绝；会员到期与付款超时不混淆 | 隔离 PG 合成订单及真实权限查询 |
| MC-AC-16 | 退订、移除订阅、退款、频道停用后，发送/历史/成员/公告/文件/相册等新访问拒绝；有效名单计数一致 | 全入口权限表、API/DB副作用查询 |
| MC-AC-17 | 已连接/已缓存/离线账户撤权后，广播、重试、通知和离线拉取不交付新内容；授权竞争按冻结线性化点成立 | 两个真实 WS、收件序列、竞态日志 |
| MC-AC-18 | DB时钟跨 subscription_end_at 后无需重登即失权；管理例外仅双端权限成立；加密策略不被降级 | 截止时间、边界前后读写、角色矩阵 |
| MC-AC-19 | DB不可用、权益查询异常、退款与发送/绑定并发时 fail-closed；无误发、重复成员或半绑定 | 故障注入、并发 oracle、最终 DB 状态 |
| MC-AC-20 | 未绑定的普通群、独立频道、现有 workspace和单聊行为不变；同一组 fixtures 跑基线/候选 | 回归日志与基线对照 |
| MC-AC-21 | Android真机完成浏览帖子→订阅→讨论→返回→精华/标签/检索；数据对应同候选后端 | device/platform、版本、操作日志、API/DB |
| MC-AC-22 | Android旧深链、缓存、弱网重连及正在聊天时撤权正确；拒绝后无新内容残留或虚假成功 | 真机错误旅程、WS/API、端侧状态 |
| MC-AC-23 | 真实 Admin 浏览器完成选群/绑定/冲突/精华管理；普通管理员越权拒绝；禁止拦截伪造业务响应 | 浏览器trace、API业务码、DB前后验证 |
| MC-AC-24 | 合成数据隔离库可升级、备份、旧候选回退演练；有数据时 down 不作为无损回滚；没有写正式库 | 本地恢复日志、行数/哈希与资源身份 |
| MC-AC-25 | 同一冻结候选全EUnit连续两次退出0；Admin test/lint/typecheck/deadcode/build、App analyze/test通过；无 Required skip/cancel | 原始日志、命令/exit、版本指纹 |
| MC-AC-26 | 独立 verifier 检出缺ID、多ID、重复ID、错SHA、非零exit、假PASS/缺证据、错误设备、WIP混入；正常包重验通过 | verifier负例自测与终审输出 |

差距分类使用 IMPLEMENTATION_GAP/IMPLEMENTATION_DEFECT/REGRESSION/EVIDENCE_GAP/ENVIRONMENT_BLOCKED/AUTHORIZATION_BLOCKED/CONTRACT_CONFLICT/STALE_EVIDENCE。没有当前证据不能写PASS，没有源码缺失证据不能写IMPLEMENTATION_GAP。

| AC集合 | 当前事实与缺口 | 任务/验证 |
|---|---|---|
| 01 | 合并前后版本不同，WIP/候选/主树演进需分别追溯；EVIDENCE_GAP | W0、EVID；三仓manifest及不可混入WIP |
| 02–07 | 166/167迁移、binding DS/Repo及生命周期guards已存在；当前完整矩阵证据缺失 | DB、BIND；真实PG/HTTP角色、并发、唯一/FK、回滚 |
| 08–13 | profile门、content DS/Repo、tags/essence/search已存在；缺新组合证据 | CONTENT、UI；双profile、正文中文、分页IDOR、payload零损失 |
| 14–19 | access DS/Repo、退款/退订、history/attachment/投递guards存在；需穷举消费者及竞争 | AUTH；入群各入口、DB时钟、双manager、在线/离线/重试/通知及故障 |
| 20 | 无同fixture基线/候选完整对照 | REG；普通群/独立频道/workspace/C2C |
| 21/23 | 合并前真机/浏览器日志有成功，但不能证明当前main | UI；新候选真机及真实HTTP浏览器，逐次保存不覆盖 |
| 22 | waitChat真实失败；编排缺VIEW/control动作，非产品链不存在 | UI；live/reconnect完整真实旅程，旧intent/缓存拒绝无新残留 |
| 24 | 无完整新候选升级备份旧服务回退包 | RECOVERY；合成库恢复与服务行为，不用破坏性down冒充无损 |
| 25 | 历史full164/163失败，App145skip，未达门禁 | FULL；Required逐条处理，两轮full RC0及三端全门 |
| 26 | 旧verifier仅NO_FULL_EUNIT、强制MC25 PARTIAL | EVID；显式V2模式、全叶/两full/环境及独立复核；旧模式原样保留 |

权威授权要比较 `channel_logic_common` 与事务SQL语义，不能预设漏洞。付款超时与 subscription_end_at 分开；写入在事务权威检查/锁边界内，撤权提交后新投递重新校验。缓存不作权限真源；当前附件新入口也须纳入。

## 5. 顺序任务卡

所有卡共同要求：修改前查调用方、fixtures、配置、导出及直连回归；仅改已证实缺陷，不触保留区/外来WIP/业务范围。交付为最小diff、定向raw证据及仓外台账。失败保留首因，诊断性基础设施重试最多一次；同因两轮无进展停该路径并记录恢复检查点，不无限重跑。完成条件是该卡所有断言和负例通过，而非命令显示部分成功。

| 卡/依赖 | 目标、文件及实施步骤 | 定向入口/正反断言与恢复位置 |
|---|---|---|
| W0/无 | 三仓Git、完整CI矩阵、V1叶项、隔离资源/handoff；复采后固定起点 | git/gh只读；错仓/错SHA/资源不属本任务拒绝；恢复到新baseline |
| CI-ENV/W0 | backend workflows上下文/owner/PG；App contract PATH/sibling/setup-dart；先修最上游 | 语法/路径与定向编译/契约；缺工具/扩展必红；环境成功后再诊断业务 |
| CI-TRUST/W0 | nightly-gate及Admin quality、gitleaks判定；原始rc不被分类吞 | 包装器正负自测：非零/缺日志/取消/坏JSON/工具崩溃均红 |
| CI-CODE/CI-ENV | 当前仍存在的xref/静态增量/E2EE/format/router/SQLite/设备fixture/60或174失败分组 | 真实受影响模块及直接调用方；不照搬旧远端错误、不扩大baseline |
| CI-CONTRACT/CI-ENV | 现有API/错误码/proto生成链和文档构建 | make contract-check/rest-contract-check、现有生成check；错配SHA/漏路由必须失败 |
| CI-DOC/W0 | 正式合同与临时产物分类、计划纳入Git、模板清单/文档语法 | hygiene正负例；凭据/缓存/临时报告仍拒绝；历史证据不删不批量白名单 |
| EXT/W0 | 三仓Sonar项目/权限/Secret与远端检查 | 人工必要配置后真实run；阻塞仅影响依赖任务 |
| DB/CI-ENV | 现有迁移链空库/旧库升级；保持166/167并兼容当前后续迁移 | 真实PG行数/字段/FK/唯一性；旧内容订单成员不丢 |
| BIND/DB | binding DS/Repo及Delete/Transfer调用；角色/资源/事务与并发 | focused EUnit+PG/HTTP；双owner、同值重试、两频道抢群、故障零副作用 |
| CONTENT/DB | content DS/Repo及App/Admin现有入口，MC09–13 | tags边界/payload、精华置顶4组合、正文中文、分页IDOR/总数不泄露 |
| AUTH/BIND | access DS/Repo及全部已启用消费者；退款/到期/撤权与投递线性化 | 真PG/两WS/HTTP、barrier、DB和收件序列；故障failclosed |
| REG/AUTH | 同fixtures对照旧基线与新候选 | 普通群/独立频道/workspace/C2C，不以不同数据集代替 |
| UI/AUTH+CI-CODE | Android owned positive/live/reconnect、Admin真实浏览器 | 设备/浏览器需授权；无响应伪造；API/DB/实际投递交叉断言 |
| RECOVERY/DB | 独占合成库备份→升级→恢复备份→旧服务启动 | 行/关系哈希与旧服务行为；正式库不可触达 |
| EVID/W0 | 扩展既有micro verifier显式V2；复用现有检查，不改统一verifier语义 | 缺/多/重复父叶、错SHA/设备、非零、假PASS、缺日志、越界、WIP、复用两轮等负例 |
| FULL/本地修复全部完成 | 固定三仓最终组合，统一全门和独立终审 | 连续两完整EUnit RC0、无Required skip/cancel；失败先定向修复再重新冻结 |

## 6. 命令与证据格式

已存在：Backend `make compile`、`IMBOYENV=local make test-build`、`IMBOYENV=local make eunit t=<存在模块>`、`make dialyze-check`、`make contract-check ADMIN_DIR=<固定Admin> FLUTTER_DIR=<固定App>`、`make rest-contract-check`；App `flutter analyze`、`flutter test <实际测试>`/最终`flutter test`；Admin `bun run test/lint/typecheck/deadcode/build`。macOS需GNU Make4；不能用系统Make3失败结果冒充源码失败。

最终完整入口为 `IMBOYENV=local EUNIT_CONFIG=<独占配置> EUNIT_USE_RELAY=0 make eunit`，不传t/EUNIT_MODS/EUNIT_TEST_SPEC覆盖；Makefile已有排除须映射独立必需专项门。环境覆盖及执行器摘要入manifest，两轮各自完整日志，禁止拼接。

现有PG/HTTP/WS集成是导出`run/0`，不是任意`make eunit t=...`就会执行。fixture默认旧路径/固定库必须先与本次租约匹配再运行。Android真实配置使用现有owned session和gzCustomer参数，不能只设一个API变量。

V2 verifier沿用CLI `--run-root --plan-path --policy-path --expected-candidate-sha256`，manifest执行策略为MICRO_COMMUNITY_V2，外部预期摘要绑定整个清单。新增 `environment_path/environment_sha256/executor`；每个command保存同环境摘要与environment_overrides、带START/END原始封套。`acceptance-leaves.json`必须覆盖policy全部叶项，引用command与raw log摘要、候选、独立JSON oracle（leaf_id/command_id/candidate/log摘要、assertions的name/expected/actual/passed；Android含device_id/platform）。

`quality-gates.json`保留既有八门，新增`backend_full_eunit: PASS`及两个不同command的`backend_full_rounds`。`independent-review.json`记录不同于executor的reviewer、候选、PASS及acceptance.tsv/acceptance-leaves.json/quality-gates.json/commands.jsonl摘要。正常V2状态是MICRO_COMMUNITY_CANDIDATE_PASS；结构校验不能证明人工伪造日志真实，不代替独立来源审核，更不等于远端CI或生产资格。

待完成的最终证据不得填synthetic/self-test为业务PASS。自测只证明工具行为。

## 7. 最终判定

A本地：固定候选全部本地必需门通过。B远端：每个适用job实际checkout绑定同组合且原始结果真实通过。C微社群：26项全部叶项、真PG/Admin/Android、两full与独立verifier成立。D整体：A+B+C同组合通过。缺B不说CI已修复，缺C不说微社群完成；D也不授予生产/支付/未测平台资格。

外部授权未取得时只阻塞相关任务。最终候选变化必须重验，不以“只改测试/只解决冲突/主树本来失败”为豁免。
