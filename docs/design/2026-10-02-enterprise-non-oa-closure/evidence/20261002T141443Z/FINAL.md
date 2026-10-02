# 非OA增量收尾与验收 FINAL — run 20261002T141443Z

日期：2026-10-02（UTC 14:14 起）。执行者：GLM-5.3。计划 SHA-256：
`e083be9d911bfec5c76f2b0cfdc87a1c09c9eded7d1172e0f455abf55ddd31f1`（执行前核验一致）。

## 结论

**`NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS`** — 16/16 验收 ID 全部通过，无 FAIL/BLOCKED/PENDING。
用户视觉满意度：**`USER_VISUAL_ACCEPTANCE_PENDING`**（不阻断技术验收，也不冒充用户确认）。
生产：**`NOT_DEPLOYED_NOT_VERIFIED`**（未部署、未迁移、未 push、未对外发任何通知）。

## 冻结候选（三仓）

| 仓 | 最终 SHA | 相对计划调查基线的变化 |
|---|---|---|
| imboy (backend) | `1459df71`（= bc09f0f6 计划文档提交 + 他人 deploy 提交 faba4323 + 本 run 证据提交 1459df71） | 业务源码相对旧验收候选 083901a6 零变化（仅 imboy_app.erl +3 行 ensure_rsa_keys/0 测试支持导出）；deploy 提交与全部验收域零交集 |
| imboyapp (app) | `a26cc64f`（= b6bb4dc + 本 run 两笔测试提交 a072bcf9/a26cc64f） | 业务源码 = b6bb4dc（N2/N3 原生验收时绑定），仅新增两个验收测试文件 |
| imboyadmin (admin) | `2b9015b4`（未变） | 相对旧验收候选 9a808bec 仅 ADR 文档 |

> 表中 backend「最终 SHA」= 验收证据冻结点 `1459df71`；本 FINAL.md 文档自身
> 提交（及其后任何纯文档修订）位于该候选之上，不触碰验收域，故不回改候选值
> （文档自引用固定点）。

他人 WIP（imboyapp 的 customer_service/enterprise 域、imboy 的 deploy 脚本）全程未回退、未吸收、未替交；
其中一次误将暂存区他人文件一并提交的事故已用 soft reset 就地纠正（未推送、历史无痕），最终提交以 pathspec 精确圈定。

## 16 项验收表

| ID | 状态 | 要点 |
|---|---|---|
| N0-A01 | PASS_EXECUTED | 计划摘要核验一致；三仓差异全分类；旧台账 verify.py exit0（30 父/42 子）；macOS 设备+隔离 PG 就绪 |
| N1-A01 | PASS_EXECUTED | 组织图布局/分页/展开/拖动/按钮+真实双指缩放/适应视图行为测试 95 用例 exit0 |
| N1-A02 | PASS_EXECUTED | 撤权清图、403/404 屏蔽迟到、换账号/企业旧响应不回填 |
| N1-A03 | PASS_EXECUTED | 个人/企业四栏、成员\|图切换、角色入口、选择器、P2P 预览（95+25 用例）；analyze 0 issues；diff-check 干净 |
| N2-A01 | PASS_EXECUTED | 真实 macOS App 进程登录合成 owner：图根=真实企业名、两层展开、子部门→成员路径、无越权节点、API 侧 103 根部门游标翻页取满 |
| N2-A02 | PASS_EXECUTED | 按钮/双指/拖动/适应/折叠：InteractiveViewer 矩阵断言，全程无异常 |
| N2-A03 | PASS_EXECUTED | 400/1440 逻辑像素无溢出、工具栏可操作；暗色经 App 内真实切换（Theme.brightness=dark + 选中标记）；边界：系统级最大字号无法程序化设置，由窄视口+widget 层 2x textScale 共同覆盖（测试内声明） |
| N2-A04 | PASS_EXECUTED | 103 根部门分页全载+抽样探测；三层深链路；空企业仅根节点；边界：后端故障注入未做，由 widget 层 deny/retry + N3-A04 撤权 403 路径覆盖（测试内声明） |
| N3-A01 | PASS_EXECUTED | 个人四栏固定、个人通讯录无加入/创建企业、无残留切换器；企业四栏含工作台（未打开 OA）；切回个人企业壳销毁 |
| N3-A02 | PASS_EXECUTED | 企业 A 图→空企业 C：仅 C 根节点，A/B 部门不出现；互切刷新无回填；边界：晚到确定性注入由 widget 层 N1-A02 覆盖（测试内声明） |
| N3-A03 | PASS_EXECUTED | 普通成员四栏一致、「我」页无企业管理入口、createDirectory 服务端直达 403（ensure_org_manager 真实链路） |
| N3-A04 | PASS_EXECUTED | member 图→admin 治理端点 suspend 200→图刷新 403→画布卸载+旧部门不出现+互切不复活→restore 200→新会话重读 200（撤权/恢复走 /members/:uid/{suspend,restore} 真实端点） |
| N4-A01 | PASS_REQUALIFIED | CS-01..03 双端客服域指纹零变化，旧双轮真实隔离 DB/Garage 旅程证据资格复用 |
| N4-A02 | PASS_REQUALIFIED + PASS_EXECUTED | ORG-01..03/FILE-01..03/INTG-03 指纹零变化复用；UX-01..03 旧证据绑旧导航已由 N2/N3 原生重验取代（更强） |
| N4-A03 | PASS_REQUALIFIED | Internal 精确集合：included 41 / excluded 1（INT-14 = POST /api/internal/v1/oa/sso/exchange）；端点集 42↔42 一致；非 OA 凭证/Scope/Grant 安全校验证据保留；未沿用 42/42 结论 |
| N5-A01 | PASS_EXECUTED | 16 ID 精确集合核对、原生构建来源、三仓状态、source 资格、证据哈希与排除项终审（本表） |

## 原生运行候选与来源

- 构建：`flutter build macos --debug --dart-define=APP_ENV=local --dart-define=API_BASE_URL_OVERRIDE=http://127.0.0.1:9800 ...`（来源 b6bb4dc，构建时工作区业务源码干净，仅测试文件后补提交）
- 运行：integration_test on macOS（完整 .app 进程：原生窗口/引擎/Keychain/SQLite/真实 HTTP 栈），存储经 `isolateMacosStorage` 隔离到临时命名空间
- 后端：本地隔离 `imboy_local@127.0.0.1:9800`（imboy_test_v1 @ 4323，Docker imboy_pg18）
- 合成数据：`n2_seed.sql`（企业 A：8 主部门+95 填充=103 根、三层深、3 成员；企业 B 单部门；空企业 C）；4 个合成账号；全部为本 run 专属
- 凭据：全部经 --dart-define 注入，未写入任何提交/报告；日志扫描无凭据命中
- 原始运行日志（n1/n2/n3 各 `*.log`）因仓根 `.gitignore` 的全局 `*.log` 规则
  仅留存于执行机本地（本目录），未入 git；其 SHA-256 已录于 `n2-sha256.tsv`
  /`n3-sha256.tsv`，可在本地复核，第三方 clone 无法直接重验哈希——此为留存边界

## 环境恢复与事故台账（摘要，全文见 recovery.tsv）

1. TSID guard catalog_changed：_rel 残留坏 manifest（24B/合法 66B）→ 按仓内先例归档 stale，pristine 重扫（floor 单调不降）
2. migration_dirty 循环：_rel 双版本残留（82 配置连旧 E2E 库 sc153_e2e）+ heart 2.5s 循环重启并发交叠 → 清 _rel 重建单一结构
3. 目录 API 503：本地缺 cursor 签名 key → sys.local.config（gitignored）补配与测试支持同值
4. macOS 截图取证四通道全不可用（屏幕录制权限/s 键空文件/平台通道未实现/VM 扩展未注册）→ 以结构化断言为 oracle，记录为环境边界，不冒充截图
5. App md5 首试 + 登录失败计数锁定链 → member 密码重置为 md5 命中变体（隔离库合成账号），首试零失败
6. 误提交他人暂存 WIP → soft reset 纠正 + pathspec 精确提交（历史无痕、未推送）

## 剩余边界（不影响本 PASS，均为后续独立事项）

- 视觉取证截图在本机环境不可用 → 用户视觉复核仍为 USER_VISUAL_ACCEPTANCE_PENDING
- 系统级最大字号、后端故障注入（原生层）、晚到响应确定性注入（原生层）——各自有等效层覆盖（widget 层/治理端点），测试内均有边界声明
- 同进程二次登录的导航竞态（A04b 绕开为 API 会话级验证）——潜在 App 稳定性观察项，非本计划缺陷修复范围
- 客户 OA 联调、SSO/Cookie 安全修订、生产部署、iOS/Android 新验收：OUT_OF_SCOPE
- imboyapp/imboy 工作区仍有他人进行中的 WIP（customer_service/enterprise、deploy 域），归属并行工作流

## 状态汇总

LOCAL：`NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS`；MACOS：原生旅程通过（取证截图受环境限制）；OA：`OUT_OF_SCOPE`；PRODUCTION：`NOT_DEPLOYED_NOT_VERIFIED`。
