# IMBoy ECC Prompt Library

IMBoy 的 ECC 可复制提示词统一索引。每个仓库维护自己的案例文件，所有案例编号全局唯一。ECC-001–072 是基础工程案例；ECC-073–125 是产品功能场景案例，每个场景提供 A 新增功能、B 优化现有功能（含新模型复审旧 AI 代码）、C 测试现有功能三个独立提示词。执行时复制单个代码块即可。当前共 125 个案例、231 个可复制提示词代码块，其中 53 个功能场景各提供 A/B/C 三种工作模式。

## 四仓案例

| 仓库 | 案例编号 | 数量 | 文件 |
|---|---|---:|---|
| Backend | ECC-001、ECC-005–022、ECC-043–045、ECC-051–089、ECC-110–116 | 68 | [ECC_USE_CASES.zh-CN.md](./ECC_USE_CASES.zh-CN.md) |
| Flutter | ECC-002、ECC-023–034、ECC-090–101、ECC-117–118 | 27 | [Flutter 案例库](https://github.com/imboy-pub/imboy-flutter/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| Admin | ECC-003、ECC-035–042、ECC-102–107、ECC-119–121 | 18 | [Admin 案例库](https://github.com/imboy-pub/imboy-admin-frontend/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| geo_regions | ECC-004、ECC-046–050、ECC-108–109 | 8 | [geo_regions 案例库](https://github.com/imboy-pub/geo_regions/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| **合计** | **ECC-001–ECC-125** | **125** | |

## 四仓契约边界

- Backend 是 REST/WS 提供方，Flutter App 与 Admin 分别是消费端；API/WS 契约以 Backend 当前真源及各消费端实现为准。
- geo_regions 是独立 Dart/Flutter 地区数据 SDK，当前由 Flutter 消费；地区协议以 geo_regions 的 `docs/PROTOCOL.md`、`docs/CONSUMER_CONTRACT.md` 为准，不把它当作 REST/WS 服务。
- TSID/EntityId 一致性只覆盖 Backend、Flutter、Admin 三端；geo_regions 的 country/region ID 与 pathIds 是独立稳定标识。
- 四仓提示词中的代码路径和测试文件是当前定位锚点，不是封闭清单；每次执行都必须重新核对目标仓 HEAD、工作树和实际代码。

## 如何使用

1. 在 Codex 中打开对应仓库。
2. 打开本仓库的 docs/prompts/ECC_USE_CASES.zh-CN.md。
3. 复制目标案例完整代码块（包含 $skill-name 行）。
4. 粘贴到 Codex 会话并执行。
5. 新功能选 A，优化/新模型复审旧 AI 代码选 B，独立验证现有行为选 C；可先执行 B 建立基线，再执行 C 证明改进。
6. 核对实际命令、退出码、测试证据和未执行项。

## 统一维护规则

- 案例编号由本索引统一登记；新增案例必须先分配未使用编号。
- 每个案例内的 A/B/C 提示词必须各自独立可复制，不依赖此前执行过的提示词。
- 新模型复审旧 AI 代码时，先限定功能切片、重建实际行为并建立基线；不得为了展示模型差异而全仓重写。
- Skill 名称以 ECC 官方仓库 skills/<name>/SKILL.md 的目录名为准。
- 项目自己的 AGENTS.md、CLAUDE.md、架构约束、测试合同和安全规则优先于 ECC 通用建议。
- 提示词不是质量证明；真实代码、测试和 CI 证据才是验收依据。
- 跨仓提示词必须区分 REST/WS 契约与 geo_regions SDK 数据契约；不得因四仓共用索引而假定四仓两两直接依赖。
- 不将一次性任务状态、会话恢复材料、运行日志、真实凭据或用户数据写入本案例库。
- 后端主仓负责编号登记和跨仓索引；各仓只维护本仓案例正文，避免重复副本。

## ECC 上游

- [ECC GitHub](https://github.com/affaan-m/ECC)
- [ECC Skills](https://github.com/affaan-m/ECC/tree/main/skills)
- [Codex Plugin 安装说明](https://github.com/affaan-m/ECC/blob/main/.codex-plugin/README.md)


## 执行时间与闲时任务安排

每个案例正文标题下方标注单次执行预算。原工程案例标注一个预计分钟数；新增业务场景分别标注 A（新增功能）、B（优化现有功能）、C（测试现有功能）的预算，三者是互斥选择，不是相加时间。

- **少于 30 分钟**：适合日常交互、只读盘点或单点验证。
- **30 分钟及以上**：建议放入 ZCODE 闲时任务；优先选择单一仓库、边界明确的案例。
- **90 分钟及以上**：建议设置阶段检查点；如需跨仓、设备、数据库或外部服务，先确认环境可用。
- 预算包含读取项目规则、分析代码、必要实施和相关验证；不包含排队等待、人工决策、环境安装及外部服务不可用造成的阻塞。
- 预算是调度参考，不是交付承诺。必须如实报告 PASS/FAIL/BLOCKED/NOT RUN，不得为赶时间缩减测试或伪造完成。

时间估算基于当前四仓代码规模、架构边界、提示词任务范围和现有测试入口；首次运行或仓库状态变化后应重新校准。
