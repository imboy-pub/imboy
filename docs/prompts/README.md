# IMBoy ECC Prompt Library

IMBoy 的 ECC 可复制提示词统一索引。每个仓库维护自己的案例文件，所有案例编号全局唯一。ECC-001–072 是基础工程案例；ECC-073–121 是产品功能场景案例，每个场景提供 A 新增功能、B 优化现有功能（含新模型复审旧 AI 代码）、C 测试现有功能三个独立提示词。执行时复制单个代码块即可。

## 四仓案例

| 仓库 | 案例编号 | 数量 | 文件 |
|---|---|---:|---|
| Backend | ECC-001、ECC-005–022、ECC-043–045、ECC-051–089、ECC-110–116 | 68 | [ECC_USE_CASES.zh-CN.md](./ECC_USE_CASES.zh-CN.md) |
| Flutter | ECC-002、ECC-023–034、ECC-090–101、ECC-117–118 | 27 | [Flutter 案例库](https://github.com/imboy-pub/imboy-flutter/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| Admin | ECC-003、ECC-035–042、ECC-102–107、ECC-119–121 | 18 | [Admin 案例库](https://github.com/imboy-pub/imboy-admin-frontend/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| geo_regions | ECC-004、ECC-046–050、ECC-108–109 | 8 | [geo_regions 案例库](https://github.com/imboy-pub/geo_regions/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| **合计** | **ECC-001–ECC-121** | **121** | |

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
- 不将一次性任务状态、会话恢复材料、运行日志、真实凭据或用户数据写入本案例库。
- 后端主仓负责编号登记和跨仓索引；各仓只维护本仓案例正文，避免重复副本。

## ECC 上游

- [ECC GitHub](https://github.com/affaan-m/ECC)
- [ECC Skills](https://github.com/affaan-m/ECC/tree/main/skills)
- [Codex Plugin 安装说明](https://github.com/affaan-m/ECC/blob/main/.codex-plugin/README.md)
