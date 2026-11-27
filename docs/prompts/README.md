# IMBoy ECC Prompt Library

IMBoy 的 ECC 可复制提示词统一索引。每个仓库维护自己的案例文件，所有案例编号全局唯一，执行时在已安装 ECC 的 Codex 会话中复制单个代码块即可。

## 四仓案例

| 仓库 | 案例编号 | 数量 | 文件 |
|---|---|---:|---|
| Backend | ECC-001、ECC-005–022、ECC-043–045、ECC-051–072 | 44 | [ECC_USE_CASES.zh-CN.md](./ECC_USE_CASES.zh-CN.md) |
| Flutter | ECC-002、ECC-023–034 | 13 | [Flutter 案例库](https://github.com/imboy-pub/imboy-flutter/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| Admin | ECC-003、ECC-035–042 | 9 | [Admin 案例库](https://github.com/imboy-pub/imboy-admin-frontend/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| geo_regions | ECC-004、ECC-046–050 | 6 | [geo_regions 案例库](https://github.com/imboy-pub/geo_regions/blob/main/docs/prompts/ECC_USE_CASES.zh-CN.md) |
| **合计** | **ECC-001–ECC-072** | **72** | |

## 如何使用

1. 在 Codex 中打开对应仓库。
2. 打开本仓库的 docs/prompts/ECC_USE_CASES.zh-CN.md。
3. 复制目标案例完整代码块（包含 $skill-name 行）。
4. 粘贴到 Codex 会话并执行。
5. 核对输出中的实际命令、退出码、测试证据和未执行项。

## 统一维护规则

- 案例编号由本索引统一登记；新增案例必须先分配未使用编号。
- 每个案例必须独立可复制，不依赖会话中此前执行过的提示词。
- Skill 名称以 ECC 官方仓库 skills/<name>/SKILL.md 的目录名为准。
- 项目自己的 AGENTS.md、CLAUDE.md、架构约束、测试合同和安全规则优先于 ECC 通用建议。
- 提示词不是质量证明；真实代码、测试和 CI 证据才是验收依据。
- 不将一次性任务状态、会话恢复材料、运行日志、真实凭据或用户数据写入本案例库。
- 后端主仓负责编号登记和跨仓索引；各仓只维护本仓案例正文，避免重复副本。

## ECC 上游

- [ECC GitHub](https://github.com/affaan-m/ECC)
- [ECC Skills](https://github.com/affaan-m/ECC/tree/main/skills)
- [Codex Plugin 安装说明](https://github.com/affaan-m/ECC/blob/main/.codex-plugin/README.md)
