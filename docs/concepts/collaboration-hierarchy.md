# 协作层级（Collaboration Hierarchy）：组织 · 工作区 · 项目 · 群组 · 频道

> Purpose：定义 IMBoy 五个协作实体的归属关系、权限根与数据库不变量。
> 这是三端歧义最集中的领域——「先有表后定语义」的边界裁定仍在推进（见文末 TARGET）。

## Concept：三层归属结构

```
组织（Organization）──────── SaaS 租户边界，owner/成员/部门/邀请
  └── 默认工作区（Default Workspace，至多 1 个引用）
  └── 工作区（Workspace）──── 协作与资源容器，owner/member/guest
        ├── 项目（Project）─── 结构化协作：任务/里程碑/成员/频道关联
        ├── 群组（Group）───── scope=workspace 的群挂到工作区；scope=personal 的群游离于组织之外
        └── 频道（Channel）─── 同上两态 scope；personal 频道不能进项目（FK 层拒绝）
个人域（无组织/工作区）：scope=personal 的群组与频道、好友单聊、朋友圈
```

各层角色是**独立正交**的角色集，不跨层继承：

| 层 | 角色集 | 证据 |
|---|---|---|
| 组织 | `owner / admin / member` | `organization_member.role` CHECK（迁移 113） |
| 工作区 | `owner / member / guest` | `workspace_member.role` CHECK（迁移 76，注释明确禁止扩展为通用 RBAC） |
| 项目 | 项目成员 ⊆ 工作区成员（双向 fail-closed 触发器） | 迁移 81 |
| 群组 | smallint 0-5：成员/嘉宾/管理员/群主/副群主 | `group_member.role`（迁移 1/101） |
| 频道 | 订阅制 + 治理角色 1 编辑/2 管理员/3 创建者 | `channel_admin.role`（迁移 3） |

## Current

**已实现（数据库 + API + 三端界面）：**

- **组织**：创建/详情/归档/恢复/删除预检、成员生命周期（role/suspend/restore/offboard）、owner 转移、邀请（token 只存摘要）、部门树（防环、同 org 命名唯一、部门管理员无资源权限）、默认工作区引用。API：`/api/v1/organizations/*`（26 条）；App 端 `lib/modules/organization/`；Admin 端 `/organizations` 治理面（`/api/adm/organizations/*` 18 条）。
- **数据库不变量**：每组织恰一 active 真人 owner（迁移 126/127 双侧触发器 + 部分唯一索引）；owner 变更同步成员行；active owner 成员行禁单独移除。
- **工作区**：创建（request_id 幂等）/团队码加入（8 位码、至多一个有效）/品牌/概览/归档恢复/成员 invite-remove-role-transfer_owner/项目列表。API：`/api/v1/workspaces/*`（19 条）。
- **项目**：任务四态（todo/doing/review/done）、里程碑（reached ⟺ reached_at）、成员（⊆ 工作区成员）、频道关联（同工作区强制）、活动事件流（`project_event` 与业务写同事务）。API：`/api/v1/projects/*`（16 条）。
- **群组/频道 scope**：`personal | workspace` 两态、创建后不可变（迁移 77 XOR CHECK）。

**实现程度差异（CURRENT 内的边界）：**

- App 端「组织」是租户管理入口（创建组织/邀请/部门/成员管理），工作区切换由 `workspace_shell` 承载；Admin 端组织治理是平台运营面（suspend/remove 等，注意 Admin 面动作名 `remove` 对应 App 面动作名 `offboard`）。
- 群组/频道在个人域（personal scope）长期可用，组织层是后来叠加的归属维度——两层共存是设计事实，不是迁移未完成。

## TARGET（已裁定未完全闭合）

- 五概念边界仍有 5 个决策点未拍板（D1-D5），其中「Project ⊆ Workspace 触发器挡学员/作业三分裂」「License 挂实例」等杂交问题待产品最终裁定（原裁定文档为历史计划稿，已随计划目录移除）。**本节是 TARGET 层事实，不是现状描述。**
- 企业组织 V1 已并入 main（CHANGELOG `1.0.0-alpha.77`）；2026-09-18 用户曾裁定原实施计划的「正文完成结论」作废、仅作历史快照——该裁定的书面矩阵存于当时会话工作目录，**未入库**（UNKNOWN：仓内无此文件）。

## Constraints

- 组织删除必须先过 deletion-preflight；active owner 转移前不可移除 owner 成员行。
- 项目成员必须是 active 工作区成员（写入端与移除端双触发器）。
- personal 频道不能关联项目（复合 FK 拒绝）。

## References

- 冻结契约：`docs/architecture/2026-09-16-*.md` 系列（organization governance / compatibility）
- 迁移：76（workspace）、77（scope）、78/81（project）、95（organization foundation）、113-131（org 家族）
- 三端路由：见 [三端 API 对齐](https://github.com/imboy-pub/imboy/blob/main/docs/api-contracts/three-platform-alignment.md)
