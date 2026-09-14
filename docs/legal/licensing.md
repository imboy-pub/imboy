# IMBoy 许可策略 / IMBoy Licensing

> 生效日期：2026-09-14
> 适用范围：2026-09-14 及之后首次公开发布的版本
> 相关：根 [`LICENSE`](../../LICENSE) ｜ [`MulanPSL-2.0.txt`](./MulanPSL-2.0.txt)（历史授权文本）｜ [CONTRIBUTING.md](../../CONTRIBUTING.md)

---

## 三端许可总览 / License matrix

| 项目 | 许可证 | 简单理解 |
|------|--------|----------|
| `imboy`（后端） | **BSL 1.1** | 源码公开，但限制竞争性商业使用 |
| `imboy-admin-frontend`（管理后台） | **BSL 1.1** | 源码公开，但限制竞争性商业使用 |
| `imboy-flutter`（移动客户端） | **木兰宽松许可证第 2 版**（MulanPSL-2.0） | 真正意义上的开源 |

**三端许可为什么不同。** BSL 1.1 的杠杆作用在服务端——托管运营、多租户 SaaS、与官方付费版竞争的商业化行为都发生在后端与管理后台，因此这两端采用 BSL 1.1。移动客户端不含可被托管运营的服务端能力，且端到端加密的可审计性要求客户端源码完全开放，故保持 MulanPSL-2.0 这一真正的开源许可证。

---

## BSL 1.1 实际给了什么 / What BSL 1.1 permits

BSL 1.1 的默认授予是"复制、修改、创作演绎作品、再分发，以及**非生产用途**"。生产用途默认不授予，仅授予 `LICENSE` 中 Additional Use Grant 明确许可的部分。

### ✅ 免费，无需商业授权

| 用途 | 依据 |
|------|------|
| 评估、开发、测试、演示、教学、CI 等非生产用途 | BSL 1.1 默认授予 |
| 组织内部生产使用（含 50% 控股口径下的关联实体、员工与承包商） | Additional Use Grant |
| 在你自己控制的基础设施上部署与运维 | Additional Use Grant |
| 修改源码、集成进自有产品 | BSL 1.1 默认授予 + Additional Use Grant |
| 为**单一**最终客户组织部署的专有实例（含代为运维） | Additional Use Grant |
| 复制与再分发（含修改版） | BSL 1.1 默认授予 |

### 🔑 需要商业授权

| 用途 | 说明 |
|------|------|
| 多租户托管服务（SaaS） | 单次部署服务多个最终客户组织，或按自服务/订阅制向公众开放 |
| 与官方付费版竞争的付费产品或服务 | 含以付费支持形式提供的竞争性服务 |
| 白标转售、以 IMBoy 名称/Logo 做商业产品 | 见下方「商标」一节 |

> **判据提示**：Additional Use Grant 用"单租户 / 多租户"作为 SaaS 的判据，而不是"是否收费"——为单一客户做的私有部署即使收费也是允许的，一套实例服务多个客户即使免费也不在授予范围内。

### ⏳ 自动转开源

每个版本自其首次公开发布之日起，四年后（以先到者为准）自动转为 **MPL 2.0** 授权，上述限制随之终止。该承诺写在 `LICENSE` 的 Covenants of Licensor 第 1 条，对全部接收者有效。

---

## 版本分界 / Version boundary

**2026-09-14 之前**公开发布的版本按 [木兰宽松许可证第 2 版](./MulanPSL-2.0.txt)（MulanPSL-2.0）授权。该文件作为历史授权文本保留在 `docs/legal/`。

> ⚠️ **不要把该文件移回仓库根目录，也不要改回 `LICENSE-*` 一类的名字。** 代码托管平台的许可证扫描会把根目录下任何 `LICENSE*` / `COPYING*` 文件识别为一个独立许可证。由于这类平台普遍**识别不出 BSL 1.1**（会被归入 "Other"），一旦历史文本出现在根目录，仓库侧边栏里**唯一被具名的许可证就会变成最宽松的那个**，可能被误读为"双授权、可择一适用"。

**2026-09-14 及之后**首次公开发布的版本按 [BSL 1.1](../../LICENSE) 授权。

> ⚠️ **许可证变更不具追溯力。** MulanPSL-2.0 授予的是"永久性的、全球性的、免费的、非独占的、**不可撤销的**"授权。已经取得 2026-09-14 之前版本的人，对该版本仍享有不受限制的商用、修改与再分发权利，BSL 1.1 无法收回。本次变更只约束新版本。

---

## 与客户端 AGPL 依赖的关系 / Client-side AGPL dependency

`imboy-flutter` 当前依赖两个 **AGPL-3.0** 包（`flutter_vodozemac`、`vodozemac`，E2EE 的 Olm/Megolm 实现），12 个源文件直接 import。AGPL-3.0 的分发义务在对外分发时触发。

因为服务端（BSL 1.1）与客户端（MulanPSL-2.0）是**分别交付**的独立程序，服务端的 BSL 1.1 授权不受客户端 AGPL 义务影响。但客户端自身声明的 MulanPSL-2.0 与其 AGPL 依赖存在张力——**这是客户端侧独立存在的未决事项**，处置方案（自建 Apache-2.0 FFI 绑定 / 购买上游商业授权 / 客户端改为 AGPL）见 [第三方依赖许可证清单](./third-party-licenses.md) 与 `docs/roadmap/security-roadmap.md`。

---

## 商标 / Trademark

BSL 1.1 的正文明确**不授予**许可方及其关联方的商标、服务标记或产品名称的任何权利。"IMBoy" 名称与 Logo 的使用权限独立于代码许可证，任何商业产品均不得使用。详见 [`docs/brand/README.md`](../brand/README.md)。

---

## 贡献 / Contributions

贡献以 BSL 1.1 授权给项目（DCO 形式确认）。依 BSL 1.1 的条款，全部代码——含所有贡献——将在 Change Date 自动转为 MPL 2.0，无需额外授权动作。若日后需要变更 Change License 或增加商业双授权，则需另行取得贡献者同意；**引入外部贡献之前应先引入 CLA**。详见 [CONTRIBUTING.md](../../CONTRIBUTING.md)。

---

## 变更记录 / Change log

| 日期 | 变更 |
|------|------|
| 2026-09-14 | 后端 `imboy` 与管理后台 `imboy-admin-frontend` 由 MulanPSL-2.0 切换为 BSL 1.1（Change License: MPL 2.0）；移动客户端 `imboy-flutter` 保持 MulanPSL-2.0。旧许可文本保留为 `docs/legal/MulanPSL-2.0.txt`（不放根目录：避免被托管平台的许可证扫描识别为第二个许可证）。 |
