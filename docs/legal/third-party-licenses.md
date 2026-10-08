# 第三方依赖许可证清单与合规结论

> 对应 E2EE gap-matrix **D3**（依赖清单缺失 / AGPL 未标注）与 **X5/X15**（AGPL 发布门）。
> 技术来源核对：2026-10-08；许可证处置要求仍沿用原发布门，未作新的法律判断。

仓内 **[third-party-licenses.generated.md](./third-party-licenses.generated.md)** 是历史清单快照，不能证明当前锁文件或交付产物已完成许可证核查。当前扫描结果保存在仓库外，并绑定候选版本与依赖锁文件。

重新生成：

```bash
cd imboy
scripts/license_inventory.sh > /tmp/imboy-third-party-licenses.md
scripts/license_inventory.sh --check      # 门禁：发现强 copyleft 退出 1
scripts/license_inventory.sh --selftest   # 校验判别函数本身（10 条样本，含无标题行的裸 MIT）
```

判定依据是磁盘上真实的 LICENSE **正文**，不是包管理器元数据——元数据可以填错，
正文不会。

## 覆盖范围

| 范围 | 状态 |
|------|------|
| imboy Erlang 运行时依赖（`deps/*`） | 脚本扫描本机依赖副本；数量以当前结果为准 |
| imboyapp Flutter 直接依赖（`direct main`） | 脚本扫描 path/git/sdk 来源；数量以当前结果为准 |
| Flutter 传递依赖 | ❌ 未扫——随包分发，发布前需补 |
| imboyadmin（npm） | ❌ 未扫 |

**未扫的部分是真未扫**，不要把本文当成"依赖已全部合规"的凭据。

## 历史快照的许可证分布（不代表当前交付产物）

| 许可证 | 数量 |
|--------|------|
| MIT | 59 |
| BSD（2/3-Clause） | 47 |
| Apache-2.0 | 25 |
| ISC | 6 |
| MPL-2.0 | 1 |
| LGPL-2.1（带链接例外） | 1 |
| **AGPL-3.0** | **2** ⛔ |
| UNKNOWN | 2 ⚠️ |

绝大多数是宽松许可证，可直接用于闭源商业分发。以下 4 项需要动作。

## ⛔ 阻断项：AGPL-3.0

| 包 | 版本 | 位置 |
|----|------|------|
| `vodozemac` | 0.8.0 | imboyapp `pubspec.lock` 的同名依赖条目 |
| `flutter_vodozemac` | 0.8.1 | imboyapp `pubspec.lock` 的同名依赖条目 |

这是 E2EE 的 Olm/Megolm 实现，功能上无法直接摘掉。AGPL-3.0 的网络条款要求
向使用者提供**完整对应源码**，与私有化售卖的商业模式不相容。

**已拍板处置（2026-08-02，R4 裁定 ③）**：基于上游 **Apache-2.0 的 vodozemac Rust
crate** 自建 FFI 绑定，保持 `fvod` 调用面兼容以使 `lib/` 零改动。追踪项 **X15**。

⚠️ **在 X15 落地前，产品不得对外分发**——包括试用版、演示 APK、应用商店上架。
内部使用与开发不触发 AGPL 的分发义务。

## ⚠️ 需查明：无许可证授权的依赖

当前副本中，`simple_captcha` 没有项目级许可证正文，`ic_storage_space` 的 LICENSE 只有版权声明。来源以实际 Git revision 与 Flutter 锁文件为准；不能从 fork 地址推断授权。
**没有许可证 = 没有再分发权**，性质上和 AGPL 一样是分发阻断项，只是更容易解决。

| 包 | 现状 | 动作 |
|----|------|------|
| `simple_captcha` | 无 LICENSE 正文，`app.src` 的 `{licenses,[]}` 为空 | 查明上游出处并补回 LICENSE；查不到则替换或自研 |
| `ic_storage_space` | LICENSE 文件内容仅有 `Copyright 2021`，无任何授权条款 | 同上 |

## ℹ️ 已澄清：LGPL 不构成阻断

`gpb`（protobuf 编译器）是 LGPL-2.1，但其 `COPYING.LIB` 开头写死了两条例外：

1. 允许与其他许可证（含 Apache-2.0）的代码链接；
2. **该许可证不覆盖 gpb 生成的代码**。

我们只用它编 `.proto`，生成物归我们所有，属例外范围内，无需开源自身代码。

## 进 CI 的时机

`--check` 现在必然退出 1（两个 AGPL 包还在）。**X15 完成后**再把它接进
`backend-ci.yml`，否则一上来就是红的，红久了就没人看了。届时同步补 `NOTICE` 文件。
