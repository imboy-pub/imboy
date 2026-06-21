# go-fastdfs → Garage 历史附件迁移方案

> 状态：**待决策**（未执行）｜创建 2026-06-21｜对应 commit 5261267 之后的遗留项

## 1. 现状：双后端半迁移态

- **后端已不写 fastdfs**：Erlang 源码零引用，唯一残留是 `config/nginx-imboy.conf:78` 的 fastdfs 反代（仅供旧文件读取）。新上传全部走 `elib_oss`（Garage）。
- **fastdfs 仍存历史附件**：prod 容器 `imboy_fastdfs` Up 中，物理卷 `/data/docker/img_fastdfs_data` = **370 文件 / 320M**。
- **读路径并存**：旧文件经 fastdfs URL（`/img/...`、`/audio/...`、`/files/...`，存于各表 `url` 字段）+ nginx 反代；新文件经 Garage `attach_logic:view_url` 短时签名。

## 2. 核心难点：引用散落（非数据量）

prod `attachment` 表仅 **10 条** fastdfs 记录，但物理卷有 **370 个文件** → 约 360 个文件的 URL **不在 attachment 表**。迁移的真正工作量在于「找全所有引用 fastdfs URL 的字段」，而非搬运 blob。

**必须先做引用普查**（迁移前置，逐表确认存量 fastdfs URL）：

| 候选引用源 | 字段 | 排查方式 |
|---|---|---|
| `attachment` | `path` / `url` | `WHERE path NOT LIKE 'u%'` |
| `user` | `avatar` | 旧头像可能是 fastdfs URL |
| `group_file` / `group_album` | 文件/图片 URL | 群文件、相册 |
| `moment_post` | 图文/视频 URL | 朋友圈 |
| 消息存储（`msg_archive` 等） | payload 内的 image/file/audio URL | 历史消息附件（最难，URL 嵌在 JSON payload） |

> ⚠️ 消息 payload 里的 fastdfs URL 是最大不确定项：旧消息已投递，客户端本地可能缓存原始 URL；重写历史消息 payload 风险高，通常**只迁物理文件 + 保留 URL 可达**，不重写消息。

## 3. 推荐策略：保 URL 可达，而非重写引用

鉴于引用散落且消息 payload 不宜改写，推荐 **「物理迁移 + 读兼容」**，不做全量 path 重写：

1. **双读兼容**（代码）：`view_url` / 下载层对 `path NOT LIKE 'u%'` 的旧记录，回退走 fastdfs URL（现状已是如此，确认覆盖所有读入口）。
2. **物理搬迁**：370 文件从 fastdfs 卷批量上传到 Garage 的兼容桶，**保持相同相对路径**（如 `/img/202512/...` → Garage key `legacy/img/202512/...`）。
3. **反代切换**：nginx 把原 fastdfs 路由（`/img/`、`/audio/`、`/files/`）改为反代到 Garage 兼容桶（路径映射），fastdfs URL 对客户端**保持不变** → 零客户端改动、零消息改写。
4. **观察期**：保留 fastdfs 容器 + 卷只读运行 N 周，对比 Garage/fastdfs 命中。
5. **下线**：观察无回退后，停 `imboy_fastdfs` 容器、移除 nginx fastdfs 反代、归档物理卷。

该策略下 attachment 表那 10 条旧记录可选迁（量小），核心是 nginx 路径映射让旧 URL 透明指向 Garage。

## 4. 风险与回滚

| 风险 | 缓解 |
|---|---|
| 漏迁某些引用源的文件 | 引用普查 + 以物理卷 370 文件为全集校验（搬完比对文件数/大小） |
| nginx 路径映射与 Garage key 命名不一致 → 旧 URL 404 | 搬迁时严格保持相对路径；切换前用旧 URL 抽样回归 |
| 消息 payload 内 URL 失效 | 不改 payload；靠 nginx 让旧 URL 继续可达 |
| 下线过早丢历史文件 | 容器+卷只读保留至观察期满；卷先 `backup_garage` 同款备份 |

回滚：任一步失败，nginx 反代切回 fastdfs，容器/卷未删 → 秒级恢复。

## 5. 执行需另行授权

本文档仅为方案。涉及 prod 物理数据搬迁 + nginx 切换 + 容器下线，属不可逆外向操作，**执行前需逐阶段人工确认**。建议先单独做「引用普查」（只读），用真实引用清单收敛方案再动手。
