# 解释（Explanation）

> **本目录定位**：理解导向。讲清「为什么这样设计」：背景、约束、被否决的方案、权衡。

**写作要求**：
- 可以有观点，但必须标注权衡（「选择了 A 牺牲了 B，因为……」）
- 引用 ADR 编号，不复制 ADR 内容
- 不放操作步骤（那是 guides/ 的事），不堆参数表（那是 reference/ 的事）

**判断标准**：如果读者看完能回答「为什么不是别的方案」，这篇就成功了。

## 现有文档

| 文档 | 回答的问题 | 关联 |
|------|-----------|------|
| [product-profile-and-plugin-registry-design.md](./product-profile-and-plugin-registry-design.md) | 产品画像与插件注册中心为什么这样设计 | plugin/ 规范 |
| [current-module-classification.md](./current-module-classification.md) | 现行模块分类的全景与归类逻辑 | architecture/module-map.md |
| [ai-companion-flutter-ui.md](./ai-companion-flutter-ui.md) | AI 助手冷启动的 Flutter UI 设计蓝图 | — |

## 相关入口

- [architecture/](../architecture/)：架构活跃文档（overview、module-map、database-access、module-layer-cheatsheet）
- [archive/architecture/](../archive/architecture/)：已完成的架构历程（DDD 迁移状态、fastdfs→Garage 迁移、资源访问控制演进）

## 待补（Phase 4 深化）

| 主题 | 回答的问题 | 素材来源 |
|------|-----------|---------|
| 四层架构设计理由 | 为什么 Handler→Logic→DS→Repo 单向依赖 | ADR-0001 + module-layer-cheatsheet |
| TSID 选型 | 为什么不用自增 ID / UUIDv4 | ADR-0004 + CONVENTIONS |
| E2EE 密钥轮换设计 | 密钥怎么换、为什么这样换 | guides/e2ee/ |
| Garage S3 直传设计 | 为什么附件不走后端中转 | archive/architecture/fastdfs-to-garage-migration.md |
| 插件系统隔离模型 | 插件为什么不能直接碰核心 | reference/plugin/ |
