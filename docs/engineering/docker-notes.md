# Docker 布局笔记（Docker Notes）

> 工程视角 · 描述现状 + 增量改进

## 现状

**后端 Dockerfile**(`imboy/Dockerfile`):多阶段——Stage 1 `erlang:28` builder(含 wx 供 relx 组装 observer),`sed` 关闭 `dev_mode`(避免符号链接断链),`make rel` 拉依赖+编译+relx 组装 ERTS 自包含;Stage 2 debian-slim runtime,仅拷 release + ERTS 所需系统库(架构自适应 amd64/arm64)。`.dockerignore` 排除 `_build/deps/_rel/.git`。

**Admin Dockerfile**:`oven/bun:1-slim` builder → nginx serve;构建参数(`VITE_API_BASE_URL` 等)build 时注入。

**编排**：Git 跟踪 `imboy/deploy/helm/`，覆盖 backend/admin Service、Deployment、Ingress、HPA 与配置/密钥模板。本机另有 12 服务的生产 Compose 草稿，但它被 `imboy/.gitignore:43` 排除，不能作为买家或 CI 可复现的交付证据。

## 优点

- 多阶段构建,runtime 镜像不含编译器/源码,自包含 ERTS,体积与攻击面小。
- Dockerfile 注释详尽(构建/运行说明、每阶段用意),可维护性高。
- 架构自适应(amd64/arm64),`.dockerignore` 强制干净构建。
- build-arg 注入前端配置,镜像不可变、环境无关。
- Helm 业务服务编排已版本化；Prometheus/Loki/Promtail/Alertmanager 等配置文件也有部分进入 Git。

## 潜在改进

1. **补齐可复现的生产编排**(高):要么把脱敏后的生产 Compose 文件纳入版本控制，要么明确只支持 Helm，并删除文档中的 Compose 一键部署承诺。
2. **镜像扫描进 CI**(优先级低-中,增量):对构建产物做漏洞扫描(trivy/grype),纳入发布门。
3. **非 root 运行**(中):确认 runtime 容器以非特权用户运行(最小权限);若当前 root,增量改为专用用户。
4. **调试工具与镜像瘦身**(低):结合 dependency-notes,确认生产 release 是否 bundle 了 observer/sync 等,可进一步瘦身。
5. **健康检查**(低):为 backend/admin 容器加 `HEALTHCHECK`,配合编排与蓝绿切换。
6. **镜像标签规范**(低):与版本双源对齐(见 release-notes),tag 用 VERSION 单一真源。

## 相关模块

`imboy/Dockerfile`、`imboy/.dockerignore`、`imboyadmin/Dockerfile`、`imboy/deploy/helm/`、`imboy/deploy/nginx/`、`imboy/.gitignore:43`

## 优先级

| 建议 | 优先级 |
|---|---|
| 补齐可复现生产编排或明确仅支持 Helm | 高 |
| 容器非 root 运行确认 | 中 |
| 镜像漏洞扫描进 CI | 中 |
| 镜像瘦身(剥调试工具) | 低 |
| 容器 HEALTHCHECK | 低 |
| 镜像标签对齐版本单一真源 | 低 |
