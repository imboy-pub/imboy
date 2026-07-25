# IMBoy 发布指南 / Release Guide

> 串联版本、镜像构建、版次、部署、部署后校验的完整发布流程。
> 配套：[edition-boundary.md](../../business/edition-boundary.md) ｜ [../../../deploy/README.md](../../../deploy/README.md) ｜ [../operations/deployment/day1-quickstart.md](../operations/deployment/day1-quickstart.md)

---

## 1. 版本号机制 / Versioning

**单一真相源：`imboy/VERSION`**（当前 `1.0.0-rc.1`）。
- `Makefile` 的 `PROJECT_VERSION` 读取 `VERSION`（`$(shell cat VERSION)`）。
- `deploy/.env.example` 的 `IMBOY_VERSION` 与之保持一致；compose 镜像 tag 默认 `${IMBOY_VERSION}`。
- `relx.config` 的版本号需手动与 `VERSION` 对齐；CI 打包时用 `RELX_REL_VSN=$(cat VERSION) make rel` 覆盖兜底。

**升级版本（bump）**：改 `VERSION` 一处 → 同步 `relx.config` → 打 tag `v$(cat VERSION)`。

---

## 2. 镜像构建 / Image Build

### 后端 imboy-backend（本仓）
多阶段 Dockerfile（OTP 28 编译 + relx 自包含 release + debian-slim 运行）：

```bash
cd imboy
docker build -t imboy/imboy-backend:$(cat VERSION) .
```

要点：
- builder 不设 `IMBOYENV`，用 `relx.config` + 完整 `config/sys.config`；Makefile 自动生成 `config/sys.runtime.config`。
- 构建期关闭 `dev_mode`（sed `relx.config`）出自包含 release，默认 `include_erts=true`。
- runtime **不依赖 apt**：从 builder 拷贝 `libssl/libcrypto/ncurses/tinfo` + CA 证书，适配受限网络的私有化客户。

### 管理后台 imboy-admin（独立仓）
`imboy-admin-frontend/Dockerfile`（Bun 构建 + Nginx）已就绪，在该仓内构建：

```bash
cd imboy-admin-frontend
docker build --build-arg VITE_API_BASE=https://api.yourdomain.com -t imboy/imboy-admin:$(cat ../imboy/VERSION) .
```

### 自动化发布（CI）
- 镜像构建发布工作流 `release.yml`（打 tag → 构建推送 GHCR）规划于 **commercialization-readiness 计划 Task C1**；本仓后端 Dockerfile 已为其就绪。
- **C1 落地前的过渡**：本地 `docker build` 后手动推送：
  ```bash
  docker tag imboy/imboy-backend:$(cat VERSION) ghcr.io/<your-org>/imboy-backend:$(cat VERSION)
  docker push ghcr.io/<your-org>/imboy-backend:$(cat VERSION)
  ```

---

## 3. 版次 / Edition

部署时设 `IMBOY_EDITION`（默认 `community`）。功能边界与商业分层见 [edition-boundary.md](../../business/edition-boundary.md)。社区版即本开源仓全部能力；专业版/企业版功能为闭源商业模块。

---

## 4. 部署 / Deploy

一键部署五步见 [../../../deploy/README.md](../../../deploy/README.md)：preflight → 配 `.env` → `docker compose up -d` → 查日志 → `/setup` 向导建管理员。

---

## 5. 部署后校验 / Post-deploy Sanity

```bash
bash scripts/sanity_check.sh            # 完整 8 项
bash scripts/sanity_check.sh --skip-tls # 本地无域名/TLS 时
```
任一 `[ERROR]` 退出码非 0。校验项：容器/PG/后端 HTTP/启动日志/迁移/admin/nginx/Grafana。

---

## 6. 发版 Checklist

- [ ] `VERSION` 已 bump，`relx.config` 已对齐
- [ ] `docker build` 后端镜像成功（本地或 CI）
- [ ] admin 镜像在 `imboy-admin-frontend` 仓构建成功
- [ ] 镜像已推送 registry（GHCR 或私有）
- [ ] 全栈 `docker compose up -d` 起来
- [ ] `bash scripts/sanity_check.sh` 全绿
- [ ] `/setup` 向导可建管理员并登录
- [ ] 打 tag `v$(cat VERSION)` 并更新 CHANGELOG
