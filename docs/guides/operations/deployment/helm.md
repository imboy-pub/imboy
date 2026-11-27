# Helm 部署 / Helm deployment

Helm 适合已有 Kubernetes 集群、需要评估 IMBoy 的团队。当前 Chart 属实验性，生产交付使用单机 Docker Compose。后端保持一个副本并关闭后端 HPA。跨 Pod 的消息路由和节点注册仍待验证。

Use Helm to evaluate IMBoy on an existing Kubernetes cluster. The chart is experimental. Supported production delivery uses single-node Docker Compose. Keep the backend at one replica with backend autoscaling disabled while cross-Pod routing and registration remain unverified.

## 开始前 / Before you start

| 准备 / Input | 用途 / Purpose |
|---|---|
| 外部 PostgreSQL / External PostgreSQL | 业务数据库，由现场维护 / Site-managed application database |
| Ingress 与证书 / Ingress and certificates | API 和管理后台入口 / API and Admin endpoints |
| 受保护的 `site-values.yaml` / Protected values file | 固定镜像、域名、数据库和 Secret 配置 / Pinned images, domains, database, and Secret settings |

后端与附件 Edge 共用一个独立密钥。使用至少 32 个字符的非占位值，并与 JWT、数据库加密及 Cookie 密钥区分。引用现有 Secret 时须包含 `IMBOY_EDGE_INTERNAL_TOKEN`。由 Chart 生成 Secret 时，须提供 `secrets.postgresUser`、`secrets.postgresPassword`，以及授予 `v5_worker` 的专用清理账号 `secrets.directCleanupPgUser`、`secrets.directCleanupPgPassword`。外部 Secret 使用相同的两组账号。

Give the backend and attachment Edge the same dedicated non-placeholder token of at least 32 characters. Use separate JWT, database encryption, and cookie keys. An existing Secret must contain `IMBOY_EDGE_INTERNAL_TOKEN`. When the chart creates the Secret, supply `secrets.postgresUser`, `secrets.postgresPassword`, and a separate `v5_worker` login through `secrets.directCleanupPgUser` and `secrets.directCleanupPgPassword`. Supply both identities in the external Secret if you manage it separately.

## 预览和验证 / Preview and verify

从仓根渲染。输出可能包含 Secret，请在受控终端查看。复用既有 Secret 的升级要在获准集群执行服务端 dry-run，以便模板读取该 Secret。升级镜像须包含 `/opt/imboy/bin/start_download_edge.sh`。

Render from the checkout root. Treat the output as sensitive. Use an authorized server-side dry-run for upgrades that reuse an existing Secret. The upgraded backend image must include `/opt/imboy/bin/start_download_edge.sh`.

```bash
helm template imboy ./deploy/helm -n imboy \
  -f ./deploy/helm/values.prod.yaml -f ./site-values.yaml
```

评估环境获准后，从仓根使用相同的参数安装。 / Once authorized, install from the checkout root using the same values.

```bash
helm upgrade --install imboy ./deploy/helm -n imboy --create-namespace --wait \
  -f ./deploy/helm/values.prod.yaml -f ./site-values.yaml
kubectl get pods,ingress -n imboy
```

获准部署后，检查 Pod 就绪、Ingress 的 HTTPS 路由和业务登录。Chart 的默认值在 `deploy/helm/values.yaml`，生产覆盖示例在 `values.prod.yaml`。支持范围见[版本与交付边界](../../../business/edition-boundary.md)。

After an authorized deployment, verify Pod readiness, HTTPS ingress, and application login. See the chart values for configuration and the edition guide for support boundaries.
