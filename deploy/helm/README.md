# IMBoy Helm Chart

将 IMBoy IM 平台部署到 Kubernetes 集群的 Helm chart。

## 架构

```
Internet → nginx Ingress Controller → imboy-backend (Erlang/OTP)
                                    → imboy-admin   (React/Vite 静态)
外部 PostgreSQL（不由本 chart 管理）
```

## 前置条件

| 组件 | 说明 |
|------|------|
| Kubernetes | >= 1.27 |
| Helm | >= 3.12 |
| nginx ingress controller | `kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/cloud/deploy.yaml` |
| cert-manager | `helm install cert-manager jetstack/cert-manager --set installCRDs=true` |
| metrics-server | HPA 依赖，通常云托管集群已预装 |
| 外部 PostgreSQL | RDS / Cloud SQL / 自建（不在本 chart 内） |

## 快速开始

### 1. 准备命名空间

```bash
kubectl create namespace imboy
```

### 2. 创建密钥（推荐使用 external-secrets，此处演示手动方式）

```bash
kubectl create secret generic imboy-release-imboy-secrets \
  -n imboy \
  --from-literal=POSTGRES_USER=imboy_user \
  --from-literal=POSTGRES_PASSWORD='your-strong-password' \
  --from-literal=POSTGRES_DB=imboy_pro \
  --from-literal=IMBOY_JWT_KEY='32-byte-random-key' \
  --from-literal=IMBOY_POSTGRE_AES_KEY='32-byte-random-key' \
  --from-literal=IMBOY_ADM_COOKIE_SECRET='32-byte-random-key' \
  --from-literal=GF_SECURITY_ADMIN_PASSWORD='grafana-password' \
  --from-literal=SENTRY_DSN=''
```

### 3. 安装

```bash
helm upgrade --install imboy ./deploy/helm \
  -f ./deploy/helm/values.prod.yaml \
  -n imboy \
  --wait
```

### 4. 验证

```bash
kubectl get pods -n imboy
kubectl get ingress -n imboy
```

## 配置说明

### 核心参数

| 参数 | 默认值 | 说明 |
|------|--------|------|
| `backend.replicaCount` | 2 | 后端副本数 |
| `admin.replicaCount` | 1 | 前端副本数 |
| `ingress.api.host` | api.example.com | 后端 API 域名 |
| `ingress.admin.host` | admin.example.com | 管理后台域名 |
| `externalDatabase.host` | postgres.example.com | 外部 PG 主机 |
| `hpa.backend.maxReplicas` | 10 | 后端最大副本数 |

### 镜像版本升级

```bash
helm upgrade imboy ./deploy/helm \
  -f ./deploy/helm/values.prod.yaml \
  --set backend.image.tag=1.1.0 \
  --set admin.image.tag=1.1.0 \
  -n imboy
```

### 临时扩容

```bash
kubectl scale deployment imboy-release-imboy-backend --replicas=5 -n imboy
```

## 目录结构

```
deploy/helm/
├── Chart.yaml              # Chart 元数据
├── values.yaml             # 默认值（开发/CI 参考）
├── values.prod.yaml        # 生产环境覆盖（不含密钥）
├── templates/
│   ├── _helpers.tpl        # 通用模板函数
│   ├── NOTES.txt           # 安装后提示
│   ├── configmap.yaml      # 非敏感运行时配置
│   ├── secret.yaml         # 数据库密码、业务密钥
│   ├── deployment-backend.yaml   # Erlang 后端 Deployment + PVC
│   ├── deployment-admin.yaml     # React 前端 Deployment
│   ├── service-backend.yaml      # ClusterIP Service（后端）
│   ├── service-admin.yaml        # ClusterIP Service（前端）
│   ├── ingress.yaml              # nginx Ingress（对应 Caddyfile）
│   └── hpa.yaml                  # 自动扩缩容
└── README.md
```

## 与 Docker Compose 的对应关系

| Docker Compose 服务 | Kubernetes 资源 |
|---------------------|----------------|
| `imboy_backend` | Deployment `*-backend` + Service + Ingress |
| `imboy_admin` | Deployment `*-admin` + Service + Ingress |
| `imboy_pg18` | 不部署（外部 managed DB） |
| `imboy_caddy` | nginx Ingress Controller 替代 |
| `imboy_prometheus` | 建议使用 kube-prometheus-stack |
| `imboy_grafana` | 建议使用 kube-prometheus-stack |
