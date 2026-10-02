# Seat 控制台嵌入 — 网关路由与部署增量 / Seat console embed — gateway routes & deploy delta

> 计划卡 SC-OPS（seat-console-embed，2026-09-28）。本文只描述**本地候选**的
> 路由拓扑与部署/回滚方法。
>
> **本地候选验证完成，生产未部署未授权。** 本文档不含任何生产变更声明。

---

## 1. 新增路由（deploy/nginx/templates/cs-widget.conf.template）

在既有 CS Widget 第三域 vhost（`CS_WIDGET_DOMAIN`）内追加，既有 Widget 面
location 块零改动：

| 路由 | 上游 | 说明 |
|---|---|---|
| `location ^~ /seat/` | `imboy_backend:9800` | Seat 控制台动态 frame HTML（`/seat/:public_seat_console_id`）。超时对齐 `/w/`（300s）；**网关零 add_header** |
| `location ~ ^/api/v1/cs/organizations/[0-9A-Za-z_-]+/seats/me/events$` | `imboy_backend:9800` | Seat 工作台 SSE（真实端点，`imboy_router` seat events 路由）。`proxy_buffering off; proxy_cache off; chunked_transfer_encoding on;` 读写超时 3600s（与 Widget SSE 同构） |
| `location /api/v1/cs/` | `imboy_backend:9800` | Seat workspace APIs（普通 JSON；SSE 正则优先级更高） |
| `location /api/v1/passport/qr_login/` | `imboy_backend:9800` | Seat 扫码登录 |
| `location /api/v1/enterprise/conversations/` | `imboy_backend:9800` | Seat 会话面 |
| `location /api/v1/enterprise/organizations/` | `imboy_backend:9800` | Seat 组织面 |
| `location ^~ /seat-assets/` | `imboy_widget:8080` | 稳定别名静态资产 `/seat-assets/cs-seat.v1.{js,css}` |

**代理面是精确枚举**：刻意**没有** `location /api/v1/` 通配（负例断言见
`scripts/test/cs_deploy_unit_test.sh` A07）。四组之外的一切 `/api/v1/*` 落回
`location /` → 静态容器 404（fail-closed）。

## 2. 安全/缓存头边界（与 Widget 面同一边界）

* `/seat/` frame：CSP frame-ancestors、XFO 豁免登记、`Cache-Control: no-store`
  **全部由 backend 逐请求下发**；网关零注入（多 CSP 头浏览器取交集，网关侧
  注入只会破坏嵌入或形同虚设）。
* `/seat-assets/`：缓存头（`no-cache, must-revalidate`，内容随发布原位更新）
  由 imboy_widget 静态容器内 nginx 按冻结合同（build-contract `seat`，
  `cache_policy.seat_assets`）下发；网关不覆写、不追加任何影响缓存的头。
* 请求体上限沿用 vhost server 级 `client_max_body_size 50m`（与 Widget 上传
  端点同一来源）。

## 3. 验证（全部离线/合成环境）

```bash
bash scripts/test/cs_deploy_unit_test.sh          # A07 模板合同：正例 + 6 组变异负例
bash scripts/test/customer_service_deploy_test.sh # CS 部署事务（vhost 由 cs_deploy.sh 生成，未触碰）
bash scripts/check_widget_asset_pairing.sh        # 配对门；seat_console 面由合同 JSON 增量自动纳入
bash deploy/widget/dryrun.sh --dist <dist-widget> # 产物清单 fail-closed（缺 seat-assets 即拒）
envsubst '${CS_WIDGET_DOMAIN}' < deploy/nginx/templates/cs-widget.conf.template | nginx -t 路径见 run 证据
```

dryrun 的 STEP 0.1 要求产物同时携带 Widget 面与 Seat 面
（`seat/index.html` + `seat-assets/cs-seat.v1.{js,css}` + manifest 校验和一致），
缺任一文件 fail-closed 退出 1。

## 4. 回滚 / Rollback

```bash
# 1) git revert 对 cs-widget.conf.template 的增量提交（或恢复上一版模板文件）
# 2) 重新拉起 nginx 容器（envsubst 重渲染；社区版 up -d imboy_nginx，商务版加 overlay）
docker compose --env-file .env -f docker-compose.community.yml up -d imboy_nginx
# 3) 验收：/seat/ 与 /seat-assets/* 恢复 404（路由面下线），Widget 面行为不变
```

Seat 资产缓存策略为 no-cache 重验证，回滚后已打开的 Seat 页刷新即取回旧路由
语义；backend frame 路由与数据面回滚不在本增量职责内。

---

## 5. `imboy.base_url` 硬约束（附件 presign 的上传基址）

坐席附件链的 presign 步骤以下发 `upload.url` 的**绝对地址**来自 `{imboy, base_url}`
（`src/features/enterprise_business/interfaces/eb_enterprise_http.erl` 的
`api_base_url()`）：

* **必须等于坐席可达的 API origin**，即本 vhost 的 `CS_WIDGET_DOMAIN`
  （`https://<CS_WIDGET_DOMAIN>`）。base_url 配错源会把 presign 下发的裸 PUT
  打向无关主机——本候选验收期实测（DEF-SC153-14）：base_url 指向无关主机时
  坐席附件链整条失败、零资产落库。
* **fail-closed 语义**：base_url 非 `https://` 时 presign 不下发 `upload.url`
  （为空），坐席端 `SeatUploadTargetMissingError` 终止上传，绝不自造端点。
  因此 base_url 配置错误的表现是"附件功能整体不可用"，而非降级到错误主机。
* **上线前检查**：从坐席网络环境 `curl -i https://<base_url>/api/v1/cs/...`
  必须命中本网关域（同证书、同域名）；建议在部署 preflight 中加入该比对
  （当前 preflight.sh 未覆盖，属已知加固项）。

---

## 6. 状态声明 / Status

* 本地候选验证完成（模板渲染 `nginx -t` 通过、两组离线 harness 全绿、
  dryrun 合成栈 install/restart/rollback 演练通过、配对门与 admin 仓三方一致）。
* **2026-10-02 生产已部署（用户授权联调）**：坐席路由经 `imboy-deploy.sh cs
  --skip-backend` 走 staged → nginx -t → 原子替换上线（release
  20261002232124 及此前多版）。直连形态渲染真源有两处与容器模板的刻意
  差异：(1) listen 为 `127.0.0.1:10443 ssl http2 proxy_protocol`（生产
  443 由 nginx stream 层 ssl_preread SNI 分流，非容器内 `listen 443`）；
  (2) 静态面由宿主机 `$CS_ROOT_REAL` 顶级软链直读（非 imboy_widget 容器）。
  网关级 IP 限流同批上线（cs_widget_per_ip 30r/m burst=10 对齐后端节流；
  cs_seat_per_ip 120r/m burst=40），zone 定义于本 vhost 文件顶部。
  渲染真源 = `scripts/lib/cs_deploy.sh:cs_render_vhost`（容器模板
  `deploy/nginx/templates/cs-widget.conf.template` 与其保持路由族一致，
  listen/静态面按形态分叉；断言函数 `cs_assert_vhost_render` 防再漂移）。
