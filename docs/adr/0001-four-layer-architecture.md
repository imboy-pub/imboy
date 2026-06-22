# 0001 — 后端四层单向依赖架构

- 状态：Accepted
- 关联目录：`src/api/`、`src/logic/`、`src/ds/`、`src/repo/`

## 背景
即时通讯后端的 HTTP/WS 处理若把鉴权、业务规则、SQL 拼装混在 handler 里，会导致难测试、难复用、改一处牵连全身。需要一条清晰且**可机械校验**的分层边界。

## 决定
后端代码严格分四层，依赖**只能自上而下**，不得反向或跨层穿透：

```
api (Cowboy handler)  →  logic (业务规则)  →  ds (data service)  →  repo (SQL/存储)
```

- `api/`：HTTP/WS 入口，仅做参数解析、鉴权（JWT / WS token）、响应封装，调用 logic。
- `logic/`：业务规则与编排，不直接写 SQL。
- `ds/`：数据服务，聚合 repo、处理缓存与组装。
- `repo/`：数据访问，SQL 一律参数化。

新增 API 端点的标准流程：`src/api/` handler → `imboy_router.erl` 注册 → `src/logic/` 逻辑 → 写 EUnit 测试。

## 后果
- ✅ 各层可独立用 meck mock 测试（Repo 80% / Logic 70% / Handler 60% 覆盖目标）。
- ✅ 边界由脚本 `scripts/check_module_boundaries` 做门禁，违规可在 CI 拦截。
- ⚠️ 跨多模块改动前必须先确认调用方向，禁止 logic 直接写 SQL 或 api 直连 repo。
