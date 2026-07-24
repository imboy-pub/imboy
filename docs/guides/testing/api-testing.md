# API 契约测试（API Testing）

## 为什么需要
评审证 REST 契约三端漂移严重(SDK 5 项漂移致登录/握手/端点全断;OpenAPI 仅覆盖 ~130/278 路由;billing 越权)。API 测试保证请求/响应契约在后端、app、admin、SDK 四端一致,且鉴权/授权正确。

## 覆盖范围
- 请求契约:字段名(pwd vs password)、类型、必填、TSID(JSON integer)、rsa_encrypt 默认值
- 响应契约:统一信封(success/data/error/meta)、错误码、TSID 回传
- 鉴权:JWT 校验、设备签名门、open/option 豁免矩阵
- 授权:对象级归属(billing current_uid、租户隔离)
- 覆盖全部 278 路由(对齐 OpenAPI)

## 推荐框架
- 后端:CT + cowboy 请求模拟(`test/common/cowboy_req_h.erl`)测 handler;真 PG 测端到端
- 契约金标:OpenAPI(`api/openapi.yaml`)作单一真相,`schemathesis`/契约测试对齐三端
- SDK:vitest E2E 打真后端

## 目录结构
```
imboy/test/{api,adm}/*_handler_tests.erl
imboy/api/openapi.yaml(金标)
imboy-sdk-js/test/contract/
```

## Mock 策略
handler 层测鉴权/参数用 cowboy_req mock;契约/授权用真 handler + 真 PG。**授权测试绝不 mock**(否则 billing 越权测不出)。

## Fixture 策略
每端点造合法/非法/越权三类请求;多租户账号 fixture(A 租户 token 访问 B 租户对象应拒)。响应用 OpenAPI schema 断言。

## 数据准备
真 PG 播种多用户/多租户;JWT 由 `token_ds:encrypt_token(Uid)` 造;越权用例造跨归属数据。

## CI 执行方式
Stage 2 集成门(handler+PG)+ Stage 1 契约 diff 门(OpenAPI 覆盖率 + proto diff,ARCH-03)。SDK E2E 进发版门禁(ARCH-04)。

## 覆盖率要求
Handler 60%;**授权路径 100%**(每个认证后端点必测越权拒绝);OpenAPI 路由覆盖从 47% 提至 100%。

## 验收标准
- [ ] 278 路由全部有契约测试且对齐 OpenAPI
- [ ] 每认证端点有越权拒绝测试(billing 9 端点优先)
- [ ] 请求字段名/类型/TSID 四端一致(契约门拦漂移)
- [ ] SDK 登录→握手→收发 E2E 绿
- [ ] 响应信封/错误码 schema 断言
