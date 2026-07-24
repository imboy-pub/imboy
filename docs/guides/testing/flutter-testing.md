# Flutter 客户端测试（Flutter Testing）

## 为什么需要
794 dart 文件、三套运行时并存、巨型文件(chat_page.dart 2234 行)、autoDispose 陷阱(67 Notifier 复发面)、DDL 三镜像漂移、消息可靠性(message_retry 前 100 条截断)。客户端是用户直接感知面,测试须覆盖状态、渲染、本地库、离线可靠性。

## 覆盖范围
- 单元:service/store/utils/model、状态 Notifier 纯逻辑、编解码(见 unit/websocket)
- widget:关键页渲染、暗色主题、气泡、输入框(autoDispose 数据不丢)
- 本地库:sqflite 迁移(schema 三镜像一致、降级失败显式)、消息表 CRUD、msg_c2c.id 类型
- 状态:Riverpod provider 生命周期(autoDispose read vs listen)、build() 不覆盖 state
- 网络:Dio interceptor(presign 跳 JWT)、WS 客户端解码、出站确认状态机(MessageRetry 全量扫描)
- 离线:离线消息、重试、撤回/编辑 ack→UI
- integration:23 flow;maestro:14 e2e yaml(无 09)

## 推荐框架
- 单元/widget:`flutter test` + mocktail;进程隔离 runner(无头 widget 与异步页不兼容,评审)
- integration:`integration_test` 真机(禁模拟器做功能验证)
- E2E:maestro(现有 14 流)
- 自定义 lint:custom_lint 拦 autoDispose/裸URL/800行/token(ENG-01)

## 目录结构（沿用现有丰富结构）
```
imboyapp/test/{unit,widget,page,service,store,modules,component,utils,smoke}/
imboyapp/test/integration/  imboyapp/integration_test/
imboyapp/maestro/*.yaml  imboyapp/test/helpers/
```

## Mock 策略
mock 网络(Dio)、推送、presign 远端;**真 sqflite**(sqlcipher setDbForTest 注入 ffi)测本地库;WS 解码真实。currentUid/contact 必须播种(评审:空 null 致级联登出)。

## Fixture 策略
`test/helpers/` 共享构造器;ProviderScope override 注入测试依赖;真 sqflite 内存库。maestro 用 `_already_logged_in.yaml` 等前置状态。

## 数据准备
`scripts/setup_test_data.sh` + `test.env`;真机需 TEST_PHONE;integration 需真后端。

## CI 执行方式
analyze + 模块边界 + new code guard + 单元/widget(排除 integration/smoke)Stage 1;integration 独立 job(修复坏死 yml,TEST-05);maestro nightly/真机;custom_lint 门(ENG-01)。

## 覆盖率要求
service/store 70%;整体 55%;autoDispose 陷阱由 lint 门 100% 拦。

## 验收标准
- [ ] service/store 覆盖达标,进程隔离 runner 就位
- [ ] autoDispose/裸URL/800行/token custom_lint 门生效
- [ ] DDL 三镜像一致 + 降级失败显式有测试
- [ ] message_retry 全量扫描、撤回/编辑 ack→UI 有测试
- [ ] integration_test.yml 修复并在 CI 跑
- [ ] maestro 关键流(含 e2ee)进 nightly
