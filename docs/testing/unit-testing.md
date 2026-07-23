# 单元测试（Unit Testing）

## 为什么需要
金字塔底座,毫秒级反馈,定位精确。逻辑分支、编解码、状态机、校验函数必须在最低层被测,不靠上层兜底。评审教训:巨型文件(chat_page.dart 2234 行)与命令式 Notifier 是 bug 高发区,正因缺细粒度单元覆盖。

## 覆盖范围
- 后端:`src/logic/`(业务规则)、`src/lib/`(elib_* 工具、编解码 imboy_codec/imboy_frame、TSID、密码 KDF)、纯计算的 ds
- Flutter:`service/`、`store/`、`utils/`、状态 Notifier 的纯逻辑、model 序列化
- Admin:`services/`(safeParseBigIntJson、api 转换)、`hooks/`、`lib/`(纯函数)

## 推荐框架
- 后端:EUnit + meck(现用)
- Flutter:`flutter test` + mocktail/mockito
- Admin:bun test（当前仅 bun test，无 vitest）

## 目录结构（沿用现有）
```
imboy/test/{logic,lib,ds}/*_tests.erl
imboyapp/test/{unit,service,store,utils}/*_test.dart
imboyadmin/src/**/*.test.ts(就近)+ src/test/
```

## Mock 策略
只 mock 系统边界外(时钟、随机、第三方 API、DB 调用)。**禁止 mock 自己的编解码/协议逻辑**(否则 imboy_codec 的 to_pb_map 字段蒸发类 bug 测不出)。后端用 `test/common/meck_helper.erl`,注意 meck history 三元组 `{Pid,{M,F,Args},Result}`。

## Fixture 策略
内存构造器 + 工厂函数。测试数据在测试内联构造,AAA 结构(Arrange-Act-Assert)。共享构造器放 `test/common/test_helper.erl` / Flutter `test/helpers/`。

## 数据准备
无外部依赖,纯内存。时间/随机注入(禁用真实 Date.now/random 以保证确定性,与评审的 TSID 确定性一致)。

## CI 执行方式
Stage 1 快门,每 PR 阻塞。后端 `make eunit EUNIT_MODS=...`;Flutter 排除 integration/smoke 后跑;Admin `bun run test`。

## 覆盖率要求
后端 Logic 70% / Lib 高(编解码/密码/TSID 应近 90%);Flutter service/store 70%;Admin services/hooks 70%。见 coverage-plan。

## 验收标准
- [ ] 编解码/状态机/校验/密码学有分支级覆盖
- [ ] 零 mock 协议边界
- [ ] 确定性(无真实时钟/随机)
- [ ] 每个纯逻辑 bug 有单元回归
