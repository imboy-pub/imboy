# TDD 证据报告：APP 支付宝登录（后端 + Flutter 客户端）

> 日期：2026-08-19 | 后端分支：main | imboy 提交：44b95a51(RED) → b2d8c97b(GREEN) → 85f98e06(authinfo GREEN)
> Flutter 提交：imboyapp f7367b9d

## 1. 来源

无 `*.plan.md` 输入；用户旅程在本轮 TDD 直接编写（下方第 2 节）。
需求来自 `/ecc:tdd-workflow` 指令：对接 APP 支付宝登录（`alipay.system.oauth.token` +
`alipay.user.info.share`），密钥材料为证书模式三证书（app_id `2021004142626807`）。

## 2. 用户旅程

1. 作为新用户，我希望用支付宝授权码登录，系统自动建号并返回登录态，无需注册。
2. 作为老用户，我希望用支付宝授权码登录，直接返回已有账号登录态。
3. 作为平台，过期/伪造授权码、令牌失效、账号禁用、License 配额满都必须明确拒绝。
4. 作为运维，普通公钥/证书两种加签方式都要可用，证书 SN 可经环境变量注入。

## 3. 任务报告

| 阶段 | 验证命令 | 结果摘录 |
|------|---------|---------|
| RED | `make eunit-local t=alipay_openapi_tests` | `Failed: 11. Passed: 0.` error:undef（模块不存在） |
| RED | `make eunit-local t=passport_alipay_login_tests` | `Failed: 10. Passed: 0.` error:undef |
| GREEN | `make eunit-local t=alipay_openapi_tests` | `All 11 tests passed.` |
| GREEN | `make eunit-local t=passport_alipay_login_tests` | `All 10 tests passed.` |
| 回归 | `make eunit-local t={auth_oidc_logic,passport_handler,imboy_router_registry,imboy_env_policy,token_ds,sso_config_ds}_tests` | 21/6/17/2/11/9 全绿 |
| 对照 | HEAD 版 passport_logic 跑 passport_logic_tests | 同样 7 红（meck 穿透 DB noproc，既有问题，与本次无关） |
| 覆盖率 | `cover:analyse/1`（21 用例全绿下） | alipay_openapi 导出函数 100% 覆盖；内部未覆盖行仅为 dir_string/enc_char 兜底与 httpc 传输错误分支（≈84% 行覆盖） |

Refactor 阶段无改动：实现复用既有 `quota_guard`/`pick_data_for_insert`/`login_resp`/
`sso_identity_ds`，无重复代码可收；跳过 refactor 提交。

## 4. 测试规格（保证清单）

| # | 保证内容 | 测试位置 | 类型 | 结果 |
|---|---------|---------|------|------|
| 1 | 应用公钥证书 SN = md5("CN=..,OU=..,O=..,C=.."+serial)，对照 Python 参考实现预算值 | `alipay_openapi_tests:cert_sn_app_test` | unit | PASS |
| 2 | 根证书链 SN：RSA 家族证书各算 SN 用 `_` 拼接 | `root_cert_sn_chain_test` | unit | PASS |
| 3 | 根证书链混入坏 PEM 块跳过不崩 | `root_cert_sn_skip_bad_block_test` | unit | PASS |
| 4 | oauth_token 公共参数齐全 + biz 含授权码 + RSA2 签名可用公钥验过 | `oauth_token_ok_test_` | unit | PASS |
| 5 | 证书模式注入 app_cert_sn/alipay_root_cert_sn | `oauth_token_cert_mode_params_test_` | unit | PASS |
| 6 | 业务错误（40002）透出 sub_msg | `oauth_token_biz_error_test_` | unit | PASS |
| 7 | HTTP 500 / 坏 JSON / 未配置凭据 三失败分支 | `oauth_token_http_500/bad_json/no_credential_test` | unit | PASS |
| 8 | user_info_share 透传六字段、auth_token 走 biz_content、签名可验 | `user_info_share_ok_test_` | unit | PASS |
| 9 | 老用户 (alipay,user_id) 映射命中直登，不触发建号绑定 | `existing_user_login_test_` | logic | PASS |
| 10 | 新用户自动建号：昵称/头像回填、source=alipay、随机密码占位、sso_identity 绑定 | `new_user_provision_test_` | logic | PASS |
| 11 | 昵称兜底 alipay_+user_id 尾 6 位；性别缺省 0 | `nickname_fallback_test_` | logic | PASS |
| 12 | 性别 m→1 / f→2 映射 | `gender_map_female_test_` | logic | PASS |
| 13 | 授权码无效 / userinfo 失败 错误透出 | `invalid_auth_code/userinfo_failed_test_` | logic | PASS |
| 14 | 账号禁用（status=0）拒绝登录 | `disabled_user_rejected_test_` | logic | PASS |
| 15 | License 配额满返回 402 | `quota_exceeded_test_` | logic | PASS |
| 16 | 建号 23505 并发冲突回读映射直登 | `provision_conflict_rebind_test_` | logic | PASS |
| 17 | 未配置凭据拒绝并提示 | `no_credential_test` | logic | PASS |

## 5. 覆盖率与已知缺口

- alipay_openapi：导出函数 100% 覆盖；未覆盖为内部兜底分支（dir_string 非常见
  字符串类型、enc_char 非 ASCII 路径、httpc 传输层异常分支）。无 E2E（真实支付宝
  网关）覆盖——依赖应用私钥到位后真机联调补充。
- passport_logic 新增 6 个函数（alipay_login/alipay_cfg/alipay_map_or_provision/
  alipay_provision/alipay_finish/alipay_gender/alipay_nickname）全部被 10 个用例
  触达；模块整体覆盖率被既有未测代码稀释，非本次范围。
- handler 层（passport_handler:alipay_login/1）未写独立 handler 测试：逻辑全在
  logic 层，handler 仅参数提取与响应包装（与 quick_login 现有测试策略一致）。

## 6. 合并证据

- RED 提交：`44b95a51 test: 支付宝登录 RED 复现用例（alipay_openapi + passport_logic:alipay_login/2）`
- GREEN 提交：`b2d8c97b feat: APP 支付宝登录后端对接（auth_code→oauth.token→user.info.share→建号/直登）`
- authinfo GREEN 提交：`85f98e06 feat: 支付宝 authinfo 签名端点（客户端唤起 SDK 用，私钥不出服务端）`（3 用例先红后绿，alipay_openapi_tests 14/14）
- Flutter 提交：`f7367b9d feat(passport): 支付宝登录（tobias SDK 授权 + authinfo/auth_code 双端点对接）`（imboyapp 仓）
- 无 squash 计划；如需 squash，本报告即 RED/GREEN 证据副本。

## 7. 第二轮：authinfo 签名端点（2026-08-19）

客户端唤起支付宝 SDK 需要服务端签名的 authInfo（私钥不出客户端）：
`GET/POST /api/v1/passport/alipay_authinfo` → `passport_logic:alipay_authinfo/0`
（target_id 每次新生成 16 hex）→ `alipay_openapi:authinfo/2`
（alipay.open.auth.sdk.code.get 参数组装 + RSA2，sign URL 编码置尾）。

| 阶段 | 结果 |
|------|------|
| RED | 3 新用例全红（authinfo/2 undef），11 旧用例绿 |
| GREEN | alipay_openapi_tests 14/14；回归 passport_alipay_login 10、passport_handler 6、router_registry 17 全绿 |

保证清单（18-20）：authinfo 11 个固定参数齐全 + 签名可用公钥验过（`authinfo_ok_test`）；
缺 pid / 缺凭据均拒绝（`authinfo_no_pid_test` / `authinfo_no_credential_test`）。

## 8. Flutter 客户端（imboyapp 仓）

链路：`loginByAlipay` → `GET /passport/alipay_authinfo` → tobias SDK 授权 →
`POST /passport/alipay_login {auth_code}` → `_onThirdLoginSuccess`
（与 quickLogin 共用的落库/socket/跳转，DRY 抽取 ~50 行）。

| 保证内容 | 测试位置 | 结果 |
|---------|---------|------|
| SDK 回调解析 7 分支（9000 Android/iOS 两形态/缺 code/6001/6002/4000/8000） | `test/unit_test/service/alipay_auth_service_test.dart` | 7/7 PASS |
| 登录页第三方登录区渲染（按钮 Key + i18n 文案） | `test/unit_test/page/passport/login_page_test.dart` | 9/9 PASS |
| passport 目录回归 | `flutter test test/unit_test/page/passport/` | 230/230 PASS |
| lint | `dart analyze`（改动文件） | 0 issues |

诚实说明：Flutter 侧 service/widget 测试为紧邻实现后补（非严格 RED）；
编排层 `loginByAlipay` 无 notifier 单测（与 quickLogin 现状一致，真机联调兜底 E2E）。
iOS `ios/Runner/Info.plist` 的 CFBundleURLTypes 已加 name=alipay 回跳 scheme
（owner 豁免保留区，提交 1c8462bd）。

## 9. 第三轮：AES 内容加密（2026-08-19，owner 决定保留控制台 AES 设置）

控制台「接口内容加密方式」开启后：请求 biz_content = base64(AES-128-CBC(JSON))，
响应节点值变密文 string。实现：encode_biz 按 aes_key 是否配置自适应；
parse 双兼容（string 解密 / map 明文）。IV=16 字节 0，PKCS7 padding。

| 阶段 | 结果 |
|------|------|
| RED | 3 新用例 2 红 1 绿（明文回退用例现状即绿），14 旧用例绿 |
| GREEN | alipay_openapi_tests 17/17；回归 passport_alipay_login 10 全绿 |

缺陷记录：pkcs7_pad 初版 `N:(N*8)` 位编码错误（值 N 编码为 N 字节而非 N 重复 N 次），
经 python cryptography 向量交叉验证定位（解密见 `\x00\x02` 应为 `\x02\x02`），
改 `binary:copy(<<N>>, N)`。教训：padding 类原语必须用独立参考实现双向对拍，
单测「能解密自己加密的内容」不足以抓错（自洽错误）。

保证清单（21-23）：请求 biz_content 与预算向量逐字节一致（`user_info_share_aes_request_test_`）；
响应密文解密还原字段（`user_info_share_aes_response_test_`）；
配 AES 但网关返回明文时按明文处理不报错（`aes_response_plaintext_fallback_test_`）。

提交：`4764871e feat: 支付宝接口内容 AES-128-CBC 加解密支持`
