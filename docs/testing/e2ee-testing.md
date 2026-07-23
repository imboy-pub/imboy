# E2EE 测试（E2EE Testing）

## 为什么需要
E2EE 是产品核心卖点与最高风险面。评审确认基本面扎实(服务端零明文私钥、JWT/密钥卫生),但三代方案共存(RSA/Olm/Megolm)、Olm C2C 被硬门控、proto E2EEMeta 缺 olm 子对象、AGPL 依赖。密码学错误往往静默且灾难性,必须专项验证正确性而非仅功能。

## 覆盖范围
- 密码学正确性:Olm 单聊、Megolm 群、AES-256-GCM(tamper 检测)、RSA-OAEP(历史 decrypt-only)、随机数源
- 零信任不变量:**服务端永不接触明文私钥/明文 ciphertext**(已有 `check_server_zero_crypto.sh` 硬门)
- 密钥生命周期:OTK 消费/审计/清理、device trust、密钥备份(Matrix 4S)、换设备恢复
- 端到端:双端 vodozemac roundtrip、room-key-over-Olm 双包(RSA→Olm)、收端不降级(OlmAuthenticationException)
- 协议:proto E2EEMeta olm 子对象、E2EE 帧路由
- 会话:多设备密钥同步、群成员变更 rekey

## 推荐框架
- 后端:CT(零加密硬门 + OTK/trust API 契约)+ EUnit(签名验签 imboy_plugin_signature)
- Flutter:`flutter test` + 真 vodozemac(roundtrip)+ integration_test(10_e2ee_c2c/11_e2ee_group maestro 流)
- 密码学:向量测试(已知明文/密文对)

## 目录结构
```
imboy/test/(e2ee handler/OTK/trust CT)
imboy/scripts/check_server_zero_crypto.sh(硬门)
imboyapp/test/(olm/megolm roundtrip)
imboyapp/maestro/10_e2ee_c2c.yaml / 11_e2ee_group.yaml
```

## Mock 策略
**密码学核心零 mock**(真 Olm/Megolm/vodozemac,否则正确性无意义)。只 mock 密钥分发的网络层。收端降级路径必须真实测(OlmAuthenticationException 防 RSA 冒充 Olm PASS)。

## Fixture 策略
已知密钥对 + 明文/密文金标向量;多设备场景 fixture(A 设备加密,B 设备解密);tamper 向量(篡改密文应解密失败)。

## 数据准备
真机(Olm 验收需真机,MCP 不认 iOS 真机);双账号双设备;OTK 预置。真机凭证 `TEST_PHONE`(不编造)。

## CI 执行方式
零加密检查 Stage 1 硬门;OTK/trust 契约 CT Stage 2;vodozemac roundtrip 单元 Stage 1;e2ee maestro 流 nightly/真机。

## 覆盖率要求
密码学路径 100%(加密/解密/tamper/降级拒绝);零信任不变量硬门永绿。

## 验收标准
- [ ] 服务端零明文(check_server_zero_crypto 硬门永绿)
- [ ] Olm/Megolm/AES-GCM roundtrip + tamper 检测有测试
- [ ] 收端不降级(不把 RSA fallback 冒充 Olm PASS)
- [ ] OTK/trust/备份/换设备恢复有测试
- [ ] proto E2EEMeta olm 子对象对齐
- [ ] 真机 Olm 验收有记录(非模拟器)
