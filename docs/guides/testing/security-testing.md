# 安全测试（Security Testing）

## 为什么需要
评审确认唯一高危是计费对象级越权(BOLA),另有会话/密钥卫生中危。安全测试把这些变成自动回归,防止修复后复发,并系统排查同类。IM 涉及 PII、E2EE、支付,安全回归是发布门禁一部分。

## 覆盖范围
- 授权(BOLA/IDOR):每认证端点越权拒绝(billing 9 端点优先,租户隔离)、附件 key owner_of_key、Garage presign scope 越权
- 认证:JWT 校验/过期/吊销、设备签名门、鉴权豁免矩阵(P0-1 前缀死代码回归)、admin cookie 伪造/过期
- 注入:SQL 参数化(repo 全覆盖)、elib_pg_sql raw 逃生门、命令注入、路径遍历
- 密码学:服务端零明文(check_server_zero_crypto)、密码 KDF 强度、AES-GCM tamper、密钥不复用/不入库
- 输入校验:API 参数、WS 帧、文件上传(validateImageData)
- 支付:mandate.owner_uid 红线、金额结算原子、金钱 DoS 限流、binary_to_atom 外部输入(原子表 DoS)
- 传输:CSP/HSTS/cookie 属性、WS token 校验

## 推荐框架
- 静态:gitleaks(密钥扫描,--no-git 模式须 inline allow 白名单)、依赖许可扫描(防 AGPL)、SAST(Sonar)
- 动态:越权用例(CT,A token 访问 B 对象)、注入 fuzz(schemathesis 打 OpenAPI)
- 密码学:check_server_zero_crypto.sh(硬门)、tamper 向量
- 客户端:证书校验、sqlcipher、Keychain 存储审计

## 目录结构
```
imboy/test/security/(越权/注入 CT)
imboy/scripts/check_server_zero_crypto.sh + gitleaks 配置
imboy/test/(billing 越权回归)
```

## Mock 策略
授权/注入/密码学测试**零 mock**(mock 掉就测不出真越权)。真 handler + 真 PG + 真 JWT。

## Fixture 策略
多租户/多角色账号 fixture(跨归属数据);恶意输入向量库(注入 payload、超大帧、篡改密文);越权矩阵(角色 × 端点)。

## 数据准备
真 PG 播种多租户;JWT 造合法/过期/他人;越权数据造跨归属对象。PII 绝不用真实数据,一律合成。

## CI 执行方式
gitleaks + 许可扫描 + 零加密检查 Stage 1 硬门;越权/注入回归 Stage 2;OpenAPI fuzz nightly;安全回归随每 PR。

## 覆盖率要求
**每认证端点授权测试 100%**;SQL 参数化 100%;密码学零明文硬门永绿;已知漏洞(billing 越权、cookie 伪造、P0-1)全有回归。

## 验收标准
- [ ] billing 9 端点 + 全认证端点越权拒绝有回归
- [ ] 鉴权豁免矩阵(P0-1)有回归,前缀死代码不复发
- [ ] SQL 注入/raw 逃生门有防护断言
- [ ] 服务端零明文硬门永绿,密钥不入库(gitleaks)
- [ ] admin cookie 伪造/过期有测试
- [ ] 支付红线 + 金钱 DoS 限流有测试
- [ ] 依赖许可扫描拦 AGPL/GPL
