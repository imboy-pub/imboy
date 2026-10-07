# 一期交付包说明（AC27）

> 状态：current | 生成器：`scripts/deliver/make_delivery_pack.sh` | 合同：主计划 AC27

## 一句话

交付包 = **git tracked 白名单** 减去 AC27 禁项（真实配置/凭据/日志/crash/缓存/临时 evidence），
输出 manifest + tar；生成物只落仓外目录（默认 `/tmp`），仓库内不产生任何打包残留。

## 生成命令

```bash
# manifest + 自检 + tar（输出到 /tmp/imboy-delivery-<sha8>-<时间戳>/）
scripts/deliver/make_delivery_pack.sh

# 只要 manifest + 自检（不产 tar）
scripts/deliver/make_delivery_pack.sh --no-tar

# 指定输出目录（必须在仓库外，脚本会拒绝仓内路径）
scripts/deliver/make_delivery_pack.sh --out-dir /path/outside/repo

# 附加环境特定凭据指纹扫描（指纹文件一行一个字面量，仓外维护绝不提交）
scripts/deliver/make_delivery_pack.sh --secret-fingerprints ~/.imboy/secret-fp.txt
```

## 排除规则（禁项 ↔ 规则对照）

| 类别 | 规则 | 说明 |
|---|---|---|
| 真实配置 | `config/sys.{local,dev,prod,eunit}*`（任意层级） | 环境配置永不入包 |
| 真实 env | `.env`、`.env.<variant>`；`*.example` 豁免 | example 是配置入口模板 |
| 日志/crash | `*.log`、`erl_crash.dump`/`crash.dump` | 归档可追溯性由 git 历史与结论文档承担 |
| 临时 evidence | 顶层 `evidence/` | 内部审计工作产物；`docs/archive`、`docs/qa` 的文档化归档保留 |
| 缓存 | `node_modules/` | — |
| 凭据命名 | basename 含 `private[_-]?key` 一律拦；`password` 词元且扩展非源码/契约/文档 | `elib_password.erl` 等业务语义文件豁免 |
| 内容级（硬失败） | 真实私钥 PEM 块（私钥头 + 200+ 连续 base64）；可选指纹扫描 | 文档注释与占位串不会命中 |

排除与自检共用同一规则函数（`credential_named_paths`），两者逻辑不漂移；
自检对最终 manifest 断言，任一命中即 exit 1（双保险）。

## 校验方法

1. `manifest.meta.md`：记录 git SHA、tracked/excluded/manifest 计数、规则摘要——核对计数闭合（tracked = excluded + manifest）。
2. `tar.sha256`：tar 的 SHA256；接收方 `shasum -a 256 -c tar.sha256` 验完整性。
3. tar 条数 == manifest 条数（脚本内建断言）。
4. `.pem` 文件触发 WARN 清单：需人工确认全部为公钥/开发 fixture（当前唯一命中 `priv/vendor_pubkey_dev.pem` 已确认为 `BEGIN PUBLIC KEY`）。
5. 有环境凭据指纹清单时，用 `--secret-fingerprints` 复跑一轮，命中即 FAIL。

## 已知边界

- 白名单源是 `git ls-files`（`core.quotepath=off` 保证非 ASCII 文件名原样输出）；
  工作区未跟踪产物（`_build/`、`deps/`、本地配置）天然不进包。
- 若未来 tracked 集混入禁项，第 2 步排除 + 第 3 步自检会拦截，但**新增禁项类别**需同步更新
  排除段与自检段两处（规则函数注释有对应关系表）。
- 本脚本只打包 imboy 主仓；App/Admin 仓交付物由各仓自己的发布通道产出（见各自 README），
  三仓指纹对应关系由 Q12 终验冻结（AC30）。
