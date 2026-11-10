# CI 流程笔记（CI Notes）

> 工程视角 · 描述现状 + 增量改进 · 与 `docs/guides/testing/ci-testing.md` 互补(那份偏测试编排,本份偏工程门禁现状)

## 现状

**后端**(`imboy/.github/workflows/`):`backend-ci.yml`(Ubuntu Compile Gate + Debian compile/core EUnit + Moment/契约回归 + PostgreSQL 专项门 + Dialyzer 基线门 + E2EE + 零加密/重复模块检查 + DCO)、`contract-gate.yml`(API/枚举/EntityId 契约门)、`quality.yml`、`sbom-diff.yml`、`codemap.yml`、`docs.yml`、`release.yml`、`nightly.yml`、`native-eunit-acceptance.yml`、`native-shell-delivery.yml`、`mirror.yml`、`widget-pairing.yml`、`dependabot-auto-merge.yml`。Full EUnit 在 nightly 路径，未复制到 Debian 快速门。

**Flutter**(`imboyapp/.github/workflows/`):`ci.yml`(analyze + 模块边界 + new code guard + unit/widget + Moment regression gate + 独立 integration job)、`contract.yml`(错误码契约门)、`integration_test.yml`(评审记录路径坏死)、`quality.yml`、`mirror.yml`、`dependabot-auto-merge.yml`。

**Admin**:`ci.yml`(lint + bun test + Channel/Moment regression gate)、`quality.yml`、`admin-e2e.yml`、`widget-pairing.yml`、`mirror.yml`、`dependabot-auto-merge.yml`。Playwright E2E 由 `admin-e2e.yml` 在 CI 覆盖。

**本地钩子**(三仓 `lefthook.yml`):后端 pre-commit erlfmt --check + gitleaks protect,commit-msg Conventional Commits;前端类似。

## 优点

- **ratchet 框架已成型**:Dialyzer 已是递减基线硬门，新增告警失败；Ubuntu OTP 28/29 编译门阻塞，OTP 27.3 保留原有 experimental 观察语义。
- 关键回归有针对性硬门(Moment/Channel regression gate 三仓一致)。
- 安全门到位:零加密检查(E2EE 不变量)、gitleaks、重复模块检查、SBOM diff、DCO。
- 本地钩子 + CI 双层,格式/规范在提交即拦。
- matrix(OTP 版本)+ 缓存(deps/PLT),CI 效率有优化。

## Ubuntu / Debian 兼容边界 / Compatibility boundary

现有 Linux jobs 固定 `ubuntu-24.04`，包括快速门、nightly、质量与发布工作流；job 名称、依赖关系和已有门禁条件保持不变。这样避免 `ubuntu-latest` 在 2026-10-19 至 2026-11-19 的 [Ubuntu 26.04 迁移](https://github.blog/changelog/2026-09-17-ubuntu-26-generally-available-and-latest-migration/)期间隐式更换发行版。固定标签仍会接收 GitHub runner 镜像更新，不等于固定全部系统包。

Existing Linux jobs use `ubuntu-24.04`; their names, dependencies and gate conditions are preserved. This isolates the distribution change from the `ubuntu-latest` rollout, while retaining hosted-image security and tool updates.

| 环境 / Environment | 执行内容 / Scope | 阻塞 / Blocking |
| --- | --- | --- |
| Ubuntu 24.04，OTP 28/29 | 既有编译、核心 EUnit、Moment、PG、E2EE、xref、Dialyzer 及其他适用门禁 | 是，保留既有条件 |
| Ubuntu 24.04，OTP 27.3 | 原有 Compile Gate 向后兼容观察项 | 原有 experimental |
| Ubuntu host + Debian 13/trixie 容器，OTP 29.1.2 | `IMBOYENV=local make test-build` + `bash scripts/ci/core-eunit.sh` | 是，失败即 job/workflow 失败 |

Debian 使用官方 `erlang:29.1.2-slim` 加完整 SHA-256 digest；[上游 Dockerfile](https://github.com/erlang/docker-erlang-otp/blob/dcff99bf8e6d498f040e504f2c233aa040e3c673/29/slim/Dockerfile)从 `debian:trixie` 构建 OTP，[镜像元数据](https://github.com/docker-library/repo-info/blob/master/repos/erlang/remote/29.1.2-slim.md)记录发布 digest。运行时断言 `/etc/os-release` 为 Debian 13/trixie、OTP 文件版本为 29.1.2，并实际加载 SSL/crypto 和计算 SHA-256，以检查动态库可用性。没有借用 Ubuntu 的 OTP 安装包。

Debian runs a digest-pinned official Erlang image, verifies the distribution and OTP version, and loads SSL/crypto before building. The compiler, dependencies and EUnit process execute inside Debian user space.

容器在 checkout 前用 `apt-get --no-install-recommends` 安装 `ca-certificates curl git build-essential pkg-config libssl-dev zlib1g-dev python3 ripgrep unzip`，使用 Bash 和 `C.UTF-8`。容器默认 root，不调用宿主机的 `sudo`；已有需要非 root PostgreSQL、Docker、完整应用启动或 VM 生命周期的门禁仍使用原执行器。Debian 快速门复用 Ubuntu 已有的两个环境恢复生成器，以及 S3 签名、存储注册表和 MIME 绑定三组 EUnit；EUnit 非零返回、取消或缺少非空成功汇总都会失败。

The additional job bootstraps its own build tools, then reuses the same five non-PostgreSQL EUnit selections as the Ubuntu compile gate. It does not duplicate the PostgreSQL, E2EE, Dialyzer or nightly matrices. Failures propagate through Bash `pipefail`, and an empty test summary fails closed.

### 固定与升级策略 / Version and cache policy

- Ubuntu 保留当前 OTP `28`/`29`/`27.3` 选择范围，以继续发现维护版本变化；`setup-beam` 解析出的精确版本写入缓存 key。Debian 固定 OTP 29.1.2 和镜像 digest，升级时在同一 PR 同步 image、版本断言和 cache key。
- Ubuntu/ Debian 的构建缓存分别包含发行版、架构、OTP 版本和构建文件 hash；Debian 另含镜像 digest 前缀。`deps`、`.erlang.mk`、PLT 和原生二进制不得跨发行版恢复；不使用跨环境 `restore-keys`。
- `erlang.mk` 中固定的 rebar3 兼容工具继续控制 Make 构建；Debian image 自带的全局 rebar3 不替换该路径。APT 包保持 Debian 13 的安全更新流并记录实际版本，没有承诺逐字节可复现。若要冻结所有系统包，还需单独维护镜像和 APT snapshot 更新流程。

Ubuntu keeps its existing OTP ranges and keys caches by the resolved version. Debian pins its image and OTP release. Distribution-specific caches prevent reuse of Ubuntu-built native dependencies or PLTs in Debian. The image digest does not freeze later APT installs; package security updates remain intentional.

### 验证和下一次迁移 / Validation and the next migration

1. PR 先通过 YAML/actionlint、ShellCheck 与仓库既有静态门；在 GitHub Actions 核对 Debian 身份日志、OTP/crypto 版本、完整编译和正数 EUnit 成功汇总。原有适用检查也必须完成，不能仅凭新 job 绿色判定整体通过。
2. Ubuntu 26.04 升级在隔离分支把固定 runner 从 24.04 改为 26.04，并同步缓存发行版字段；运行既有完整门禁，比较 setup-beam、GCC/OpenSSL、PostgreSQL 构建和 ELP 等工具的实际日志。先完成验证，再单独提交基线升级；不增加常驻全量发行版乘法矩阵。
3. 容器共用 Ubuntu 宿主内核，未覆盖 Debian 内核、systemd、主机网络、文件权限/服务安装和重启升级。需要这些证据时，在一次性自托管 Debian 13 VM 上运行对应原生/安装验收；必须先实际注册 runner，再使用其自定义标签。GitHub 标准托管 runner 没有 `debian-latest` 标签。

Validate both the new Debian job and every existing applicable gate. Promote Ubuntu 26.04 through a separately reviewed branch with cache isolation and full regression evidence. Containers validate Debian user space, not a Debian kernel or service lifecycle; those require a real Debian VM and a registered self-hosted runner.

## 潜在改进

1. **继续收紧存量基线**(优先级高,增量):Dialyzer 已是 ratchet 硬门；持续减少既有告警，保留 nightly 全量结果，不通过跳过或放宽门禁取得绿色。
2. **覆盖率门**(高):三仓仅 `test -f lcov.info` 存在性检查,补真实阈值门(见 `docs/guides/testing/coverage-plan.md`)。
3. ~~**admin E2E 进 CI**(高)~~ 已完成:`admin-e2e.yml` 已在 CI 覆盖 Playwright E2E。
4. **排查坏死工作流**(中):`imboyapp/integration_test.yml` 的触发配置(paths/working-directory)在 umbrella 工作区下失效,可能从未运行;注意 `ci.yml` 另有在跑的 integration job,该 yml 可能是冗余坏死副本。
5. **契约门禁**(高):proto/OpenAPI/ws_url diff 进 CI,拦协议漂移(roadmap ARCH-03)。
6. **"有约定无 lint"收口**(中):autoDispose/裸URL/800行/token 等约定补 custom_lint(roadmap ENG-01),让 CI 强制而非靠自觉。

## 相关模块

`imboy/.github/workflows/backend-ci.yml`、`imboyapp/.github/workflows/{ci,integration_test}.yml`、`imboyadmin/.github/workflows/ci.yml`、三仓 `lefthook.yml`、`imboy/scripts/check_*.sh`

## 优先级

| 建议 | 优先级 |
|---|---|
| 收紧现有质量基线 | 高 |
| 覆盖率阈值门 | 高 |
| 契约 diff 门(proto/OpenAPI/ws_url) | 高 |
| custom_lint 收口"有约定无机制" | 中 |
| 修坏死 integration_test.yml | 中 |
