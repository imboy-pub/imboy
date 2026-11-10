# 依赖管理笔记（Dependency Notes）

> 工程视角 · 描述现状 + 增量改进 · AGPL 法务面此处不重复（另见法务合规文档）

## 现状

**后端**以 `Makefile` 的依赖图和 `include/deps.mk` 的来源声明为准，分组清晰：
- Web:ranch/cowlib/cowboy/gun
- 基础:erlware_commons/jose/gen_smtp/throttle/jsone/goldrush(jose 为 JWT 签发/验签库,业务入口 src/lib/imboy_jwt.erl;jsone 为唯一 JSON 库,2026-09 移除 jsx 直用,兼容契约见 test/lib/json_codec_equivalence_tests.erl)
- 数据/中间件:epgsql/pooler/erlang_migrate/depcache/syn/ecron/uid
- 运维/调试:telemetry/lager/observer_cli/recon/redbug/**sync**
- 其他:simple_captcha/erlydtl、内部 `erlang_pay`
- transitive dep 手工 pin 有注释说明(goldrush 为 lager 传递依赖;jsone 走上游 git tag)。
- `LOCAL_DEPS`/`BUILD_DEPS`/`TEST_DEPS`/`DOC_DEPS` 分层明确；根 `rebar.lock` 为 `[].`，不锁定 erlang.mk 的依赖图。

**Flutter**(`imboyapp/pubspec.yaml`)**100+ 个直接依赖**(实测 dependencies 块远超 60，顶层依赖 100+)+ `pubspec.lock` 锁定，依赖膨胀明显。含较多 fork/同类库:
- fork:`flyer_chat_*` 系列(8 个)、`plugin/r_upgrade`
- 同类多库:音频 `just_audio`/`flutter_sound`/`audio_waveforms`/`audio_session`;图片 `image_picker`/`wechat_assets_picker`/`wechat_camera_picker`/`photo_manager`/`photo_view`
- AGPL:`flutter_vodozemac`(法务见 review)

**Admin**(`imboyadmin/package.json`)约 30 运行时依赖,React 19.2 生态(radix/tanstack/zustand/zod/react-hook-form)。

**更新机制**:三仓有 `dependabot-auto-merge.yml`;imboy 有 `sbom-diff.yml`(SBOM 变更追踪)。

## 优点

- 后端依赖显式声明 + 分组 + transitive pin 注释,可追溯性好。
- Flutter、Admin 与后端文档站有各自锁文件；后端 Git 引用仍需区分声明与实际解析的提交。
- SBOM diff 追踪经过覆盖校验的源码依赖声明变化，覆盖范围见下节。
- 依赖分层(LOCAL/BUILD/TEST/DOC)规范。

## 源码 SBOM 覆盖与门禁 / Source SBOM coverage and gates

`sbom-diff.yml` 生成 **pre-build 源码清单**，不是部署镜像的完整清单。Trivy 固定为 0.72.0，必须启用 `--include-dev-deps`：文档站仅通过开发依赖引入 VitePress，默认排除开发依赖会得到零包；升级到 0.76.2 也不会改变这个默认行为。

The workflow produces a **pre-build source inventory**. It is not a complete inventory of the deployed image. Development dependencies are required for the documentation site's Bun lockfile.

| 范围 / Scope | 验证与限制 / Validation and limits |
|---|---|
| `docs-site/package.json` + `docs-site/bun.lock` | 清单中的 npm 身份必须逐项匹配全部锁项，包含开发依赖；锁文件的 application 节点不算依赖。All locked npm identities, including development dependencies, must match. |
| `Makefile` + `include/deps.mk` | 补充显式 `DEPS`、`BUILD_DEPS`、`TEST_DEPS` 的来源声明并集和 Hex 构建前置 `hex_core`。包括 local 条件下的 `sync`，不表示这些依赖同时进入生产。Declared runtime/build/test sources, including the local profile, are inventoried. |
| Erlang Git / Hex | Git 保留真实 URL/ref，不冒充 Hex 包；Hex 使用实际包名（如 `chatterbox` 对应 `ts_chatterbox`）。浮动分支不填造解析后的版本。Git refs are declarations; floating refs are unresolved. |
| Erlang 其他范围 | 传递依赖、OTP 自带应用、额外构建工具及未解析的文档依赖（如 `edown`）仍不完整。An empty `rebar.lock` does not establish an empty erlang.mk dependency graph. |
| Docker / 多语言制品 | `trivy fs` 不执行 Dockerfile、不解析已安装的系统包，也不取得其他仓库的构建产物。Container, copied native libraries, and externally built assets remain outside this source inventory. |

CycloneDX 中使用 `lifecycles: pre-build`、`compositions: incomplete` 和覆盖契约记录边界、输入摘要及各域依赖身份摘要。任一应有包漏扫、关键声明无法解析、清单与当前来源不一致，均在比较和上传前失败；不能以总组件数大于零替代逐项校验。只有实际声明和锁项均为空，才可把该域标记为无依赖。

Every required dependency must be accounted for before comparison or artifact upload. Missing inputs and unsupported declarations fail closed. A domain is empty only when its manifests prove it is empty; unscanned scopes remain incomplete.

历史基线也必须通过覆盖契约自检。合法但没有契约的旧清单（包括历史零组件产物）默认失败；维护者确认后，可手动运行 `workflow_dispatch`，将 `rebaseline_legacy` 设为 `true`。该操作只建立新基线，明确报告旧清单不可比较，不输出正常增删统计。JSON 损坏、契约无效或当前漏扫不能借此开关放行。首次确实不存在历史 artifact 时可建立基线，下载错误仍然失败。

A legacy baseline requires an explicit manual rebaseline. It never produces normal dependency-change counts. The override cannot bypass current coverage checks, malformed artifacts, or invalid coverage contracts. A validated baseline continues to use normal comparison even when the override is set.

本地可复现验证（使用工作流固定的 Trivy 0.72.0）：

```bash
python3 -m unittest discover -s scripts/test -p test_sbom_workflow.py -v
SBOM_TRIVY_BINARY=trivy python3 -m unittest discover -s scripts/test -p test_sbom_workflow.py -v
trivy fs . --format cyclonedx --include-dev-deps --output /tmp/sbom-trivy.json
python3 scripts/sbom.py prepare --root . --input /tmp/sbom-trivy.json --output /tmp/sbom-current.json
```

Backend CI 继续强制运行离线回归；SBOM 工作流在安装 Trivy 后同时强制运行真实扫描回归。完整镜像清单还需针对实际构建结果补充：当前 Dockerfile 用 Gitee 固定归档预置 `erlang_pay`，而 `include/deps.mk` 声明 GitHub tag；还从 builder 复制 ERTS 和共享库，不能从源码声明推断其最终安装版本。

Backend CI enforces the offline regressions; the SBOM workflow also enforces real Trivy scans. A release inventory must inspect actual build outputs, including Docker's archived `erlang_pay` override and libraries copied from the builder.

## 潜在改进

1. **生产 profile 剥离调试依赖**(优先级高):`sync`(热加载)、`observer_cli`/`recon`/`redbug`(调试工具)在主 DEPS 中,若随生产 release 打包会扩大攻击面与镜像体积。建议 relx release 用 profile 区分,生产 release 排除或标注为仅 dev。需核对 `relx.config` 的 release 组装是否已排除(本笔记记录待核实点,不下结论)。
2. **Flutter 同类库合并评估**(中):4 个音频库、5 个图片相关库,评估是否可收敛,降低维护面与包体积。
3. **fork 依赖维护策略**(中):`flyer_chat_*` 8 个 fork + `r_upgrade`,需明确 upstream 跟进机制,避免长期脱节(评审已记 flyer_chat_ui.Avatar 裸加载类问题)。
4. **依赖许可扫描进 CI**(中):AGPL 事件暴露许可盲区,建议加许可扫描门(见 roadmap ENG-08),防再引入 GPL/AGPL。
5. **内部依赖 erlang_pay 版本协同**(低):作为独立仓依赖,明确版本对齐与联动发布。

## 相关模块

`imboy/Makefile`、`imboy/rebar.lock`、`imboy/relx.config`、`imboyapp/pubspec.yaml`、`imboyadmin/package.json`、`.github/workflows/sbom-diff.yml`、`dependabot-auto-merge.yml`

## 优先级

| 建议 | 优先级 |
|---|---|
| 生产 profile 剥离调试依赖 | 高 |
| 依赖许可扫描进 CI | 中 |
| Flutter 同类库合并评估 | 中 |
| fork 依赖 upstream 跟进机制 | 中 |
| erlang_pay 版本协同 | 低 |
