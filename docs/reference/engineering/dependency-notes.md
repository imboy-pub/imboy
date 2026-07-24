# 依赖管理笔记（Dependency Notes）

> 工程视角 · 描述现状 + 增量改进 · 补充 `docs/planning/tech-debt.md`(AGPL 法务已记 P0-4,此处不重复法务面)

## 现状

**后端**(`imboy/Makefile` DEPS)显式声明约 27 个依赖,分组清晰:
- Web:ranch/cowlib/cowboy/gun
- 基础:erlware_commons/jwerl/gen_smtp/throttle/jsone/jsx/goldrush
- 数据/中间件:epgsql/pooler/erlang_migrate/depcache/syn/ecron/uid
- 运维/调试:telemetry/lager/observer_cli/recon/redbug/**sync**
- 其他:simple_captcha/erlydtl、内部 `erlang_pay`
- transitive dep 手工 pin 有注释说明(goldrush 为 lager 传递依赖、jsx 走 gitee 避免 hex_core sub-make)。
- `LOCAL_DEPS`/`BUILD_DEPS`/`TEST_DEPS`/`DOC_DEPS` 分层明确;有 `rebar.lock`。

**Flutter**(`imboyapp/pubspec.yaml`)**100+ 个直接依赖**(实测 dependencies 块远超 60，顶层依赖 100+)+ `pubspec.lock` 锁定，依赖膨胀明显。含较多 fork/同类库:
- fork:`flyer_chat_*` 系列(8 个)、`plugin/r_upgrade`
- 同类多库:音频 `just_audio`/`flutter_sound`/`audio_waveforms`/`audio_session`;图片 `image_picker`/`wechat_assets_picker`/`wechat_camera_picker`/`photo_manager`/`photo_view`
- AGPL:`flutter_vodozemac`(法务见 review)

**Admin**(`imboyadmin/package.json`)约 30 运行时依赖,React 19.2 生态(radix/tanstack/zustand/zod/react-hook-form)。

**更新机制**:三仓有 `dependabot-auto-merge.yml`;imboy 有 `sbom-diff.yml`(SBOM 变更追踪)。

## 优点

- 后端依赖显式声明 + 分组 + transitive pin 注释,可追溯性好。
- 三仓均有锁文件与 dependabot,更新不失控。
- SBOM diff 让供应链变更可见。
- 依赖分层(LOCAL/BUILD/TEST/DOC)规范。

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
