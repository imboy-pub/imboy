# 文档站 / Documentation site

文档正文来自仓库的 `docs/`；`bun run sync` 将其复制到 `content/`，首页 `content/index.md` 单独保留。修改正文时编辑源文件。

Documentation comes from the repository's `docs/` directory. `bun run sync` copies it into `content/` while preserving `content/index.md`. Edit the source documents when changing content.

## 构建与本地查看 / Build and local viewing

需要 Bun、与已安装 Vite/VitePress 的 `engines` 兼容的 Node.js，以及 `bash`、`rsync`。在本目录执行现有 CI 检查：

Install Bun, Node.js compatible with the installed Vite/VitePress `engines`, `bash`, and `rsync`. Run the existing CI checks from this directory:

```sh
bun install --frozen-lockfile
bun test ./scripts/*.test.ts
bun run build
```

开发服务使用 `/`；构建和预览默认使用 `.vitepress/config.ts` 中的 `/imboy/`。预览必须沿用构建时的 `DOCS_BASE`，否则 HTML 中的资源路径与服务器路径不一致。预览使用 Vite 静态服务器，通过 `.vitepress/preview.config.ts` 复用构建的 `base` 和 `outDir` 并默认绑定本机；VitePress 1.6.4 的预览入口不使用 `--host` 参数。以下命令只监听本机：

Development uses `/`; build and preview default to `/imboy/` from `.vitepress/config.ts`. Preview must use the same `DOCS_BASE` as the build so that asset URLs match the server path. Preview uses Vite's static server with `.vitepress/preview.config.ts`, which reuses the build's `base` and `outDir` and defaults to loopback; VitePress 1.6.4's preview entry point does not use `--host`. These commands listen on loopback only:

```sh
bun run dev --host 127.0.0.1 --port 5173
# http://127.0.0.1:5173/

bun run preview --host 127.0.0.1 --port 4173
# http://127.0.0.1:4173/imboy/
```

## 安全依赖固定 / Security dependency pins

`package.json` 的 `overrides` 固定文档工具链的修复版本，`bun.lock` 记录实际解析结果：

The `overrides` in `package.json` pin patched versions in the documentation toolchain. `bun.lock` records the resolved dependency graph:

| 依赖 / Dependency | 固定版本 / Pin | 依据 / Reference |
| --- | --- | --- |
| `vue`, `@vue/shared` | `3.5.42` | [Vue 3.5.42 changelog](https://github.com/vuejs/core/blob/v3.5.42/CHANGELOG.md): server-renderer attribute-name fix; keep the Vue packages aligned. |
| `nanoid` | `3.3.18` | [Upstream release](https://github.com/ai/nanoid/releases/tag/3.3.18), CVE-2026-67213. |
| `source-map-js` | `1.2.2` | [Upstream release](https://github.com/7rulnik/source-map-js/releases/tag/v1.2.2), CVE-2026-93749. |
| `vite` | `6.4.5` | [Upstream advisory](https://github.com/vitejs/launch-editor/security/advisories/GHSA-587h-7wmj-h5pm), CVE-2026-112942; [follow-up fix](https://github.com/vitejs/vite/security/advisories/GHSA-8g5g-mxqh-grgj) for the 6.4.4 `server.fs.deny` regression. |

VitePress `1.6.4` 声明依赖 `vite: ^5.4.14`。这里显式使用范围之外的 Vite `6.4.5`，以采用该安全修复；这不是 VitePress 上游对该组合的兼容承诺。`@vitejs/plugin-vue` `5.2.4` 的 peer 范围包含 Vite 6，但仍须验证本站。单独固定 `@vue/shared` 可避免顶层保留旧版、Vue 内部再安装多份新版。

VitePress `1.6.4` declares `vite: ^5.4.14`. This explicit override uses Vite `6.4.5` outside that range to obtain the security fix; it is not an upstream compatibility guarantee from VitePress. The peer range of `@vitejs/plugin-vue` `5.2.4` includes Vite 6, but this site still requires validation. Pinning `@vue/shared` separately prevents an older top-level copy alongside newer nested copies.

修改或移除这些固定值时，重新生成并审查锁文件，保持 Vue 组件版本一致；执行上述安装、测试和完整构建，检查开发/预览页面及其资源加载，再运行仓库安全门禁。上游发布包含修复且经本站验证的兼容依赖范围后，再移除不再需要的 override。Linux 构建成功不代表 Windows 运行已验证，也不证明这些依赖漏洞曾在本站生产环境可利用。

When changing or removing these pins, regenerate and review the lockfile, keep the Vue packages aligned, run the install, tests, and full build above, check development and preview pages and their assets, and rerun the repository security gates. Remove unnecessary overrides after upstream provides a patched dependency range that has been validated with this site. A successful Linux build does not establish Windows compatibility or prove that these dependency vulnerabilities were exploitable in this site's production deployment.
