# 本地跑通 IMBoy 后端

本地启动步骤已统一到[本地开发指南](../guides/operations/deployment/local-development.md)，
使用仓库根目录的 `docker-compose.yml` 与 `scripts/dev_setup.sh`。

- 想先体验整套服务：按指南的默认启动步骤，启动 API、Admin、PostgreSQL、Garage 与 Nginx。
- 需要修改 Erlang 源码：使用 `--infra-only`，按指南的「宿主机源码调试」配置、构建 release 并启动节点。
- 需要音视频或客服 Widget：按需加 `--with-livekit` / `--with-widget`。

容器启动、HTTP 接口响应与真实业务验收分别检查；启动成功不代表双端音视频、附件或
登录流程已验收。版本与生产部署要求见[生产部署手册](../../deploy/README.md)。
