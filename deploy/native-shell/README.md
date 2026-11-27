# 原生 systemd 交付 / Native systemd delivery

这里是 Linux 单机原生安装包的源码。它用 systemd 启动 IMBoy、PostgreSQL 和 Garage，适合明确需要在宿主机运行的交付。普通 Docker 安装从 [部署首页](../README.md) 开始。

This directory contains the source for the native Linux delivery kit. It uses systemd to run IMBoy, PostgreSQL, and Garage on the host. For Docker installation, start with the [deployment guide](../README.md).

- [安装入口与支持范围 / Install and support scope](../../docs/guides/operations/deployment/native-shell/README.md)
- [发布方制包说明 / Publisher assembly](../../docs/guides/operations/deployment/native-shell/tools/README_PUBLISHER.md)

制包时会把完整手册放入客户包，并生成包内校验清单。 / Assembly adds the full offline manuals and generates package checksums.
