---
layout: home

hero:
  name: IMBoy
  text: 企业私有化即时通讯平台
  tagline: 开箱即用的 E2EE 加密 IM — 自托管、可审计、零信任
  actions:
    - theme: brand
      text: 快速上手
      link: /tutorials/quickstart-backend
    - theme: alt
      text: E2EE 协议规范
      link: /reference/e2ee-protocol-specification
    - theme: alt
      text: 部署指南
      link: /guides/operations/deployment/day1-quickstart

features:
  - title: 端到端加密
    details: Olm/Megolm 双棘轮 + X3DH 密钥协商 + TOFU 身份钉扎，服务端零知识。509 项自动化安全测试全绿。
  - title: 私有化部署
    details: Docker Compose 一键部署，Helm Chart 支持 K8s。数据完全自主可控，满足等保合规要求。
  - title: 全平台覆盖
    details: Erlang/OTP 高并发后端 + Flutter 移动端 + React 管理后台 + JS SDK，覆盖企业 IM 全场景。
  - title: 可审计安全
    details: 威胁模型 12 项防御点全覆盖，CI 零密码学门禁，哈希链审计日志，一键可复现安全验证。
---
