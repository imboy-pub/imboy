# 获取帮助 / Getting Help

## 我应该去哪里提问？/ Where should I ask questions?

在提问之前，请先查阅以下资源，大多数常见问题已有解答：
Before asking, please check these resources — most common questions are already answered:

- 📖 **部署文档 / Deployment docs**：[`deploy/README.md`](deploy/README.md)
- 📖 **贡献指南 / Contributing guide**：[`CONTRIBUTING.md`](CONTRIBUTING.md)
- 📖 **安全政策 / Security policy**：[`SECURITY.md`](SECURITY.md)
- 🔍 **搜索已有 Issue / Search existing issues**：[GitHub Issues](../../issues)

---

## 社区支持（免费）/ Community Support (Free)

### GitHub Discussions

一般性问题、使用咨询、功能想法，请前往：
For general questions, usage advice, and feature ideas:

👉 **[GitHub Discussions](../../discussions)**

分类建议 / Suggested categories:
- **Q&A** — 如何配置 / 部署 / 使用 IMBoy / How to configure / deploy / use IMBoy
- **Ideas** — 功能建议 / Feature suggestions
- **Show and tell** — 分享你的部署案例 / Share your deployment

### GitHub Issues

**仅用于 Bug 报告**。请先确认问题可稳定复现，再提交 Issue。
**For bug reports only.** Please confirm the issue is reproducible before filing.

使用 Issue 模板 / Use the issue template：[Bug Report](.github/ISSUE_TEMPLATE/bug_report.yml)

### 关于 Erlang/OTP 和 Flutter

如果你的问题是关于 Erlang、Flutter 或其他底层技术本身（而非 IMBoy 的使用），
建议前往对应社区提问：
If your question is about Erlang, Flutter, or underlying technologies (not IMBoy itself):

- Erlang 社区：[Erlang Forums](https://erlangforums.com/)
- Flutter 社区：[Flutter Community](https://flutter.dev/community)

---

## 安全漏洞 / Security Vulnerabilities

**请勿在公开 Issue 中披露安全漏洞！**
**Do NOT disclose security vulnerabilities in public issues!**

请阅读 [`SECURITY.md`](SECURITY.md) 了解负责任披露流程。
Please read [`SECURITY.md`](SECURITY.md) for the responsible disclosure process.

---

## 提问时请包含 / When asking, please include

1. **IMBoy 版本**：`cat VERSION` 输出 / Output of `cat VERSION`
2. **操作系统 / 部署环境**：Ubuntu 22.04 / macOS 等 / OS and deployment environment
3. **Docker 版本**：`docker --version && docker compose version`
4. **问题复现步骤** / Steps to reproduce
5. **相关日志**：`docker compose -f deploy/docker-compose.prod.yml logs <service>` 输出
   Relevant logs from `docker compose -f deploy/docker-compose.prod.yml logs <service>`

---

## 响应时间说明 / Response Time

IMBoy 是由志愿者维护的开源项目，我们会尽力及时响应，但无法保证 SLA。
IMBoy is maintained by volunteers. We will do our best to respond, but cannot guarantee SLAs.

| 渠道 / Channel | 预期响应 / Expected response |
|---|---|
| GitHub Issues（Bug） | 1–2 周 / 1–2 weeks |
| GitHub Discussions | 尽力响应 / Best effort |
| 安全漏洞（私密披露）/ Security (private) | 72 小时内确认 / Acknowledge within 72 hours |

---

*感谢你使用 IMBoy！/ Thank you for using IMBoy!*
