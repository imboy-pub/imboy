<p align="center">
  <img src="./docs/brand/logo/imboy_logo_2474E5_0512.png" alt="IMBoy" width="140" />
</p>

<h1 align="center">IMBoy</h1>

<p align="center">
  <strong>High-performance, self-hostable, end-to-end encrypted open-source instant messaging SKU</strong>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/version-1.0.0--rc.1-2474E5" alt="version" />
  <img src="https://img.shields.io/badge/license-MulanPSL--2.0-blue" alt="license" />
  <img src="https://img.shields.io/badge/Erlang%2FOTP-28%2B-A90533" alt="erlang" />
  <img src="https://img.shields.io/badge/PostgreSQL-18%2B-336791" alt="postgres" />
  <img src="https://img.shields.io/badge/Flutter-3.41%2B-02569B" alt="flutter" />
  <img src="https://img.shields.io/badge/React-19.2-61DAFB" alt="react" />
</p>

<p align="center">
  <a href="README.md">简体中文</a> | <strong>English</strong>
</p>

---

IMBoy is an open-source instant messaging system designed for **1 million concurrent connections on a single node**, covering 10 complete feature lines including one-on-one chat, group chat, channels, moments, and end-to-end encryption. The backend is built on Erlang/OTP + Cowboy + PostgreSQL; clients cover iOS/Android/macOS/Windows/Linux (Flutter) with a React admin console — targeting enterprises, SaaS teams, and independent developers who need self-hosted IM.

This repository is a **multi-project workspace** (not a single Git repo). All three components (backend / app / admin) can be released independently; the workspace-level version is managed in the `VERSION` file (currently `1.0.0-rc.1`).

## Why IMBoy

- **End-to-end encryption (disabled by default, opt-in)**: RSA-OAEP-256 + AES-256-GCM envelope suite; the server never decrypts `ciphertext`; includes social recovery and device migration. ⚠️ The client currently defaults to a "transport encryption + server-side at-rest encryption" path (messages are readable by the server); true E2E is opt-in and requires the key-recovery subsystem (on the roadmap). No forward secrecy (PFS); not third-party audited.
- **Strict message ordering**: Persistent messages use a `conv_seq` cursor (monotonically increasing per conversation). Strict replay is guaranteed across data centers and nodes without relying on TSID for ordering.
- **High-concurrency foundation**: Erlang/OTP is inherently distributed. "1M concurrent on a single node" is an architectural design target pending a reproducible third-party benchmark; single-node deployment is currently recommended (multi-node horizontal scaling is on the roadmap — cross-node ACK dedup state migration is in progress).
- **Complete three-component delivery**: Flutter client (including 12 E2EE settings pages) + React admin console (27 unit + 4 E2E tests) + Erlang backend (382 files, 90k+ lines, 0 functional TODOs).
- **One-command self-hosting**: `deploy/docker-compose.prod.yml` starts PG18 + backend + admin + Caddy (auto-TLS) in one command. A `/setup` wizard creates the super admin — no `erl shell` needed.
- **MulanPSL-2.0 open source**: Business-friendly Chinese open-source license, unified across all three components.

## 10 Feature Lines

| # | Feature | Completeness | Highlights |
|---|---|---|---|
| 1 | One-on-one chat (C2C) | 100% | WAL zero-loss / recall / edit / read receipts / burn-after-read / quote reply |
| 2 | Group chat (C2G) | 100% | Mute / @mention / admin @all / batch delivery / read statistics |
| 3 | Conversation management | 100% | Merged C2C+C2G / soft delete / pin / pagination |
| 4 | Push notifications | 100% | FCM + APNs (backend) + client local notifications |
| 5 | WebSocket / ACK | 100% | 4-step retry 2s→5s→7s→11s / syn cross-node broadcast / 120s heartbeat |
| 6 | End-to-end encryption (E2EE) | Implemented · **disabled by default in client** | RSA-OAEP-256 + AES-256-GCM / social recovery / device migration; defaults to plaintext + server-side at-rest encryption, true E2E is opt-in; no PFS, not audited |
| 7 | Tag system | 100% | Friend grouping / favorites namespace / cascade delete |
| 8 | Favorites system | 100% | Text / image / audio / video / file / location / contact |
| 9 | Channel system | 100% | Subscribe / publish / admin / invite / paid / statistics |
| 10 | Moments | 100% | ACL privacy / comments / likes / reporting / admin review |

See [`IMBOY_FEATURE_PROGRESS.md`](./IMBOY_FEATURE_PROGRESS.md) for details.

## System Architecture

```
                   ┌─────────────────────────────────────┐
                   │            Clients                  │
                   │  Flutter App   │   React Admin      │
                   │  (iOS/Android/ │   (imboy-admin-    │
                   │   Desktop)     │    frontend)       │
                   └────────┬───────┴─────────┬──────────┘
                            │ WSS + HTTPS     │ HTTPS
                            ▼                 ▼
                   ┌─────────────────────────────────────┐
                   │          Caddy (auto TLS)           │
                   └────────────────┬────────────────────┘
                                    ▼
                   ┌─────────────────────────────────────┐
                   │        IMBoy Backend (Erlang)       │
                   │  ┌──────────────────────────────┐   │
                   │  │ Handler → Logic → DS → Repo  │   │
                   │  └──────────────────────────────┘   │
                   │  Cowboy 2.10 │ syn │ depcache      │
                   └────────┬───────────────────┬────────┘
                            ▼                   ▼
                   ┌────────────────┐  ┌────────────────┐
                   │ PostgreSQL 18+ │  │   FastDFS      │
                   │ + pg_jieba     │  │  (file store)  │
                   │ + postgis      │  └────────────────┘
                   │ + timescaledb  │
                   │ + pgcrypto     │
                   └────────────────┘
```

For detailed layers, module index and Mermaid diagrams see [`imboy/CLAUDE.md`](./imboy/CLAUDE.md).

## Sub-projects

| Directory | Description | Stack |
|---|---|---|
| [`imboy/`](./imboy) | Erlang/Cowboy IM backend | Erlang/OTP 28+ · Cowboy 2.10 · PostgreSQL 18+ |
| [`imboyapp/`](./imboyapp) | Flutter cross-platform client | Flutter 3.41+ · Riverpod · Dart 3 |
| [`imboy-admin-frontend/`](./imboy-admin-frontend) | React admin console | React 19.2 · TypeScript · Vite · Bun |
| [`elib/`](./elib) | Shared Erlang library | Erlang/OTP |
| [`go-fastdfs/`](./go-fastdfs) | File storage component | Go |
| [`deploy/`](./deploy) | One-command production deployment | Docker Compose · Caddy |

## Quick Start (Production)

**Prerequisites**: A Linux server (Ubuntu 22.04 / Debian 12 / Alma 9 recommended), Docker 24+, two domain names pointing to the server, ports 80/443 open.

```bash
# 1. Clone the workspace
git clone https://github.com/imboy-pub/imboy.git
cd imboy/deploy

# 2. Prepare environment variables
cp .env.example .env
$EDITOR .env          # Set API_DOMAIN / ADMIN_DOMAIN / secret keys

# 3. Pre-flight check
bash ../script/preflight.sh --docker

# 4. Start
docker network create imboy-network 2>/dev/null || true
docker compose -f docker-compose.prod.yml up -d

# 5. Initialize
#    Open https://${ADMIN_DOMAIN} in your browser
#    Auto-redirects to /setup wizard → fill phone/email + strong password + nickname
#    Redirects back to /login on completion
```

**No `erl shell` needed** — the first-run setup wizard (P0-5) implements a dual guard: config flag + `adm_user` table existence check. It can only run once successfully.

For the full deployment guide see [`deploy/README.md`](./deploy/README.md).

## Local Development

### Backend

```bash
cd imboy
make compile
make eunit
IMBOYENV=local make run
```

### Flutter Client

```bash
cd imboyapp
flutter pub get
flutter run --dart-define=APP_ENV=local_home
```

10-feature-line automation entry:

```bash
cd imboyapp
bash test_automation/scripts/run_yaml_mapped_suite.sh --dry-run
```

### Admin Console

```bash
cd imboy-admin-frontend
bun install
bun run dev         # Dev server
bun test            # Unit tests
bun run test:e2e    # Playwright E2E (includes 6 P0-5 setup wizard tests)
```

## Documentation

| Topic | Link |
|---|---|
| Backend architecture (DDD 4 layers) | [`imboy/CLAUDE.md`](./imboy/CLAUDE.md) |
| Admin console conventions | [`imboy-admin-frontend/CLAUDE.md`](./imboy-admin-frontend/CLAUDE.md) |
| WebSocket API v2 | [`imboy/doc/api/websocket-api-2.md`](./imboy/doc/api/websocket-api-2.md) |
| Deployment guide | [`deploy/README.md`](./deploy/README.md) |
| Feature completeness audit | [`IMBOY_FEATURE_PROGRESS.md`](./IMBOY_FEATURE_PROGRESS.md) |
| Changelog | [`CHANGELOG.md`](./CHANGELOG.md) |
| Observability / Sentry | [`imboy/doc/operations/observability.md`](./imboy/doc/operations/observability.md) |
| Backup & restore | [`imboy/doc/operations/deployment/BACKUP-RESTORE.md`](./imboy/doc/operations/deployment/BACKUP-RESTORE.md) |
| Privacy policy template | [`PRIVACY_POLICY_TEMPLATE.md`](./PRIVACY_POLICY_TEMPLATE.md) |
| Security disclosure | [`SECURITY.md`](./SECURITY.md) |
| Contributing guide | [`CONTRIBUTING.md`](./CONTRIBUTING.md) |
| Code of conduct | [`CODE_OF_CONDUCT.md`](./CODE_OF_CONDUCT.md) |
| Roadmap | [`ROADMAP.md`](./ROADMAP.md) |
| Support | [`SUPPORT.md`](./SUPPORT.md) |

## Roadmap

- **1.0.0-rc.1** — Current release candidate; first standard SKU. P0-5 first-run wizard shipped; all three components functionally complete.
- **1.0.0** — Sentry DSN production injection, Grafana dashboards, upgrade runbook, brand assets, seed_demo script.
- **1.1.x** — OpenAPI/AsyncAPI schema freeze, docs-site (VitePress), iOS App Store release.
- **Enterprise** — Enhanced audit logs, multi-tenant isolation, advanced RBAC, SSO.

See [`ROADMAP.md`](./ROADMAP.md) for the full roadmap.

## Contributing

All contributions are welcome — bug reports, feature requests, documentation fixes, and code submissions. Please read first:

- [Contributing Guide](./CONTRIBUTING.md)
- [Code of Conduct](./CODE_OF_CONDUCT.md)
- [Security Disclosure Process](./SECURITY.md) (**do not** report security vulnerabilities in public issues)

Before submitting a PR, ensure local checks pass for the relevant component:

```bash
cd imboy && make eunit && make dialyze          # Backend
cd imboyapp && flutter test                     # Flutter
cd imboy-admin-frontend && bun test             # Admin console
```

## Workspace Constraints

- The root directory is not a unified Git repository; `imboy`, `imboyapp`, and `imboy-admin-frontend` each maintain their own Git state.
- Each sub-project's `CLAUDE.md` is the authoritative source for AI context and project conventions.
- This README contains only stable, actionable, traceable information — no temporary notes, credentials, API keys, or prompt residue.

## License

[MulanPSL-2.0](./LICENSE) © IMBoy Contributors

All three components (`imboy/` `imboyapp/` `imboy-admin-frontend/`) use the same license; a full copy is retained in each directory.
