# Imboy Backend Module Map

> Date: 2026-03-15
> Scope: backend modular monolith domain boundaries and future public entry rules

## Hard Rules

- Cross-domain calls should converge on stable public logic or facade modules instead of reaching through `api/`, `ds/`, or `repo/` internals.
- `src/api/` and `src/adm/` stay as protocol adapters. They should not become the long-term public boundary for cross-domain reuse.
- Compatibility wrappers are allowed during migration, but new code should target the future public entry module for each domain.

## Domain Map

| Domain | Current file roots | Future public entry module | Forbidden direct dependencies |
|---|---|---|---|
| `identity` | `src/api/auth_handler.erl`, `src/api/passport_handler.erl`, `src/api/user_handler.erl`, `src/logic/auth_logic.erl`, `src/logic/passport_logic.erl`, `src/logic/user_logic.erl`, `src/ds/auth_ds.erl`, `src/ds/user_ds.erl`, `src/repo/user_repo.erl` | `src/logic/identity_logic.erl` | Other domains should not call `auth_handler`, `passport_handler`, `user_ds`, or `user_repo` directly |
| `messaging` | `src/api/msg_handler.erl`, `src/api/conversation_handler.erl`, `src/logic/message_router_logic.erl`, `src/logic/msg_*.erl`, `src/ds/message_ds.erl`, `src/ds/msg_*.erl`, `src/repo/msg_*.erl` | `src/logic/messaging_logic.erl` | Other domains should not call `msg_handler`, `message_router_logic`, `msg_c2c_ds`, or `msg_c2g_repo` directly |
| `social_graph` | `src/api/friend_handler.erl`, `src/api/friend_category_handler.erl`, `src/api/user_tag_handler.erl`, `src/api/user_tag_relation_handler.erl`, `src/logic/friend_logic.erl`, `src/logic/user_tag_logic.erl`, `src/repo/friend_repo.erl`, `src/repo/user_tag_repo.erl` | `src/logic/social_graph_logic.erl` | Other domains should not depend directly on `friend_handler`, `friend_repo`, `user_tag_handler`, or `user_tag_relation_repo` |
| `group_collab` | `src/api/group_*.erl`, `src/logic/group_*.erl`, `src/ds/group_*.erl`, `src/repo/group_*.erl` | `src/logic/group_collab_logic.erl` | Other domains should not reach directly into `group_handler`, `group_vote_logic`, `group_schedule_repo`, or `group_task_repo` |
| `channel_content` | `src/api/channel_handler.erl`, `src/logic/channel_*.erl`, `src/ds/channel_*.erl`, `src/repo/channel_*.erl` | `src/logic/channel_content_logic.erl` | Other domains should not call `channel_handler`, `channel_logic_message`, `channel_repo`, or `channel_subscription_repo` directly |
| `moment_social` | `src/api/moment_handler.erl`, `src/logic/moment_logic.erl`, `src/logic/moment_logic_notify.erl`, `src/ds/moment_ds.erl`, `src/repo/moment_*.erl` | `src/logic/moment_social_logic.erl` | Other domains should not call `moment_handler`, `moment_ds`, `moment_post_repo`, or `moment_comment_repo` directly |
| `security_privacy` | `src/api/e2ee_*.erl`, `src/logic/e2ee_*.erl`, `src/ds/e2ee_*.erl`, `src/repo/e2ee_*.erl` | `src/logic/security_privacy_logic.erl` | Other domains should not depend directly on `e2ee_handler`, `e2ee_transfer_logic`, or shard/backup repos |
| `ops_governance` | `src/api/feedback_handler.erl`, `src/api/report_handler.erl`, `src/api/app_version_handler.erl`, `src/logic/report_logic.erl`, `src/repo/feedback_repo.erl`, `src/repo/report_ticket_repo.erl`, `src/repo/app_version_repo.erl` | `src/logic/ops_governance_logic.erl` | Other domains should not call `feedback_handler`, `report_handler`, `feedback_repo`, or `report_ticket_repo` directly |

## Migration Note

The future public entry module names above are target boundaries for incremental migration. Thin wrappers are acceptable until callers converge and automated gates are in place.
