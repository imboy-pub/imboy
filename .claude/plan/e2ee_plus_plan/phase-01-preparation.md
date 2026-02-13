# 阶段 1: 准备工作

> **预计工期**: 1-2 天
> **依赖**: 无
> **交付物**: 数据库迁移文件、错误码定义

---

## 目标

完成基础设施准备工作：
1. 创建数据库迁移文件
2. 添加新的错误码定义
3. 验证测试环境

---

## 步骤 1: 创建数据库迁移文件

### 1.1 创建迁移文件

```bash
# 在后端项目目录执行
cd /Users/leeyi/project/imboy.pub/imboy

# 创建迁移文件
vim priv/migrations/00000045_e2ee_key_recovery.sql
```

### 1.2 迁移文件内容

```sql
-- ================================================================
-- E2EE 密钥恢复功能 - 数据库迁移
-- 版本: 00000045
-- 日期: 2026-01-30
-- 说明: 添加设备传输、社交恢复、本地备份功能所需的数据表
-- ================================================================

-- ================================================================
-- 1. 设备传输会话表
-- 用途: 管理设备间私钥传输会话
-- ================================================================
CREATE TABLE e2ee_transfer_sessions (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 会话标识（UUID v4，用于客户端识别）
    session_id VARCHAR(48) NOT NULL UNIQUE,

    -- 传输方信息
    from_uid INTEGER NOT NULL,
    from_device_id VARCHAR(64) NOT NULL,

    -- 接收方信息
    to_uid INTEGER NOT NULL,
    to_device_id VARCHAR(64),

    -- 会话状态: pending, accepted, confirmed, expired, cancelled
    status VARCHAR(20) NOT NULL DEFAULT 'pending',

    -- 加密的密钥包（客户端加密，服务器不解密）
    -- 格式: base64(RSA-OAEP-256(encrypted_key_bundle))
    encrypted_key_bundle TEXT NOT NULL,

    -- 会话过期时间（创建后 5 分钟过期）
    expires_at TIMESTAMP NOT NULL,

    -- 创建时间
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 确认时间
    confirmed_at TIMESTAMP,

    -- 索引
    CONSTRAINT e2ee_transfer_sessions_from_uid_check CHECK (from_uid > 0),
    CONSTRAINT e2ee_transfer_sessions_to_uid_check CHECK (to_uid > 0)
);

-- 创建索引
CREATE INDEX idx_e2ee_transfer_sessions_session_id ON e2ee_transfer_sessions(session_id);
CREATE INDEX idx_e2ee_transfer_sessions_from_uid ON e2ee_transfer_sessions(from_uid);
CREATE INDEX idx_e2ee_transfer_sessions_to_uid ON e2ee_transfer_sessions(to_uid);
CREATE INDEX idx_e2ee_transfer_sessions_status ON e2ee_transfer_sessions(status);
CREATE INDEX idx_e2ee_transfer_sessions_expires_at ON e2ee_transfer_sessions(expires_at);

-- 注释
COMMENT ON TABLE e2ee_transfer_sessions IS 'E2EE 设备间传输会话表';
COMMENT ON COLUMN e2ee_transfer_sessions.session_id IS '会话唯一标识（UUID v4）';
COMMENT ON COLUMN e2ee_transfer_sessions.encrypted_key_bundle IS '加密的密钥包，服务器不解密';
COMMENT ON COLUMN e2ee_transfer_sessions.expires_at IS '会话过期时间（5分钟）';

-- ================================================================
-- 2. 可信联系人表
-- 用途: 管理社交恢复功能中的可信好友
-- ================================================================
CREATE TABLE e2ee_trusted_contacts (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 用户 ID
    uid INTEGER NOT NULL,

    -- 可信联系人 ID
    contact_uid INTEGER NOT NULL,

    -- 联系人昵称（可选，方便识别）
    contact_nickname VARCHAR(100),

    -- 状态: active（活跃）, removed（已移除）
    status VARCHAR(20) NOT NULL DEFAULT 'active',

    -- 创建时间
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 更新时间
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 唯一约束：一个用户不能重复添加同一联系人
    UNIQUE(uid, contact_uid),

    -- 约束检查
    CONSTRAINT e2ee_trusted_contacts_uid_check CHECK (uid > 0),
    CONSTRAINT e2ee_trusted_contacts_contact_uid_check CHECK (contact_uid > 0),
    CONSTRAINT e2ee_trusted_contacts_status_check CHECK (status IN ('active', 'removed'))
);

-- 创建索引
CREATE INDEX idx_e2ee_trusted_contacts_uid ON e2ee_trusted_contacts(uid);
CREATE INDEX idx_e2ee_trusted_contacts_contact_uid ON e2ee_trusted_contacts(contact_uid);
CREATE INDEX idx_e2ee_trusted_contacts_status ON e2ee_trusted_contacts(status);

-- 注释
COMMENT ON TABLE e2ee_trusted_contacts IS 'E2EE 可信联系人表（社交恢复）';
COMMENT ON COLUMN e2ee_trusted_contacts.contact_nickname IS '联系人昵称，方便用户识别';

-- ================================================================
-- 3. 密钥分片表
-- 用途: 存储社交恢复中的密钥分片（加密存储）
-- ================================================================
CREATE TABLE e2ee_key_shares (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 密钥所有者 ID
    owner_uid INTEGER NOT NULL,

    -- 受托人 ID（存储密钥分片的好友）
    trustee_uid INTEGER NOT NULL,

    -- 加密的密钥分片（使用受托人的公钥加密）
    -- 格式: base64(RSA-OAEP-256(encrypted_share))
    encrypted_share TEXT NOT NULL,

    -- 分片索引（1-3，表示第几个分片）
    share_index INTEGER NOT NULL,

    -- 恢复阈值（需要多少个分片才能恢复，默认 2）
    threshold INTEGER NOT NULL DEFAULT 2,

    -- 总分片数（默认 3）
    total_shares INTEGER NOT NULL DEFAULT 3,

    -- 创建时间
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 更新时间
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 唯一约束：一个受托人只能存储所有者的一个分片
    UNIQUE(owner_uid, trustee_uid),

    -- 约束检查
    CONSTRAINT e2ee_key_shares_owner_uid_check CHECK (owner_uid > 0),
    CONSTRAINT e2ee_key_shares_trustee_uid_check CHECK (trustee_uid > 0),
    CONSTRAINT e2ee_key_shares_share_index_check CHECK (share_index >= 1 AND share_index <= 3),
    CONSTRAINT e2ee_key_shares_threshold_check CHECK (threshold >= 1 AND threshold <= total_shares),
    CONSTRAINT e2ee_key_shares_total_shares_check CHECK (total_shares >= 2 AND total_shares <= 5)
);

-- 创建索引
CREATE INDEX idx_e2ee_key_shares_owner_uid ON e2ee_key_shares(owner_uid);
CREATE INDEX idx_e2ee_key_shares_trustee_uid ON e2ee_key_shares(trustee_uid);
CREATE INDEX idx_e2ee_key_shares_share_index ON e2ee_key_shares(share_index);

-- 注释
COMMENT ON TABLE e2ee_key_shares IS 'E2EE 密钥分片表（社交恢复）';
COMMENT ON COLUMN e2ee_key_shares.encrypted_share IS '加密的密钥分片，使用受托人公钥加密';
COMMENT ON COLUMN e2ee_key_shares.share_index IS '分片索引（1-3）';
COMMENT ON COLUMN e2ee_key_shares.threshold IS '恢复阈值（需要多少分片才能恢复）';

-- ================================================================
-- 4. 本地备份元数据表
-- 用途: 记录用户的本地备份历史（仅元数据，不包含实际私钥）
-- ================================================================
CREATE TABLE e2ee_local_backups (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 用户 ID
    uid INTEGER NOT NULL,

    -- 设备 ID
    device_id VARCHAR(64) NOT NULL,

    -- 备份版本号（递增）
    backup_version INTEGER NOT NULL,

    -- 密钥校验和（SHA-256，用于验证备份完整性）
    key_checksum VARCHAR(64) NOT NULL,

    -- 创建时间
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- 约束检查
    CONSTRAINT e2ee_local_backups_uid_check CHECK (uid > 0),
    CONSTRAINT e2ee_local_backups_backup_version_check CHECK (backup_version > 0)
);

-- 创建索引
CREATE INDEX idx_e2ee_local_backups_uid ON e2ee_local_backups(uid);
CREATE INDEX idx_e2ee_local_backups_device_id ON e2ee_local_backups(device_id);
CREATE INDEX idx_e2ee_local_backups_created_at ON e2ee_local_backups(created_at);

-- 注释
COMMENT ON TABLE e2ee_local_backups IS 'E2EE 本地备份元数据表（仅记录备份历史）';
COMMENT ON COLUMN e2ee_local_backups.key_checksum IS '密钥校验和（SHA-256），用于验证备份完整性';

-- ================================================================
-- 数据清理规则
-- ================================================================

-- 定期清理过期的传输会话（建议使用 pg_cron 或应用层定时任务）
-- DELETE FROM e2ee_transfer_sessions WHERE expires_at < NOW() AND status != 'confirmed';

-- ================================================================
-- 完成标记
-- ================================================================
-- 迁移完成后，运行以下验证查询：

-- 1. 检查表是否创建成功
SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE 'e2ee_%';

-- 预期结果:
-- tablename
-- -------------------
-- e2ee_key_shares
-- e2ee_local_backups
-- e2ee_transfer_sessions
-- e2ee_trusted_contacts

-- 2. 检查索引是否创建成功
SELECT indexname FROM pg_indexes WHERE schemaname = 'public' AND tablename LIKE 'e2ee_%';

-- 3. 检查表注释
SELECT obj_description('e2ee_transfer_sessions'::regclass);
```

### 1.3 执行迁移

```bash
# 方法 1: 使用 psql 命令
psql -h localhost -U imboy -d imboy -f priv/migrations/00000045_e2ee_key_recovery.sql

# 方法 2: 使用应用内置迁移工具
make rel
_rel/imboy/bin/imboy eval "imboy_migrate:up()."

# 方法 3: 手动在节点中执行
erl -name imboy@127.0.0.1 -setcookie imboy -pa _rel/imboy/lib/*/ebin
```

### 1.4 验证迁移

```erlang
% 在节点 shell 中执行验证

% 1. 检查表是否存在
{ok, _, [{Tables}]} = elib_pg:query(
    <<"SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE 'e2ee_%'">>,
    []
),
io:format("Created tables: ~p~n", [Tables]).

% 预期输出:
% Created tables: [[<<"e2ee_key_shares">>, <<"e2ee_local_backups">>,
%                   <<"e2ee_transfer_sessions">>, <<"e2ee_trusted_contacts">>]]

% 2. 检查表注释
{ok, _, [{Comment}]} = elib_pg:query(
    <<"SELECT obj_description('e2ee_transfer_sessions'::regclass)">>,
    []
),
io:format("Table comment: ~s~n", [Comment]).

% 预期输出:
% Table comment: <<"E2EE 设备间传输会话表">>
```

---

## 步骤 2: 添加错误码定义

### 2.1 编辑错误码文件

```bash
vim include/error_code.hrl
```

### 2.2 添加新错误码

在文件末尾添加以下错误码定义：

```erlang
%% ================================================================
%% E2EE 密钥恢复功能错误码 (5000-5099)
%% ================================================================

%% --- 设备传输相关 (5000-5019) ---

-define(ERR_E2EE_TRANSFER_INVALID_SESSION, 5000).
-define(ERR_E2EE_TRANSFER_SESSION_EXPIRED, 5001).
-define(ERR_E2EE_TRANSFER_SESSION_NOT_FOUND, 5002).
-define(ERR_E2EE_TRANSFER_INVALID_DEVICE, 5003).
-define(ERR_E2EE_TRANSFER_ALREADY_ACCEPTED, 5004).
-define(ERR_E2EE_TRANSFER_CANNOT_CONFIRM, 5005).
-define(ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH, 5006).
-define(ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH, 5007).

%% --- 社交恢复相关 (5020-5039) ---

-define(ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND, 5020).
-define(ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS, 5021).
-define(ERR_E2EE_SOCIAL_CONTACT_IS_SELF, 5022).
-define(ERR_E2EE_SOCIAL_CONTACT_NOT_TRUSTED, 5023).
-define(ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES, 5024).
-define(ERR_E2EE_SOCIAL_SHARE_ALREADY_CREATED, 5025).
-define(ERR_E2EE_SOCIAL_SHARE_NOT_FOUND, 5026).
-define(ERR_E2EE_SOCIAL_INVALID_THRESHOLD, 5027).
-define(ERR_E2EE_SOCIAL_RECOVER_FAILED, 5028).
-define(ERR_E2EE_SOCIAL_TRUSTEE_LIMIT_EXCEEDED, 5029).

%% --- 本地备份相关 (5040-5049) ---

-define(ERR_E2EE_BACKUP_INVALID_PASSWORD, 5040).
-define(ERR_E2EE_BACKUP_FILE_CORRUPTED, 5041).
-define(ERR_E2EE_BACKUP_VERSION_MISMATCH, 5042).
-define(ERR_E2EE_BACKUP_CHECKSUM_MISMATCH, 5043).

%% --- 通用错误 (5050-5099) ---

-define(ERR_E2EE_INVALID_KEY_FORMAT, 5050).
-define(ERR_E2EE_KEY_DERIVATION_FAILED, 5051).
-define(ERR_E2EE_ENCRYPTION_FAILED, 5052).
-define(ERR_E2EE_DECRYPTION_FAILED, 5053).
-define(ERR_E2EE_KEY_NOT_FOUND, 5054).
-define(ERR_E2EE_OPERATION_NOT_SUPPORTED, 5055).
```

### 2.3 添加错误消息映射

编辑 `src/imboy_error.erl`，添加错误消息映射：

```erlang
%% E2EE 密钥恢复错误消息
error_msg(?ERR_E2EE_TRANSFER_INVALID_SESSION) ->
    <<"无效的传输会话"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_SESSION_EXPIRED) ->
    <<"传输会话已过期"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND) ->
    <<"传输会话不存在"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_INVALID_DEVICE) ->
    <<"无效的设备"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED) ->
    <<"传输会话已被接受"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_CANNOT_CONFIRM) ->
    <<"无法确认传输会话"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH) ->
    <<"发送方用户 ID 不匹配"/utf8>>;
error_msg(?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH) ->
    <<"接收方用户 ID 不匹配"/utf8>>;

error_msg(?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND) ->
    <<"可信联系人不存在"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS) ->
    <<"可信联系人已存在"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_CONTACT_IS_SELF) ->
    <<"不能添加自己为可信联系人"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_CONTACT_NOT_TRUSTED) ->
    <<"该联系人不在可信列表中"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES) ->
    <<"密钥分片数量不足"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_SHARE_ALREADY_CREATED) ->
    <<"密钥分片已创建"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_SHARE_NOT_FOUND) ->
    <<"密钥分片不存在"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_INVALID_THRESHOLD) ->
    <<"无效的恢复阈值"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_RECOVER_FAILED) ->
    <<"密钥恢复失败"/utf8>>;
error_msg(?ERR_E2EE_SOCIAL_TRUSTEE_LIMIT_EXCEEDED) ->
    <<"受托人数量超过限制"/utf8>>;

error_msg(?ERR_E2EE_BACKUP_INVALID_PASSWORD) ->
    <<"备份密码错误"/utf8>>;
error_msg(?ERR_E2EE_BACKUP_FILE_CORRUPTED) ->
    <<"备份文件已损坏"/utf8>>;
error_msg(?ERR_E2EE_BACKUP_VERSION_MISMATCH) ->
    <<"备份版本不匹配"/utf8>>;
error_msg(?ERR_E2EE_BACKUP_CHECKSUM_MISMATCH) ->
    <<"备份校验和不匹配"/utf8>>;

error_msg(?ERR_E2EE_INVALID_KEY_FORMAT) ->
    <<"无效的密钥格式"/utf8>>;
error_msg(?ERR_E2EE_KEY_DERIVATION_FAILED) ->
    <<"密钥派生失败"/utf8>>;
error_msg(?ERR_E2EE_ENCRYPTION_FAILED) ->
    <<"加密失败"/utf8>>;
error_msg(?ERR_E2EE_DECRYPTION_FAILED) ->
    <<"解密失败"/utf8>>;
error_msg(?ERR_E2EE_KEY_NOT_FOUND) ->
    <<"密钥不存在"/utf8>>;
error_msg(?ERR_E2EE_OPERATION_NOT_SUPPORTED) ->
    <<"不支持的操作"/utf8>>;

error_msg(Code) when Code >= 5000, Code =< 5099 ->
    <<"E2EE 操作失败"/utf8>>;
```

### 2.4 重新编译项目

```bash
# 编译项目
make compile

# 验证错误码定义
erl -noshell -eval "io:format('Error codes loaded: ~p~n', [?ERR_E2EE_TRANSFER_INVALID_SESSION])" -s init stop

# 或者检查头文件
grep "ERR_E2EE" include/error_code.hrl
```

---

## 步骤 3: 验证测试环境

### 3.1 检查数据库连接

```erlang
% 在节点 shell 中执行

% 设置测试环境
application:set_env(imboy, env, test).

% 测试数据库连接
{ok, _, [{Result}]} = elib_pg:query(<<"SELECT 1 as test">>, []),
io:format("Database connection test: ~p~n", [Result]).
```

### 3.2 检查新表

```erlang
% 检查新表是否存在
Tables = [
    <<"e2ee_transfer_sessions">>,
    <<"e2ee_trusted_contacts">>,
    <<"e2ee_key_shares">>,
    <<"e2ee_local_backups">
],

CheckFun = fun(Table) ->
    case elib_pg:query(<<"SELECT COUNT(*) FROM ">>, []) of
        {ok, _, [{_}]} -> {Table, ok};
        {error, Reason} -> {Table, {error, Reason}}
    end
end,

Results = lists:map(CheckFun, Tables),
io:format("Table check results: ~p~n", [Results]).
```

### 3.3 测试错误码

```erlang
% 测试错误码是否正确定义
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

error_code_test() ->
    % 测试错误码唯一性
    ?assertEqual(5000, ?ERR_E2EE_TRANSFER_INVALID_SESSION),
    ?assertEqual(5020, ?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND),
    ?assertEqual(5040, ?ERR_E2EE_BACKUP_INVALID_PASSWORD),
    ok.
```

---

## 完成检查清单

执行完本阶段后，请确认以下事项：

- [ ] 数据库迁移文件已创建
- [ ] 4 个新表已创建
- [ ] 所有索引已创建
- [ ] 表注释已添加
- [ ] 错误码已添加到 `include/error_code.hrl`
- [ ] 错误消息已添加到 `src/imboy_error.erl`
- [ ] 项目已重新编译
- [ ] 数据库连接测试通过
- [ ] 新表验证通过
- [ ] 错误码测试通过

---

## 常见问题

### Q: 如果迁移失败怎么办？

A:
1. 检查 PostgreSQL 版本是否 >= 18
2. 检查数据库用户权限
3. 查看详细错误日志
4. 可以手动回滚：`DROP TABLE IF EXISTS e2ee_*;`

### Q: 错误码范围冲突怎么办？

A: 当前使用 5000-5099，如果与现有错误码冲突，请调整到未使用的范围。

### Q: 如何回滚迁移？

A: 创建回滚脚本 `00000045_rollback_e2ee_key_recovery.sql`：

```sql
DROP TABLE IF EXISTS e2ee_transfer_sessions CASCADE;
DROP TABLE IF EXISTS e2ee_trusted_contacts CASCADE;
DROP TABLE IF EXISTS e2ee_key_shares CASCADE;
DROP TABLE IF EXISTS e2ee_local_backups CASCADE;
```

---

## 下一阶段

完成本阶段后，请继续执行：
- [阶段 2: 设备间传输](./phase-02-device-transfer.md)

---

**最后更新**: 2026-01-30
**作者**: Claude AI Planning Agent
