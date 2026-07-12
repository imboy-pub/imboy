-- 迁移 000036: E2EE 服务端加密密钥备份表（4S 模式，P0-B B2）
--   客户端用恢复口令派生密钥（PBKDF2-HMAC-SHA256）在本地加密"身份私钥 +
--   房间 session keys"打包后上传；服务端只存密文与 KDF 参数，零信任：
--   服务端不存在任何接触明文私钥的路径。换机恢复 = 新设备拉最新版本
--   密文包，本地口令派生解密。
--   Server-side encrypted key backup (Matrix 4S semantics): the server
--   stores ciphertext + client-declared KDF params only; all cryptography
--   happens client-side.

CREATE TABLE IF NOT EXISTS public.e2ee_key_backups (
    id bigint PRIMARY KEY,
    uid bigint NOT NULL,
    backup_version integer NOT NULL,
    algo varchar(40) NOT NULL DEFAULT 'pbkdf2-sha256/aes-256-gcm',
    kdf_salt text NOT NULL,
    kdf_iterations integer NOT NULL,
    encrypted_payload text NOT NULL,
    payload_hash varchar(128) NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uk_e2ee_key_backups_uid_version UNIQUE (uid, backup_version),
    CONSTRAINT chk_e2ee_key_backups_uid CHECK (uid > 0),
    CONSTRAINT chk_e2ee_key_backups_version CHECK (backup_version > 0),
    CONSTRAINT chk_e2ee_key_backups_iterations CHECK (kdf_iterations >= 100000),
    CONSTRAINT chk_e2ee_key_backups_salt_len CHECK (length(kdf_salt) <= 256)
);

COMMENT ON TABLE public.e2ee_key_backups IS 'E2EE 加密密钥备份（服务端只存密文，4S 模式）';
COMMENT ON COLUMN public.e2ee_key_backups.backup_version IS '备份版本号，客户端单调 +1，UNIQUE(uid,version) 防并发覆盖';
COMMENT ON COLUMN public.e2ee_key_backups.kdf_salt IS 'KDF 盐值 base64（客户端生成，恢复端派生用）';
COMMENT ON COLUMN public.e2ee_key_backups.kdf_iterations IS 'KDF 迭代次数（客户端声明 >=310000，DB 下限 100000 防降级攻击）';
COMMENT ON COLUMN public.e2ee_key_backups.encrypted_payload IS 'base64 密文包（身份私钥+房间 session keys），服务端不可解';
COMMENT ON COLUMN public.e2ee_key_backups.payload_hash IS '客户端算的 SHA-256 十六进制，恢复端校验完整性';
