-- 合规密钥管理表
-- 用于三层加密架构中的 compliance_e2ee 模式
-- 合规密钥对允许指定人员在合规审计场景下解密消息

CREATE TABLE IF NOT EXISTS public.compliance_key (
    id BIGSERIAL PRIMARY KEY,
    key_id VARCHAR(64) NOT NULL UNIQUE,           -- 密钥标识符
    public_key TEXT NOT NULL,                      -- RSA 公钥 (PEM 格式)
    private_key_encrypted TEXT,                    -- 加密后的私钥（仅管理员可解密）
    algorithm VARCHAR(64) DEFAULT 'RSA-OAEP-256',  -- 加密算法
    status INT2 DEFAULT 1 NOT NULL,                -- 0:已撤销 1:活跃 2:已轮换
    created_by BIGINT DEFAULT 0,                   -- 创建者管理员ID
    revoked_at TIMESTAMPTZ,                        -- 撤销时间
    revoked_by BIGINT,                             -- 撤销者管理员ID
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 活跃密钥索引（快速查找当前可用的合规密钥）
CREATE INDEX IF NOT EXISTS idx_compliance_key_status
    ON public.compliance_key(status) WHERE status = 1;

COMMENT ON TABLE public.compliance_key IS '合规密钥管理表 - 三层加密架构';
COMMENT ON COLUMN public.compliance_key.status IS '0:已撤销 1:活跃 2:已轮换';
