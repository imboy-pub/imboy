-- Migration: APP 平滑升级降级策略
-- Date: 2026-04-07
-- Description: 为 app_version 表增加升级策略字段，新建版本策略配置表

-- ============================================================
-- 1. 扩展 app_version 表
-- ============================================================

-- 最低支持版本：低于此版本的客户端被强制升级
ALTER TABLE public.app_version
  ADD COLUMN IF NOT EXISTS min_supported_vsn varchar(40) NOT NULL DEFAULT '0.0.0';

-- 灰度发布比例：0-100，100 表示全量发布
ALTER TABLE public.app_version
  ADD COLUMN IF NOT EXISTS grayscale_percent smallint NOT NULL DEFAULT 100;

-- 升级类型：force=强制 / recommend=推荐 / silent=静默
ALTER TABLE public.app_version
  ADD COLUMN IF NOT EXISTS upgrade_type varchar(20) NOT NULL DEFAULT 'recommend';

-- 结构化更新日志 JSON 数组
-- 例: [{"tag":"新功能","text":"支持频道聊天"},{"tag":"修复","text":"消息偶尔丢失"}]
ALTER TABLE public.app_version
  ADD COLUMN IF NOT EXISTS changelog jsonb NOT NULL DEFAULT '[]';

-- 安装包文件大小（字节）
ALTER TABLE public.app_version
  ADD COLUMN IF NOT EXISTS file_size bigint NOT NULL DEFAULT 0;

-- 安装包 SHA256 校验值
ALTER TABLE public.app_version
  ADD COLUMN IF NOT EXISTS file_hash varchar(128) NOT NULL DEFAULT '';

COMMENT ON COLUMN public.app_version.min_supported_vsn IS '最低支持版本：低于此版本的客户端必须升级';
COMMENT ON COLUMN public.app_version.grayscale_percent IS '灰度发布比例：0-100，100 表示全量发布';
COMMENT ON COLUMN public.app_version.upgrade_type IS '升级类型：force=强制 / recommend=推荐 / silent=静默';
COMMENT ON COLUMN public.app_version.changelog IS '结构化更新日志 JSON 数组';
COMMENT ON COLUMN public.app_version.file_size IS '安装包文件大小（字节）';
COMMENT ON COLUMN public.app_version.file_hash IS '安装包 SHA256 校验值';

-- ============================================================
-- 2. 新建版本策略配置表（全局版本策略，每个平台一条记录）
-- ============================================================

CREATE TABLE IF NOT EXISTS public.app_version_policy (
    id                    bigint PRIMARY KEY,
    type                  varchar(20) NOT NULL,               -- android / ios / web
    min_vsn               varchar(40) NOT NULL DEFAULT '0.0.0', -- 全局最低支持版本
    grayscale_enabled     smallint NOT NULL DEFAULT 0,        -- 0=关闭灰度 1=启用灰度
    check_interval_hours  int NOT NULL DEFAULT 24,            -- 客户端检查间隔（小时）
    status                smallint NOT NULL DEFAULT 1,        -- 0=禁用 1=启用
    updated_at            timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at            timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT uk_app_version_policy_type UNIQUE (type)
)
TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.app_version_policy OWNER to imboy_user;

COMMENT ON TABLE public.app_version_policy IS 'APP版本策略配置表（每个平台一条记录）';
COMMENT ON COLUMN public.app_version_policy.id IS '主键 TSID';
COMMENT ON COLUMN public.app_version_policy.type IS '平台类型：android / ios / web';
COMMENT ON COLUMN public.app_version_policy.min_vsn IS '全局最低支持版本（兜底安全网）';
COMMENT ON COLUMN public.app_version_policy.grayscale_enabled IS '是否启用灰度：0=关闭直接全量 1=启用';
COMMENT ON COLUMN public.app_version_policy.check_interval_hours IS '客户端版本检查间隔（小时）';
COMMENT ON COLUMN public.app_version_policy.status IS '状态：0=禁用 1=启用';
COMMENT ON COLUMN public.app_version_policy.created_at IS '创建时间';
COMMENT ON COLUMN public.app_version_policy.updated_at IS '最后更新时间';

-- 初始化默认策略记录
INSERT INTO public.app_version_policy (id, type, min_vsn, grayscale_enabled, check_interval_hours, status)
VALUES
    (1, 'android', '0.0.0', 0, 24, 1),
    (2, 'ios',     '0.0.0', 0, 24, 1),
    (3, 'web',     '0.0.0', 0, 24, 1)
ON CONFLICT (type) DO NOTHING;
