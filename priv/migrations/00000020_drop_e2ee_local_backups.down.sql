-- 回滚：重建表结构（不恢复数据，DROP 已不可逆）
CREATE TABLE public.e2ee_local_backups (
    id bigint NOT NULL,
    uid bigint NOT NULL,
    device_id character varying(64) NOT NULL,
    backup_version integer NOT NULL,
    key_checksum character varying(128) NOT NULL,
    file_size bigint DEFAULT 0,
    user_notes text DEFAULT ''::text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT e2ee_local_backups_backup_version_check CHECK ((backup_version > 0)),
    CONSTRAINT e2ee_local_backups_file_size_check CHECK ((file_size >= 0)),
    CONSTRAINT e2ee_local_backups_uid_check CHECK ((uid > 0))
);

ALTER TABLE ONLY public.e2ee_local_backups
    ADD CONSTRAINT e2ee_local_backups_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.e2ee_local_backups
    ADD CONSTRAINT uk_e2ee_local_backup_version UNIQUE (uid, device_id, backup_version);

CREATE INDEX idx_e2ee_local_backups_created_at ON public.e2ee_local_backups USING btree (created_at);
CREATE INDEX idx_e2ee_local_backups_device_id ON public.e2ee_local_backups USING btree (device_id);
CREATE INDEX idx_e2ee_local_backups_uid ON public.e2ee_local_backups USING btree (uid);

COMMENT ON TABLE public.e2ee_local_backups IS 'E2EE 本地备份元数据表（仅记录备份历史）';
