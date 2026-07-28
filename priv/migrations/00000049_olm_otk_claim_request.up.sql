-- 00000049_olm_otk_claim_request.up.sql
-- E2EE-062：OTK claim 幂等租约（21-playbook E2EE-025「同 request id 重放 100 次只消费一次」）。
--
--   X3DH 的 one-time prekey 是一次性资源。此前 claim 没有任何幂等键——
--   客户端一次网络超时后的重试、或 app 重启后的重发，都会再消费一条 OTK；
--   恶意方重放同一请求即可定向耗尽某用户的池，把其所有新会话逼到复用同一条
--   fallback prekey（前向保密显著下降）。
--
--   claim_request_id：领取方生成的幂等键。租约按**领取方**隔离——
--   唯一索引含 claimed_by，换一个领取方拿同样的 request_id 不得命中他人的
--   claim 结果（否则 request_id 就成了越权读取他人已领 key 的通道）。
--
--   部分唯一索引只约束非空行：迁移前已 claimed 的审计行 claim_request_id 为
--   NULL，互不冲突；旧客户端不带 request_id 时也走 NULL，保持逐次消费语义。
--
--   并发原子性依赖此索引：两个并发同 request_id 的请求可能同时查空租约，
--   第二条 UPDATE 撞 23505 后由 repo 回查返回第一条的结果，不重复消费。
--   仅靠「先查后写」是 TOCTOU，索引不可省。
--
-- Non-breaking additive migration.

ALTER TABLE public.olm_one_time_key
    ADD COLUMN IF NOT EXISTS claim_request_id varchar(64);

COMMENT ON COLUMN public.olm_one_time_key.claim_request_id IS '领取方幂等键；同 (claimed_by, user_id, device_id, claim_request_id) 重放只消费一条 OTK。NULL=旧行/旧客户端';

CREATE UNIQUE INDEX IF NOT EXISTS uk_olm_otk_claim_request
    ON public.olm_one_time_key (claimed_by, user_id, device_id, claim_request_id)
    WHERE claim_request_id IS NOT NULL;
