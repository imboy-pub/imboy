-- 内容审核子系统：敏感词黑名单 + 消息人工复审队列
-- Content moderation: sensitive word blacklist + manual message review queue

CREATE TABLE IF NOT EXISTS sensitive_word (
    id         bigint       NOT NULL,                          -- TSID
    word       varchar(128) NOT NULL,
    category   varchar(32)  NOT NULL DEFAULT 'custom',
    severity   varchar(16)  NOT NULL DEFAULT 'medium',         -- low | medium | high
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX IF NOT EXISTS uk_sensitive_word_word ON sensitive_word(word);
CREATE INDEX IF NOT EXISTS idx_sensitive_word_category ON sensitive_word(category);
COMMENT ON TABLE sensitive_word IS '敏感词黑名单（内容审核）';

CREATE TABLE IF NOT EXISTS review_queue (
    id            bigint       NOT NULL,                       -- TSID
    msg_id        bigint       NOT NULL DEFAULT 0,             -- 关联消息 TSID
    msg_type      varchar(32)  NOT NULL DEFAULT 'text',
    content       text         NOT NULL DEFAULT '',
    from_id       bigint       NOT NULL DEFAULT 0,
    from_account  varchar(64)  NOT NULL DEFAULT '',
    to_id         bigint       NOT NULL DEFAULT 0,
    to_type       varchar(16)  NOT NULL DEFAULT 'user',        -- user | group | channel
    hit_words     text         NOT NULL DEFAULT '',            -- 逗号分隔命中词
    review_status varchar(16)  NOT NULL DEFAULT 'pending',     -- pending | approved | rejected
    reviewer_id   bigint,
    reason        text,
    reviewed_at   timestamp with time zone,
    created_at    timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (id)
);
CREATE INDEX IF NOT EXISTS idx_review_queue_status_created ON review_queue(review_status, created_at DESC);
COMMENT ON TABLE review_queue IS '消息人工复审队列（内容审核）';
