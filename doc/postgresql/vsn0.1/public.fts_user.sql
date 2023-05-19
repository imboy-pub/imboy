-- Table: public.fts_user

-- DROP TABLE IF EXISTS public."fts_user";

CREATE TABLE IF NOT EXISTS public."fts_user"
(
    user_id bigint NOT NULL,
    allow_search smallint NOT NULL DEFAULT 2,
    token tsvector,
    CONSTRAINT pk_fts_user_uid PRIMARY KEY (user_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.fts_user OWNER to imboy_user;

COMMENT ON TABLE public.fts_user IS '用户全文索引矢量信息表';

COMMENT ON COLUMN public.fts_user.user_id IS '用户唯一ID';

COMMENT ON COLUMN public.fts_user.allow_search IS '用户允许被搜索 1 是  2 否';
COMMENT ON COLUMN public.fts_user.token IS '搜索矢量信息';
CREATE INDEX user_fts_gin_idex ON public."fts_user" USING gin (token);


-- select * from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search=1 and fts.token @@ to_tsquery('jiebacfg', '硕士')

/*
select * from to_tsquery('jiebacfg', '小明硕士毕业于中国科学院计算所，后在日本京都大学深造')
select * from to_tsvector('jiebacfg', '小明硕士毕业于中国科学院计算所，后在日本京都大学深造')

INSERT INTO public."fts_user" (user_id, allow_search, token) VALUES
    (1, 1, to_tsvector('jiebacfg', '小明硕士毕业于中国科学院计算所，后在日本京都大学深造'))


https://m.imooc.com/wiki/sqlbase-sqlpractice6

DROP TABLE IF EXISTS article;
CREATE TABLE article
(
  id      serial PRIMARY KEY,
  title   varchar(40),
  content text
);

ALTER TABLE article ADD COLUMN fts tsvector;
UPDATE article
SET fts = setweight(to_tsvector('jiebacfg', title), 'A') ||
          setweight(to_tsvector('jiebacfg', content), 'B');
CREATE INDEX article_fts_gin_index ON article USING gin (fts);

-- http://javabin.cn/2018/pg_jieba.html
select * from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.token @@ to_tsquery('jiebacfg', '硕士小名')

(to_tsvector(‘jiebacfg’, “name”)
*/
