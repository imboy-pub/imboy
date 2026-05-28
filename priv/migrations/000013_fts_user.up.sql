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
CREATE INDEX IF NOT EXISTS user_fts_gin_idex ON public."fts_user" USING gin (token);


-- select * from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search=1 and fts.token @@ to_tsquery('jiebacfg', '硕士')

/*
select * from to_tsquery('jiebacfg', '小明硕士毕业于中国科学院计算所，后在日本京都大学深造')
select * from to_tsvector('jiebacfg', '小明硕士毕业于中国科学院计算所，后在日本京都大学深造')

INSERT INTO public."fts_user" (user_id, allow_search, token) VALUES
    (1, 1, to_tsvector('jiebacfg', '小明硕士毕业于中国科学院计算所，后在日本京都大学深造'))


https://m.imooc.com/wiki/sqlbase-sqlpractice6

DROP TABLE IF EXISTS article;
CREATE TABLE IF NOT EXISTS article
(
  id      serial PRIMARY KEY,
  title   varchar(40),
  content text
);

ALTER TABLE article ADD COLUMN fts tsvector;
UPDATE article
SET fts = setweight(to_tsvector('jiebacfg', title), 'A') ||
          setweight(to_tsvector('jiebacfg', content), 'B');
CREATE INDEX IF NOT EXISTS article_fts_gin_index ON article USING gin (fts);

-- http://javabin.cn/2018/pg_jieba.html
select * from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.token @@ to_tsquery('jiebacfg', '硕士小名')

//用户备注包含“中国的软件世界的软件”句子
select * from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.token @@ to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', '软件中国')::text, ' <-> ', ' & '))

// ts_rank()：根据匹配词位的频率对向量进行排名。
// ts_rank_cd 给定文档向量和查询分词的覆盖密度排名，覆盖密度相似除了ts_rank考虑到匹配词位彼此的接近程度之外，排名不分先后
select ts_rank_cd(fts.token, to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', '软件中国')::text, ' <-> ', ' | '))) as rank,* from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.token @@ to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', '软件中国')::text, ' <-> ', ' | ')) order by rank desc

(to_tsvector(‘jiebacfg’, “name”)
*/

-- fts trigger function
-- FUNCTION: public.imboy_user_for_fts_fun()

-- DROP FUNCTION IF EXISTS public.imboy_user_for_fts_fun();

CREATE OR REPLACE FUNCTION public.imboy_user_for_fts_fun()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE NOT LEAKPROOF
AS $BODY$
begin
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM public.fts_user WHERE user_id = OLD.id;
    DELETE FROM public.user_setting WHERE user_id = OLD.id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    INSERT INTO public.fts_user (user_id, allow_search, token) VALUES (new.id, 2, setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C'))
    ON CONFLICT (user_id) DO UPDATE SET token = setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C');
    -- UPDATE public.fts_user SET token = setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
    --       setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
    --       setweight(to_tsvector('jiebacfg', new.region), 'B') WHERE user_id=NEW.id;
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    INSERT INTO public.fts_user (user_id, allow_search, token) VALUES (new.id, 1, setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C'));
  return new;
  END IF;
end;
$BODY$;

ALTER FUNCTION public.imboy_user_for_fts_fun()
    OWNER TO imboy_user;

-- fts trigger
-- 为用户全文索引创建触发器
DROP TRIGGER IF EXISTS imboy_for_fts_user ON public.user;
CREATE TRIGGER imboy_for_fts_user
    AFTER INSERT OR DELETE OR UPDATE
    OF nickname,sign,region
    ON public.user
    FOR EACH ROW
    EXECUTE FUNCTION public.imboy_user_for_fts_fun();
