-- Table: public.geo_people_nearby

-- DROP TABLE IF EXISTS public."geo_people_nearby";

CREATE TABLE IF NOT EXISTS public."geo_people_nearby"
(
    user_id bigint NOT NULL,
    location geometry,
    CONSTRAINT pk_people_nearby_uid PRIMARY KEY (user_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.geo_people_nearby OWNER to imboy_user;

COMMENT ON TABLE public.geo_people_nearby IS '附近的人';

CREATE INDEX i_people_nearby_location ON public.geo_people_nearby USING GIST(location);

-- 一条 SQL 实现查询附近的人
-- https://cloud.tencent.com/document/product/409/72289


-- WGS84 是目前最流行的地理坐标系统。在国际上，每个坐标系统都会被分配一个 EPSG 代码，EPSG:4326 就是 WGS84 的代码。GPS 是基于 WGS84 的，所以通常我们得到的坐标数据都是 WGS84 的。一般我们在存储数据时，仍然按 WGS84 存储。
-- INSERT INTO public.geo_people_nearby(user_id, location) VALUES(1, ST_GeomFromText('POINT(-71.060316 48.432044)', 4326));

-- INSERT INTO public.geo_people_nearby(user_id, location) VALUES(1, ST_SetSRID(ST_MakePoint(long, lat), 4326));
-- INSERT INTO public.geo_people_nearby(user_id, location) VALUES(2, ST_SetSRID(ST_MakePoint(-71.060316, 48.432044), 4326));
-- INSERT INTO public.geo_people_nearby(user_id, location) VALUES(3, ST_SetSRID(ST_MakePoint(116.404177, 39.909652), 4326));

-- 查看距离此坐标对象1000米以内的所有对象与距离。
-- select
--     user_id
--     , ST_AsText(location)
--     , ST_Distance(ST_GeomFromText('POINT(116.404176 39.909652)',4326), location) from public.geo_people_nearby where ST_DWithin(location::geography, ST_GeographyFromText('POINT(116.404177 39.909652)'), 1000.0);



## 184
-- insert into public.geo_people_nearby select generate_series(1,184), st_setsrid(st_makepoint(150-random()*100, 90-random()*100), 4326);

insert into public.geo_people_nearby select id, st_setsrid(st_makepoint(150-random()*100, 90-random()*100), 4326) from public.user order by id asc limit 5000;
