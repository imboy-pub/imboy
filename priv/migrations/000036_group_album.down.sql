DROP TABLE IF EXISTS public."group_album" CASCADE;
DROP TABLE IF EXISTS public."group_album_photo" CASCADE;
DROP TABLE IF EXISTS public."group_album_photo_like" CASCADE;
DROP TABLE IF EXISTS public."group_album_photo_comment" CASCADE;
DROP INDEX IF EXISTS idx_group_album_group;
DROP INDEX IF EXISTS idx_group_album_photo_album;
DROP INDEX IF EXISTS idx_group_album_photo_group;
DROP INDEX IF EXISTS idx_group_album_photo_like_photo;
DROP INDEX IF EXISTS idx_group_album_photo_comment_photo;
