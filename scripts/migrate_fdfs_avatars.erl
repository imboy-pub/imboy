%%% go-fastdfs 用户头像迁移到 Garage（一次性脚本，人工在 prod 节点运行）
%%%
%%% 背景：fastdfs 其他历史文件接受丢失，仅迁移高频的用户头像。
%%%   user.avatar 形如 "/img/..."（fastdfs 相对路径）的记录，
%%%   文件从 fastdfs 下载 → 上传 Garage → avatar 改写为 Garage URL。
%%%
%%% 用法（prod remote_console，依赖运行节点的 elib_oss/elib_pg/配置）：
%%%   _rel/imboy/bin/imboy remote_console
%%%   c("scripts/migrate_fdfs_avatars.erl").
%%%   FdfsBase = <<"http://127.0.0.1:8080">>.   %% ← 待确认：fastdfs 下载基址(看 nginx 反代 upstream)
%%%   migrate_fdfs_avatars:run(FdfsBase, dry).  %% 先 dry-run：只列将迁移的 (Uid, OldAvatar)，不改库
%%%   migrate_fdfs_avatars:run(FdfsBase, go).   %% 确认无误后实际迁移
%%%
%%% 安全：
%%%   - dry 模式零写入；go 模式逐条 try/catch，单条失败跳过不中断
%%%   - 迁移每条前 error_logger 记录 {Uid, 原 avatar, 新 avatar}，可据日志回滚
%%%   - 仅匹配 avatar LIKE '/%'（fastdfs 相对路径），不动已是 Garage(u.../http) 的头像
%%%
%%% 待确认点（运行前核对）：
%%%   1. FdfsBase：fastdfs 实际可下载基址
%%%   2. public 桶（get_bucket(<<"public">>) = imboy-public）在 prod 已建好且 public_url 可直读
%%%   3. 客户端是否识别返回的 Garage public_url 格式（完整 http URL 通常直接用）
%%% 落桶已显式走 public：put_object(get_bucket(public), u<Uid>/avatar/..., Body, Mime)

-module(migrate_fdfs_avatars).
-export([run/2]).

run(FdfsBase, Mode) ->
    Sql = <<"SELECT id, avatar FROM public.\"user\" WHERE avatar LIKE '/%'">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            io:format("待迁移头像: ~p 条 (mode=~p)~n", [length(Rows), Mode]),
            R = [migrate_one(FdfsBase, Mode, Row) || Row <- Rows],
            Ok = length([x || ok <- R]),
            io:format("完成: 成功=~p 跳过/失败=~p~n", [Ok, length(R) - Ok]),
            ok;
        {error, E} ->
            io:format("查询失败: ~p~n", [E]),
            {error, E}
    end.

migrate_one(FdfsBase, Mode, #{<<"id">> := Uid, <<"avatar">> := Av}) ->
    case Mode of
        dry ->
            io:format("  [dry] uid=~p avatar=~s~n", [Uid, Av]),
            ok;
        go ->
            try
                Url = <<FdfsBase/binary, Av/binary>>,
                {ok, {{_, 200, _}, _, Body}} =
                    httpc:request(get, {binary_to_list(Url), []}, [], [{body_format, binary}]),
                Name = filename:basename(Av),
                Mime = avatar_mime(Name),
                %% public 头像桶 + u<Uid>/avatar/<Ymd>/<hex>.<ext>，与新头像链路一致
                ObjectKey = elib_oss:build_object_key(Uid, <<"public">>, undefined, Name),
                Bucket = elib_oss:get_bucket(<<"public">>),
                ok = elib_oss:put_object(Bucket, ObjectKey, Body, Mime),
                NewUrl = elib_oss:public_url_for_key(ObjectKey),
                error_logger:info_msg("avatar_migrate uid=~p old=~s new=~s~n", [Uid, Av, NewUrl]),
                {ok, 1} = elib_pg:execute(
                    <<"UPDATE public.\"user\" SET avatar=$1 WHERE id=$2">>, [NewUrl, Uid]
                ),
                io:format("  [ok] uid=~p -> ~s~n", [Uid, NewUrl]),
                ok
            catch
                C:R ->
                    io:format("  [skip] uid=~p err=~p:~p~n", [Uid, C, R]),
                    skip
            end
    end.

avatar_mime(Name) ->
    case filename:extension(Name) of
        <<".png">> -> <<"image/png">>;
        <<".gif">> -> <<"image/gif">>;
        <<".webp">> -> <<"image/webp">>;
        _ -> <<"image/jpeg">>
    end.
