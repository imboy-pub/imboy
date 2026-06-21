%%% go-fastdfs 用户头像迁移到 Garage（一次性脚本，prod 节点运行）
%%%
%%% 背景：fastdfs 其他历史文件接受丢失，仅迁移高频的用户头像。
%%%   user.avatar 形如 "/img/..."（fastdfs 相对路径）→ 下载 → 上传 Garage public 桶
%%%   → avatar 改写为 Garage public_url。
%%%
%%% 用法（imboy eval，返回值即结果，便于无 console 捕获）：
%%%   Base = <<"http://127.0.0.1:8080">>.   %% fastdfs 节点内下载基址
%%%   migrate_fdfs_avatars:run(Base, dry).   %% {dry, N, [{Uid,Av}...]}  只读
%%%   migrate_fdfs_avatars:verify_one(Base). %% 真实迁 1 条+GET 验证可读: {verified,Uid,Av,NewUrl,GetStatus}
%%%   migrate_fdfs_avatars:run(Base, go).    %% {go_done, Ok, Fail}  迁剩余全部
%%%
%%% 安全：仅匹配 avatar LIKE '/%'；迁移前 error_logger 记原值可回滚；
%%%   已迁的(avatar 变 http)不再被匹配，verify_one + go 天然幂等不重复。

-module(migrate_fdfs_avatars).
-export([run/2, verify_one/1]).

-define(SEL, <<"SELECT id, avatar FROM public.\"user\" WHERE avatar LIKE '/%'">>).

run(_Base, dry) ->
    Rows = rows(),
    List = [{maps:get(<<"id">>, R), maps:get(<<"avatar">>, R)} || R <- Rows],
    {dry, length(List), List};
run(Base, go) ->
    Res = [catch migrate_one(Base, R) || R <- rows()],
    Ok = length([x || ok <- Res]),
    {go_done, Ok, length(Res) - Ok}.

verify_one(Base) ->
    case rows() of
        [] ->
            no_fdfs_avatar;
        [#{<<"id">> := Uid, <<"avatar">> := Av} | _] ->
            NewUrl = do_migrate(Uid, Av, Base),
            {ok, {{_, GetStatus, _}, _, _}} =
                httpc:request(get, {binary_to_list(NewUrl), []}, [], []),
            {verified, Uid, Av, NewUrl, GetStatus}
    end.

rows() ->
    {ok, R} = elib_pg:query(?SEL, []),
    R.

migrate_one(Base, #{<<"id">> := Uid, <<"avatar">> := Av}) ->
    _ = do_migrate(Uid, Av, Base),
    ok.

%% 下载 → 上传 public 桶 → 写回 avatar，返回新 public_url
do_migrate(Uid, Av, Base) ->
    DlUrl = <<Base/binary, Av/binary>>,
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(get, {binary_to_list(DlUrl), []}, [], [{body_format, binary}]),
    Name = filename:basename(Av),
    Key = elib_oss:build_object_key(Uid, <<"public">>, undefined, Name),
    ok = elib_oss:put_object(elib_oss:get_bucket(<<"public">>), Key, Body, avatar_mime(Name)),
    NewUrl = elib_oss:public_url_for_key(Key),
    error_logger:info_msg("avatar_migrate uid=~p old=~s new=~s~n", [Uid, Av, NewUrl]),
    case elib_pg:query(<<"UPDATE public.\"user\" SET avatar=$1 WHERE id=$2">>, [NewUrl, Uid]) of
        {error, R} -> throw({update_failed, Uid, R});
        _ -> NewUrl
    end.

avatar_mime(Name) ->
    case filename:extension(Name) of
        <<".png">> -> <<"image/png">>;
        <<".gif">> -> <<"image/gif">>;
        <<".webp">> -> <<"image/webp">>;
        _ -> <<"image/jpeg">>
    end.
