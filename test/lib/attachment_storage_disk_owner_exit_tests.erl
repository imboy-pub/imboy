-module(attachment_storage_disk_owner_exit_tests).
-include_lib("eunit/include/eunit.hrl").

%% V3-20 disk 驱动退役回归锚点（原「owner 进程退出后描述符释放与删除不被
%% 撤销」语义随 attachment_storage_disk 模块退役）：
%%   * 模块不再存在（code:which/1 = undefined、无任何导出可调）；
%%   * facade fail-closed：delete_snapshot/head_snapshot 一律
%%     {error, disk_driver_retired}；
%%   * legacy 锁目录互操作：磁盘锁文件本身不再被本仓任何代码读取或写入。
owner_exit_test_() ->
    {timeout, 30, fun disk_driver_retired/0}.

disk_driver_retired() ->
    %% 模块已删：不可加载、无导出（code:which/1 对缺失模块返回 non_existing）
    non_existing = code:which(attachment_storage_disk),
    false = erlang:function_exported(attachment_storage_disk, put_snapshot, 3),
    false = erlang:function_exported(attachment_storage_disk, delete_snapshot, 1),
    %% facade 对历史 disk 身份 fail-closed
    Identity = #{
        driver => disk,
        endpoint => <<"file:///retired">>,
        container => <<"c">>,
        key => <<"exited">>
    },
    {error, disk_driver_retired} = attachment_storage:delete_snapshot(Identity),
    {error, disk_driver_retired} = attachment_storage:head_snapshot(Identity).
