-module(attachment_storage_disk_lock_owner_death_tests).
-include_lib("eunit/include/eunit.hrl").

%% V3-20 disk 驱动退役回归锚点（原「物理锁持有至最后文件变更」锁序语义随
%% attachment_storage_disk 模块退役）：
%%   * 模块不再存在（code:which/1 = undefined、无任何导出可调）；
%%   * facade（attachment_storage）对历史 disk 身份 fail-closed：
%%     delete_snapshot/head_snapshot 一律 {error, disk_driver_retired}，
%%     历史磁盘数据保守保留（拒删拒 head，不指向已删除模块）。
lock_owner_death_test_() ->
    {timeout, 30, fun disk_driver_retired/0}.

disk_driver_retired() ->
    %% 模块已删：不可加载、无导出（code:which/1 对缺失模块返回 non_existing）
    non_existing = code:which(attachment_storage_disk),
    false = erlang:function_exported(attachment_storage_disk, put_snapshot, 3),
    %% facade 对历史 disk 身份 fail-closed
    Identity = #{
        driver => disk,
        endpoint => <<"file:///retired">>,
        container => <<"c">>,
        key => <<"owned">>
    },
    {error, disk_driver_retired} = attachment_storage:delete_snapshot(Identity),
    {error, disk_driver_retired} = attachment_storage:head_snapshot(Identity).
