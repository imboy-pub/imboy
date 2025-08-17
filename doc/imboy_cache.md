## imboy_cache.erl

copy from https://github.com/zotonic/zotonic/blob/master/apps/zotonic_core/src/support/z_depcache.erl

* 为了项目风格统一，并且不依赖 zotonic.hrl ，所以修改了module名称
* The Module name was changed in order to maintain a uniform project style and not rely on zotonic.hrl

```
imboy_cache:set(a, 1).
imboy_cache:get(a).
imboy_cache:memo(fun() ->
    a
end).
```

* macOS 禁用防火墙临时测试
```
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate off
```

## 分布式缓存功能

imboy_cache 模块现在集成了分布式缓存功能，可以通过配置启用。当启用时，缓存的变更会自动同步到集群中的其他节点。

要启用分布式缓存功能，请在 sys.config 中添加以下配置：

```
{imboy, [
    {dsync_enabled, true}
]}
```

启用后，所有的 set 和 flush 操作都会自动广播到集群中的其他节点，确保缓存数据的一致性。
