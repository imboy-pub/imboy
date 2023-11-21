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
