
## imboy_syn

```
imboy_syn:join(1, <<"ios">>, spawn(fun() -> receive _ -> ok end end), <<"did11">>).
imboy_syn:join(1, <<"andriod">>, spawn(fun() -> receive _ -> ok end end), <<"did12">>).
imboy_syn:join(1, <<"macos">>, self(), <<"3f039a2b4724a5b7">>).

imboy_syn:join(2, <<"ios">>, spawn(fun() -> receive _ -> ok end end), <<"did21">>).
imboy_syn:join(2, <<"andriod">>, spawn(fun() -> receive _ -> ok end end), <<"did22">>).
imboy_syn:join(3, <<"andriod">>, spawn(fun() -> receive _ -> ok end end), <<"did32">>).

imboy_syn:count().
imboy_syn:count_user().
imboy_syn:count_user(1).
imboy_syn:list(1).

syn:

ets:select(syn_pg_by_name_chat, [{ '$1', [], ['$1']}]).
ets:select(syn_pg_by_name_chat, [{ '$1', [], ['$1']}], 10).
```
