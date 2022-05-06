
```
cd ./test
erl -config .config

net_adm:ping('imboy@127.0.0.1').


Sql = <<"SELECT id FROM `user` WHERE email is null limit 200000">>,
 {ok, _, IDs} = mysql_pool:query(Sql, []).

[(fun(ID) -> Account = integer_to_list(account_server:allocate()),
    Email = Account ++ "@imboy.pub",
    Sql2 = <<"UPDATE `user` SET `email` = ?, `account` = ? WHERE `id` = ?">>,
    mysql_pool:query(Sql2, [Email, Account, ID]) end)(ID) || [ID] <- IDs, ID > 0].


```
