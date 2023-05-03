
数据库直接采用 postgresql 了

## How can I use mySQL replace() to replace strings in multiple records?
https://stackoverflow.com/questions/4271186/how-can-i-use-mysql-replace-to-replace-strings-in-multiple-records

```
UPDATE user_friend
SET setting = REPLACE(setting, 'isfrom', 'is_from')
WHERE setting LIKE '%isfrom%'
```
