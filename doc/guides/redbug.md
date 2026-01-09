
https://github.com/massemanet/redbug

https://www.cloudby.me/2022/05/19/使用-redbug-跟踪-erlang/

```
4> redbug("your_module:your_private_function->return").
{156,1}
5> your_module:your_public_function().
% 19:59:13 <0.1.0>({erlang,apply,4})
% your_module:your_private_function(some, parameters)

% 19:59:13 <0.1.0>({erlang,apply,4})
% your_module:your_private_function/2 -> ok
ok
```
