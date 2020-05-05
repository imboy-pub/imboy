

# tpl
The following rule will serve the file `static/index.html`
from the application `my_app`'s priv directory whenever the
path `/` is accessed.

``` erlang
{"/", cowboy_static, {priv_file, my_app, "static/index.html"}}
```
