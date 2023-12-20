-module(imboy_dtl).

% -export([template/2]).
-export([template/3]).


% imboy_dtl:template(login_dtl, [], imadm).
-spec template(atom(), list(), atom()) -> binary().
template(Name, Vars, AppName) ->
    Path = imboy_func:implode("", [code:priv_dir(AppName), "/template/", Name, ".html"]),
    erlydtl:compile(binary_to_list(Path), Name),
    Name:render(Vars).
