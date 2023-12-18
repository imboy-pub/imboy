-module(imboy_dtl).

% -export([template/2]).
-export([template/3]).


% imboy_dtl:template(login, [], imadm).
-spec template(atom(), list(), atom()) -> binary().
template(Name, Vars, AppName) ->
    % Path = iolist_to_binary([code:priv_dir(imadm), "/template/login.html"]),
    Path = imboy_func:implode("", [code:priv_dir(AppName), "/template/", Name, ".html"]),
    erlydtl:compile_file(binary_to_list(Path), Name),
    Name:render(Vars).
