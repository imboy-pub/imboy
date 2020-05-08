-module (config_repo).
%%%
% config_repo 是 config repository 缩写
%%%
-export ([get_by_key/1]).

get_by_key(Key) ->
    Sql = <<"SELECT `value` FROM `config` WHERE `key` = ?">>,
    Row = imboy_db:query(Sql, [Key]),
    % lager:info("~p", Row),
    Row.
