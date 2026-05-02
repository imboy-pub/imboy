-module(config_ds).
%% Stub module for tests that need config_ds but don't have it loaded.
%% Must include all functions that tests may mock via meck passthrough.
-export([env/1, env/2, env/3, get/1, get/2, set/2, set/3, set/4]).
env(Key) -> env(Key, undefined).
env(Key, Default) -> env(imboy, Key, Default).
env(App, [Key], Default) ->
    env(App, Key, Default);
env(App, [Key | SubKeys], Default) ->
    case application:get_env(App, Key) of
        {ok, ConfigList} when is_list(ConfigList), is_list(SubKeys) ->
            get_nested_value(SubKeys, ConfigList, Default);
        _ ->
            Default
    end;
env(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.
get(_Key) -> undefined.
get(_Key, Default) -> Default.
set(_Key, _Value) -> ok.
set(_Key, _Value, _Opts) -> ok.
set(_Key, _Value, _Title, _Remark) -> ok.

get_nested_value([], Value, _Def) ->
    Value;
get_nested_value([Key | Rest], ConfigList, Def) when is_list(ConfigList) ->
    case lists:keyfind(Key, 1, ConfigList) of
        false -> Def;
        {_, Value} -> get_nested_value(Rest, Value, Def)
    end;
get_nested_value(_, _, Def) ->
    Def.
