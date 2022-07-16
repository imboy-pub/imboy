%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010  Marc Worrell, 2014 Arjan Scherpenisse
%%
%% @doc z_depcache interface file for handing depcache functions from
%% the Zotonic context.

%% Copyright 2009-2010 Marc Worrell, 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% copy from https://github.com/zotonic/zotonic/blob/master/apps/zotonic_core/src/support/z_depcache.erl
%% 为了项目风格统一，并且不依赖 zotonic.hrl ，所以修改了module名称
%% The Module name was changed in order to maintain a uniform project style and not rely on zotonic.hrl

-module(imboy_kv).
-include_lib("imboy/include/kv.hrl").
-include_lib("imboy/include/log.hrl").

%% gen_server API
-export([start_link/1]).

%% depcache exports
-export([memo/1, memo/2, memo/3, memo/4]).

-export([set/2, set/3, set/4, get/1, get_wait/1, get/2, get_subkey/2]).
-export([flush/0, flush/1, size/0, record_depcache_event/1]).
-export([in_process_server/0, in_process_server/1, in_process/1, flush_process_dict/0]).

%% @doc Start depcache instance based on site configuration
start_link(Args) ->
    depcache:start_link(
        ?DEPCACHE_SERVER,
        [
            {memory_max, proplists:get_value(depcache_memory_max, Args)},
            {callback, {?MODULE, record_depcache_event, [Args]}}
        ]
    ).

%% @doc Cache the result of the function for an hour.
%% @param Fun a funciton for producing a value
%% @returns cached value
memo(Function) when is_function(Function, 0) ->
    depcache:memo(Function, ?DEPCACHE_SERVER).

%% @doc If Fun is a function then cache for an hour given the key. If
%%      Fun is a {M,F,A} tuple then derive the key from the tuple and
%%      cache for `MaxAge' seconds.
%%
%% @param Fun a funciton for producing a value1
%% @param MaxAge a caching time
%% @param Key a cache item key
%% @returns cached value
memo(Function, MaxAge) when is_tuple(Function) ->
    depcache:memo(Function, undefined, MaxAge, [], ?DEPCACHE_SERVER);
%% @doc Cache the result of the function as Key for `MaxAge' seconds.
%% @returns cached value
%% @equiv memo(Fun, Key, MaxAge, [], Server)
memo(F, Key) when is_function(F, 0) ->
    depcache:memo(F, Key, ?HOUR, [], ?DEPCACHE_SERVER).
memo(F, Key, MaxAge) ->
    depcache:memo(F, Key, MaxAge, [], ?DEPCACHE_SERVER).

%% @doc Cache the result of the function as Key for `MaxAge' seconds, flush
%%      the cached result if any of the dependencies is changed.
%% @returns cached value
memo(F, Key, MaxAge, Dep) ->
    depcache:memo(F, Key, MaxAge, Dep, ?DEPCACHE_SERVER).

%% @spec set(Key, Data) -> void()
%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
set(Key, Data) ->
    depcache:set(Key, Data, 3600, [], ?DEPCACHE_SERVER).

%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
set(Key, Data, MaxAge) ->
    depcache:set(Key, Data, MaxAge, [], ?DEPCACHE_SERVER).

%% @spec set(Key, Data, MaxAge, Depend) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
set(Key, Data, MaxAge, Depend) ->
    depcache:set(Key, Data, MaxAge, Depend, ?DEPCACHE_SERVER).

%% @spec get_wait(Key) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, when the key does not exist then lock the entry and let
%% the calling process insert the value. All other processes requesting the key will wait till
%% the key is updated and receive the key's new value.
get_wait(Key) ->
    depcache:get_wait(Key, ?DEPCACHE_SERVER).


%% @spec get(Key) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key) ->
    depcache:get(Key, ?DEPCACHE_SERVER).


%% @spec get_subkey(Key, SubKey) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get_subkey(Key, SubKey) ->
    depcache:get_subkey(Key, SubKey, ?DEPCACHE_SERVER).


%% @spec get(Key, SubKey) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, SubKey) ->
    depcache:get(Key, SubKey, ?DEPCACHE_SERVER).

%% @doc Flush all keys from the caches
%% <br/>
%% <b>See also:</b>
%% [http://erlang.org/doc/man/gen_server.html#call-2 gen_server:call/2].
%%
flush() ->
    depcache:flush(?DEPCACHE_SERVER).
%% @spec flush(Key) -> void()
%% @doc Flush the key and all keys depending on the key
flush(Key) ->
    depcache:flush(Key, ?DEPCACHE_SERVER).

%% @doc Return the total memory size of all stored terms
size() ->
    depcache:size(?DEPCACHE_SERVER).


in_process_server() ->
    depcache:in_process_server(?DEPCACHE_SERVER).
%% @doc Check if we use a local process dict cache
in_process_server(Server) ->
    depcache:in_process_server(Server).

%% @doc Enable or disable the in-process caching using the process dictionary
in_process(Flag) ->
    depcache:in_process(Flag).

%% @doc Flush all items memoized in the process dictionary.
flush_process_dict() ->
    depcache:flush_process_dict().

% TODO
record_depcache_event(Args) ->
    ?DEBUG(Args),
    ok.
