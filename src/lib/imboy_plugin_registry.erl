-module(imboy_plugin_registry).

-export([all/0, get/1, plugin_names/0]).

-spec all() -> map().
all() ->
    #{
        channel => #{
            kind => plugin,
            feature_keys => [channel, channel_discover, channel_invitation, channel_order],
            requires_capabilities => [],
            depends_on_plugins => []
        },
        moment => #{
            kind => plugin,
            feature_keys => [moment],
            requires_capabilities => [],
            depends_on_plugins => []
        },
        location => #{
            kind => plugin,
            feature_keys => [location],
            requires_capabilities => [],
            depends_on_plugins => []
        },
        group_collab => #{
            kind => aggregate_plugin,
            feature_keys => [group_vote, group_schedule, group_task],
            children => [vote, schedule, task],
            requires_capabilities => [],
            depends_on_plugins => []
        }
    }.

-spec get(atom()) -> map().
get(Name) ->
    maps:get(Name, all(), #{}).

-spec plugin_names() -> [atom()].
plugin_names() ->
    [channel, moment, location, group_collab].
