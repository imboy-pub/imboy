
% type depcache_server() :: pid() | atom().
-define (DEPCACHE_SERVER, imboy_cache).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).

-define(GEO_PEOPLE_NEARBY, 'geo:people_nearby').
-define(GEO_PEOPLE_NEARBY_ONLINE, 'geo:people_nearby_online').
