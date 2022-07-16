
% type depcache_server() :: pid() | atom().
-define (DEPCACHE_SERVER, depcache_imboy).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).
