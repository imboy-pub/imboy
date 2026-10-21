-module('talk_record_routes.fragment').
-moduledoc "Compatibility entry for the original route fragment; the production catalog is authoritative.".
-export([routes/0]).

-spec routes() -> [{string(), module(), map()}].
routes() ->
    talk_record_routes:routes().
