%%%-------------------------------------------------------------------
%%% @doc barrel_mcp shared types and macros
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BARREL_MCP_HRL).
-define(BARREL_MCP_HRL, true).

%% MCP Protocol Version (the server's preferred version, used in
%% `initialize' responses when the client requests one we don't
%% speak). The actual negotiated value for a session is whatever the
%% client asked for if it's in `?MCP_SUPPORTED_VERSIONS'.
-define(MCP_PROTOCOL_VERSION, <<"2025-11-25">>).
%% Legacy protocol version (JSON-RPC only transport)
-define(MCP_PROTOCOL_VERSION_LEGACY, <<"2024-11-05">>).
%% Latest protocol version targeted by the client; the client negotiates
%% downward when the peer reports an older revision.
-define(MCP_CLIENT_PROTOCOL_VERSION, <<"2025-11-25">>).
%% Older revisions the client knows how to speak (in preference order).
-define(MCP_CLIENT_SUPPORTED_VERSIONS, [
    <<"2025-11-25">>, <<"2025-06-18">>, <<"2025-03-26">>, <<"2024-11-05">>
]).
%% Versions the server is willing to accept on the
%% `MCP-Protocol-Version' request header. Same set as the client list
%% today; bumping the server's preferred default is a separate change.
-define(MCP_SUPPORTED_VERSIONS, [
    <<"2025-11-25">>, <<"2025-06-18">>, <<"2025-03-26">>, <<"2024-11-05">>
]).

%% JSON-RPC Error Codes
-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).

%% MCP-specific error codes
-define(MCP_TOOL_ERROR, -32000).
-define(MCP_RESOURCE_ERROR, -32001).
-define(MCP_PROMPT_ERROR, -32002).

%% Handler types
-type handler_type() ::
    tool
    | resource
    | prompt
    | resource_template
    | completion.

%% Tool definition
-type tool_def() :: #{
    module := module(),
    function := atom(),
    description => binary(),
    input_schema => map()
}.

%% Resource definition
-type resource_def() :: #{
    module := module(),
    function := atom(),
    name := binary(),
    uri := binary(),
    description => binary(),
    mime_type => binary()
}.

%% Prompt definition
-type prompt_def() :: #{
    module := module(),
    function := atom(),
    name := binary(),
    description => binary(),
    arguments => [prompt_arg()]
}.

-type prompt_arg() :: #{
    name := binary(),
    description => binary(),
    required => boolean()
}.

%% MCP Content types
-type text_content() :: #{
    % <<"text">>
    type := binary(),
    text := binary()
}.

-type image_content() :: #{
    % <<"image">>
    type := binary(),
    data := binary(),
    mimeType := binary()
}.

-type resource_content() :: #{
    % <<"resource">>
    type := binary(),
    resource := #{
        uri := binary(),
        text => binary(),
        blob => binary(),
        mimeType => binary()
    }
}.

-type mcp_content() :: text_content() | image_content() | resource_content().

%% Registry key for persistent_term
-define(REGISTRY_KEY, barrel_mcp_handlers).

%% ETS table name
-define(REGISTRY_TABLE, barrel_mcp_registry).

-endif.
