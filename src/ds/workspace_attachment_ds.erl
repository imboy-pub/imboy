-module(workspace_attachment_ds).
-export([scan/2]).
scan(WorkspaceId, Filters) -> workspace_attachment_repo:scan(WorkspaceId, Filters).
