-module(workspace_attachment_ds).
-export([scan/2, scan_organization/2, visible_record/2, can_govern/2]).
scan(WorkspaceId, Filters) -> workspace_attachment_repo:scan(WorkspaceId, Filters).
scan_organization(OrgId, Filters) -> workspace_attachment_repo:scan_organization(OrgId, Filters).
visible_record(Uid, RecordId) -> talk_record_repo:find_visible(Uid, RecordId).
can_govern(Uid, Org) -> workspace_attachment_repo:can_govern(Uid, Org).
