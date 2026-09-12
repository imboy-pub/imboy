-- The request ledger and recipient snapshot are append-only authorization data.
-- A code rollback must not erase them; older binaries ignore these additive objects.
-- Migration 109 continues to own msg_c2g_timeline.conv_seq and its index.
SELECT 1;
