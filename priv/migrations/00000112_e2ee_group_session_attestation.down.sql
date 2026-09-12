-- Security attestation rows are an append-only authorization ledger. A code rollback must
-- not erase them; older binaries ignore these additive tables and a later up remains idempotent.
SELECT 1;
