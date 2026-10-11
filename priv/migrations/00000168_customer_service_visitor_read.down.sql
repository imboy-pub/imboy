SET lock_timeout = '5s';
SET statement_timeout = '15min';
DROP TABLE IF EXISTS customer_service_visitor_read_cursor;
DROP TABLE IF EXISTS customer_service_visitor_message_read;
DROP INDEX IF EXISTS uq_csss_visitor_read_scope;
