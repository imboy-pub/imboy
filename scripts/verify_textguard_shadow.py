#!/usr/bin/env python3
"""Run the shadow suite and affected business regressions against isolated PG and Rust."""
import verify_textguard_business as business


if __name__ == "__main__":
    business.TASK = "T21"
    business.SUITE = "textguard_shadow_tests"
    business.ACCEPTANCE = ["AC-084", "AC-085", "AC-086"]
    business.BUSINESS_SOURCES += ["test/lib/textguard_shadow_tests.erl"]
    business.CONSUMER_SOURCES += ["test/lib/textguard_business_tests.erl", "test/lib/textguard_export_tests.erl"]
    business.STANDALONE_CONSUMERS = {"textguard_business_tests", "textguard_export_tests"}
    raise SystemExit(business.main())
