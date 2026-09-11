#!/usr/bin/env python3
"""Call the loopback admin replay endpoint without persisting its cookie."""

import argparse
import json
import os

from agent_hub_ext01_mcp_client_smoke import Api, guard


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--delivery-id", required=True)
    parser.add_argument("--out", required=True)
    args = parser.parse_args(argv)
    base = os.environ.get("IMBOY_BASE_URL", "http://127.0.0.1:9800")
    adm_uid = os.environ.get("ADM_UID", "")
    adm_sig = os.environ.get("ADM_SIG", "")
    if os.environ.get("ADM_SIG_HEX"):
        adm_sig = bytes.fromhex(os.environ["ADM_SIG_HEX"]).decode("latin-1")
    guard(base)
    if not adm_uid or not adm_sig:
        return 2

    status, _, body = Api(base, adm_uid, adm_sig).adm(
        "POST",
        "/api/adm/bot/deliveries/replay",
        {"delivery_id": args.delivery_id},
    )
    payload = (body or {}).get("payload") or {}
    passed = (
        status == 200
        and (body or {}).get("code") == 0
        and str(payload.get("delivery_id")) == args.delivery_id
        and payload.get("status") == "pending"
    )
    with open(args.out, "w", encoding="utf-8") as handle:
        json.dump(
            {
                "delivery_id": args.delivery_id,
                "http_status": status,
                "code": (body or {}).get("code"),
                "status": payload.get("status"),
                "passed": passed,
            },
            handle,
            ensure_ascii=True,
            indent=2,
        )
        handle.write("\n")
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
