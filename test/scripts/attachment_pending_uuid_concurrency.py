"""Two real PG connections; requires an explicitly named run-owned scratch DB."""
import argparse
import json
import subprocess
import time
import uuid


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--container", required=True)
    parser.add_argument("--database", required=True)
    args = parser.parse_args()
    if not args.database.startswith("attv5_chain_") or not args.database.replace("_", "").isalnum():
        parser.error("only newly created attv5_chain_ scratch databases are permitted")
    command = ["docker", "exec", "-i", args.container, "psql", "-X", "-At",
               "-v", "ON_ERROR_STOP=1", "-U", "imboy_user", "-d", args.database]

    def query(sql):
        return subprocess.check_output(command + ["-c", sql], text=True, timeout=15).strip()

    assert query("SELECT current_database()") == args.database
    cid = str(uuid.uuid4())
    key = "v5-concurrency/" + cid
    name = "attv5_uuid_" + cid.replace("-", "")
    snapshot = json.dumps({"subject": {"kind": "user", "principal": "99191"},
                           "declaration": {"scope": "private", "scope_ref": None}})

    def register(proposed_key):
        return ("SELECT out_object_key FROM fn_v5_register_pending('" + proposed_key +
                "','fixture','private',99191,'" + cid + "','" + snapshot +
                "',now()+interval '1 day',now()+interval '5 minutes',NULL,NULL,NULL,NULL);")

    first = subprocess.Popen(command + ["-c", "SET application_name='" + name +
        "'; BEGIN; " + register(key) + " SELECT pg_sleep(8); COMMIT;"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    second = None
    try:
        deadline = time.monotonic() + 10
        while query("SELECT count(*) FROM pg_stat_activity WHERE application_name='" +
                     name + "' AND wait_event='PgSleep'") != "1":
            if first.poll() is not None or time.monotonic() >= deadline:
                raise AssertionError("first connection did not reach the measured hold point")
            time.sleep(0.05)
        second_name = name + "_second"
        second = subprocess.Popen(command + ["-c", "SET application_name='" + second_name +
            "'; " + register(key + "/different-random-key")],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        deadline = time.monotonic() + 5
        waiting = ("SELECT count(*) FROM pg_stat_activity a, pg_stat_activity b WHERE "
                   "a.application_name='" + name + "' AND a.wait_event='PgSleep' "
                   "AND b.application_name='" + second_name +
                   "' AND b.wait_event_type='Lock' AND b.wait_event='advisory'")
        while query(waiting) != "1":
            if first.poll() is not None or second.poll() is not None or time.monotonic() >= deadline:
                raise AssertionError("second connection never observed waiting on UUID lock")
            time.sleep(0.05)
        out, err = first.communicate(timeout=10)
        assert first.returncode == 0, err
        assert key in out.splitlines(), out
        second_out, second_err = second.communicate(timeout=10)
        assert second.returncode == 0, second_err
        assert second_out.splitlines() == ["SET", key], second_out
        assert query("SELECT count(*) FROM attach_pending WHERE client_upload_id='" + cid + "'") == "1"
        assert query("SELECT count(*) FROM attach_pending WHERE object_key='" + key +
                     "/different-random-key'") == "0"
        print(json.dumps({"result": "PASS", "database": args.database, "client_upload_id": cid,
                          "assertions": ["measured concurrent first registration", "original key returned",
                                         "one session row", "proposed second key not reserved"]}))
    finally:
        for process in (second, first):
            if process is not None and process.poll() is None:
                process.terminate()
                process.communicate(timeout=10)


if __name__ == "__main__":
    main()
