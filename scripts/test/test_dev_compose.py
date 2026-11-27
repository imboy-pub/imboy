"""Local Compose contracts and isolated setup orchestration; no third-party Python deps."""

import hashlib
import hmac
import http.client
import http.server
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import unittest
from datetime import datetime, timezone


ROOT = Path(__file__).resolve().parents[2]
COMPOSE = ([os.environ["COMPOSE_BIN"]] if os.environ.get("COMPOSE_BIN")
           else [shutil.which("docker") or "docker", "compose"])
FIXTURE_VALUES = {
    "IMBOY_PG_PASSWORD": "ci_only_pg_password",
    "IMBOY_PG_PRO_PASSWORD": "ci_only_pro_password",
    "GARAGE_RPC_SECRET": "01" * 32,
    "IMBOY_GARAGE_ACCESS_KEY": "GK" + "02" * 16,
    "IMBOY_GARAGE_SECRET_KEY": "03" * 32,
    "IMBOY_EDGE_INTERNAL_TOKEN": "04" * 32,
    "LIVEKIT_API_SECRET": "05" * 32,
}


def environment(**overrides):
    env = os.environ.copy()
    names = re.findall(r"\$\{([A-Za-z_][A-Za-z0-9_]*)", (ROOT / "docker-compose.yml").read_text())
    for key in set(names) | {k for k in env if k.startswith(("IMBOY_", "COMPOSE_"))}:
        env.pop(key, None)
    env.update(overrides)
    return env


def run(args, **kwargs):
    return subprocess.run(args, text=True, capture_output=True, timeout=30, **kwargs)


def fixture(root, with_env=True, values=None):
    for name in ("scripts/dev_setup.sh", "docker-compose.yml", ".env.example",
                 "config/sys.config.example", "deploy/nginx/dev.conf.template",
                 "deploy/nginx/19-local-host-upstream.envsh"):
        target = root / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(ROOT / name, target)
    if with_env:
        supplied = FIXTURE_VALUES | (values or {})
        # Single quotes make dollars, hashes and spaces literal Compose data.
        text = (root / ".env.example").read_text()
        text += "\n" + "\n".join(f"{key}='{value}'" for key, value in supplied.items()) + "\n"
        (root / ".env").write_text(text)


# Only config is delegated to real Compose. All lifecycle operations below stay
# in a temporary fixture; command order and failure propagation are observable.
MOCK_DOCKER = r'''#!/usr/bin/env python3
import json, os, pathlib, subprocess, sys
args=sys.argv[1:]
root=pathlib.Path(os.environ['MOCK_ROOT'])
with (root/'calls.jsonl').open('a') as stream:
    stream.write(json.dumps({'args':args,'profiles':os.environ.get('COMPOSE_PROFILES'),
                            'upstream':os.environ.get('API_UPSTREAM')})+'\n')
def done(code=0): sys.exit(code)
if args[0]=='info': done()
if args[0]=='network': done(1 if args[1]=='inspect' else 0)
if args[0]=='inspect':
    model=json.loads((root/'model.json').read_text())
    print('\n'.join(k+'='+str(v).replace('$$','$') for k,v in model['services']['garage']['environment'].items()))
    done()
if args[:2]==['compose','version']:
    print(os.environ.get('MOCK_VERSION','2.23.1')); done()
if args[0]!='compose': done(97)
pos=next(i for i,a in enumerate(args) if a in ['config','up','ps','exec','build'])
prefix=args[1:pos]; action=args[pos]; tail=args[pos+1:]
real=json.loads(os.environ['REAL_COMPOSE'])
if action=='config': done(subprocess.run(real+prefix+['config']+tail).returncode)
if action=='up':
    phase='infra' if 'imboy_pg18' in tail else 'selected'
    if os.environ.get('MOCK_FAIL')==phase: done(42)
    model=subprocess.check_output(real+prefix+['config','--format','json'])
    (root/'model.json').write_bytes(model); done()
if action=='ps':
    print('fixture-garage' if '-q' in tail else 'fixture services healthy'); done()
if action=='build': done()
if action=='exec':
    if 'garage' in tail:
        if 'info' in tail: done(0 if os.environ.get('MOCK_BUCKET_EXISTS') else 1)
        if '--allow' in tail and os.environ.get('MOCK_FAIL')=='bucket': done(43)
    elif os.environ.get('MOCK_FAIL')=='probe': done(44)
    done()
done(98)
'''


class LocalComposeTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="imboy-compose-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        fixture(self.root)

    def model(self, *profiles, **overrides):
        args = COMPOSE + ["--project-directory", str(self.root), "--env-file", str(self.root / ".env"),
                          "-f", str(self.root / "docker-compose.yml")]
        for profile in profiles:
            args += ["--profile", profile]
        result = run(args + ["config", "--format", "json"], env=environment(**overrides))
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def setup_script(self, *args, **overrides):
        bindir = self.root / "bin"
        bindir.mkdir(exist_ok=True)
        docker = bindir / "docker"
        docker.write_text(MOCK_DOCKER)
        docker.chmod(0o755)
        (bindir / "sleep").write_text("#!/bin/sh\nexit 0\n")
        (bindir / "sleep").chmod(0o755)
        env = environment(PATH=str(bindir) + os.pathsep + os.environ["PATH"],
                          MOCK_ROOT=str(self.root), REAL_COMPOSE=json.dumps(COMPOSE), **overrides)
        result = run(["bash", str(self.root / "scripts/dev_setup.sh"), *args], env=env)
        log = self.root / "calls.jsonl"
        calls = [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []
        return result, calls

    def test_profile_selection(self):
        cases = {
            (): set(),
            ("dev",): {"imboy_pg18", "garage"},
            ("pro",): {"prod_imboy_pg18"},
            ("dev", "pro"): {"imboy_pg18", "garage", "prod_imboy_pg18"},
            ("dev", "gateway", "api", "admin"): {"imboy_pg18", "garage", "imboy_nginx", "imboy_backend", "imboy_edge", "imboy_admin"},
        }
        for profiles, expected in cases.items():
            with self.subTest(profiles=profiles):
                self.assertEqual(set(self.model(*profiles).get("services", {})), expected)
        all_services = self.model("dev", "gateway", "api", "admin", "livekit", "widget")["services"]
        self.assertEqual(set(all_services), cases[("dev", "gateway", "api", "admin")] | {"imboy_livekit", "imboy_widget"})
        # Both services inherit the backend image's healthcheck; disabling it on
        # edge leaves Compose --wait without a usable probe after image merging.
        for name in ("imboy_backend", "imboy_edge"):
            health = all_services[name]["healthcheck"]
            self.assertFalse(health.get("disable", False))
            self.assertIn(health["test"][0], ("CMD", "CMD-SHELL"))

    def test_test_profile_is_project_scoped(self):
        model = self.model("test")
        services = model["services"]
        self.assertEqual(set(services), {"test_pg18", "test_garage", "test_backend", "test_edge"})
        for service in services.values():
            self.assertNotIn("container_name", service)
            self.assertEqual(set(service["networks"]), {"test"})
            for port in service.get("ports", []):
                self.assertEqual(port["host_ip"], "127.0.0.1")
            for mount in service.get("volumes", []):
                self.assertTrue(mount["source"].startswith("test_"))
        self.assertEqual(services["test_backend"]["environment"]["IMBOYENV"], "test")
        self.assertEqual(set(self.model("test", "test-rtc")["services"]),
                         set(services) | {"test_livekit"})
        self.assertEqual(set(self.model("test", "test-monitoring")["services"]),
                         set(services) | {"test_prometheus"})
        traced = self.model("test", "test-uptrace")
        self.assertEqual(set(traced["services"]), set(services) | {
            "test_uptrace_pg_init", "test_uptrace_clickhouse", "test_uptrace_redis",
            "test_uptrace", "test_uptrace_otelcol",
        })
        self.assertTrue(traced["networks"]["test_uptrace_private"]["internal"])
        self.assertNotIn("pro", traced["networks"])
        self.assertEqual(traced["services"]["test_uptrace_pg_init"]["environment"]["PGHOST"], "test_pg18")


    def test_production_profiles_keep_existing_data_and_dns(self):
        model = self.model("prod", "monitoring", "enterprise-direct", DATA_DIR="./data")
        services = model["services"]
        self.assertEqual(
            set(services),
            {"prod_garage"} | {
                "prod_imboy_" + name for name in (
                    "pg18", "backend", "edge", "admin", "widget", "nginx", "certbot",
                    "enterprise_proof", "prometheus", "grafana", "pushgateway",
                    "node_exporter", "postgres_exporter",
                )
            },
        )
        pg = services["prod_imboy_pg18"]
        self.assertIn("imboy_pg18", pg["networks"]["pro"]["aliases"])
        self.assertEqual(next(v["source"] for v in pg["volumes"]
                              if v["target"] == "/var/lib/postgresql"),
                         str(self.root / "deploy/data/pg18"))
        self.assertEqual(model["networks"]["pro"]["name"], "imboy-network")
        self.assertEqual(model["volumes"]["grafana_data"]["name"], "imboy_grafana_data")
        prometheus_port = services["prod_imboy_prometheus"]["ports"][0]
        self.assertEqual((prometheus_port["host_ip"], prometheus_port["published"]),
                         ("127.0.0.1", "9090"))
        custom = self.model("prod", IMBOY_PROD_DATA_DIR="/data/imboy-production")
        pg = custom["services"]["prod_imboy_pg18"]
        self.assertEqual(next(v["source"] for v in pg["volumes"]
                              if v["target"] == "/var/lib/postgresql"),
                         "/data/imboy-production/pg18")

    def test_production_profiles_parse_without_development_passwords(self):
        for profile in ("prod", "prod-turn", "prod-sni"):
            with self.subTest(profile=profile):
                model = self.model(profile, IMBOY_PG_PASSWORD="", IMBOY_PG_PRO_PASSWORD="",
                                   POSTGRES_PASSWORD="separate-production-password")
                self.assertEqual(model["services"]["prod_imboy_pg18"]["environment"]["POSTGRES_PASSWORD"],
                                 "separate-production-password")
                self.assertNotIn("imboy_pg18", model["services"])

    def test_production_gateways_wait_for_a_working_edge_healthcheck(self):
        dev_edge = self.model("dev", "api")["services"]["imboy_edge"]
        for profile, gateway in (("prod", "prod_imboy_nginx"),
                                 ("prod-turn", "prod_imboy_nginx_turn"),
                                 ("prod-sni", "prod_imboy_nginx_sni")):
            with self.subTest(profile=profile):
                services = self.model(profile)["services"]
                self.assertEqual(services[gateway]["depends_on"]["prod_imboy_edge"]["condition"],
                                 "service_healthy")
                edge = services["prod_imboy_edge"]
                self.assertEqual(edge["healthcheck"], dev_edge["healthcheck"])
                self.assertIn({"source": "edge_health_vm_args", "target": "/run/edge-health.vm.args"},
                              edge["configs"])

    def test_turn_profile_selects_one_livekit_and_nginx(self):
        self.assertNotIn("prod_imboy_livekit", self.model("prod")["services"])
        standard = self.model("prod", "rtc")["services"]
        self.assertNotIn("prod_imboy_livekit_turn", standard)
        self.assertNotIn("5349", {port["published"] for port in standard["prod_imboy_livekit"]["ports"]})
        services = self.model("prod", "prod-turn", TURN_DOMAIN="turn.test.invalid",
                              IMBOY_PROD_DEFAULT_GATEWAY_PROFILE="prod-default-gateway")["services"]
        legacy = self.model("prod-turn", TURN_DOMAIN="turn.test.invalid")["services"]
        self.assertEqual(set(services), set(legacy))
        self.assertNotIn("prod_imboy_livekit", services)
        self.assertNotIn("prod_imboy_nginx", services)
        self.assertIn("prod_imboy_livekit_turn", services["prod_imboy_nginx_turn"]["depends_on"])
        turn = services["prod_imboy_livekit_turn"]
        self.assertIn("5349", {port["published"] for port in turn["ports"]})
        self.assertIn("/etc/letsencrypt/live/turn.test.invalid/fullchain.pem",
                      turn["environment"]["LIVEKIT_CONFIG"])
        self.assertTrue(all(volume["read_only"] for volume in turn["volumes"]))

    def test_business_extensions_merge_with_private_baseline(self):
        with tempfile.TemporaryDirectory(prefix="imboy-business-compose-") as path:
            directory = Path(path)
            baseline = directory / "private.yml"
            baseline.write_text(
                "services:\n  imboy_backend:\n    image: fixture/backend:test\n"
                "  imboy_nginx:\n    image: fixture/nginx:test\n"
            )
            files = ["-f", str(baseline)]
            for extension in ("policy", "widget"):
                output = subprocess.check_output(
                    ["bash", str(ROOT / "deploy/render-compose-extension.sh"),
                     "imboy-business-" + extension],
                    text=True,
                )
                rendered = directory / (extension + ".yml")
                rendered.write_text(output)
                files += ["-f", str(rendered)]
            model = json.loads(subprocess.check_output(
                [*COMPOSE, "--env-file", str(ROOT / "deploy/.env.example"),
                 *files, "config", "--format", "json"],
                text=True,
            ))
            services = model["services"]
            self.assertEqual(services["imboy_backend"]["image"], "fixture/backend:test")
            self.assertEqual(
                services["imboy_backend"]["environment"]["IMBOY_E2EE_MODE"], "required"
            )
            self.assertIn("imboy_widget", services["imboy_nginx"]["depends_on"])
            self.assertIn("CS_WIDGET_DOMAIN", services["imboy_nginx"]["environment"])

    def test_uptrace_redis_is_optional_and_private(self):
        ordinary = self.model("prod")
        self.assertFalse(any(name.startswith("uptrace") for name in ordinary["services"]))
        trace = self.model("prod-sni", "uptrace", UPTRACE_ENABLED="true")
        services = trace["services"]
        self.assertTrue(trace["networks"]["uptrace_private"]["internal"])
        self.assertEqual(set(services["uptrace_redis"]["networks"]), {"uptrace_private"})
        self.assertFalse(services["uptrace_redis"].get("ports"))
        self.assertNotIn("uptrace_private", services["prod_imboy_backend"]["networks"])
        self.assertEqual(
            services["prod_imboy_nginx_sni"]["environment"]["UPTRACE_ENABLED"], "true"
        )
        self.assertIn("prod_imboy_pg18", services["uptrace_pg_init"]["depends_on"])

    def test_sni_profile_owns_https_and_trusts_only_its_proxy(self):
        model = self.model("prod", "prod-sni", TURN_DOMAIN="turn.test.invalid",
                           IMBOY_PROD_DEFAULT_GATEWAY_PROFILE="prod-default-gateway")
        services = model["services"]
        nginx = services["prod_imboy_nginx_sni"]
        proxy = services["prod_imboy_haproxy"]
        self.assertEqual([port["published"] for port in proxy["ports"]], ["443"])
        self.assertEqual([port["published"] for port in nginx["ports"]], ["80"])
        self.assertNotIn("prod_imboy_nginx_turn", services)
        self.assertNotIn("prod_imboy_nginx", services)
        self.assertNotIn("prod_imboy_livekit", services)
        self.assertNotIn("prod_imboy_livekit_turn", services)
        self.assertEqual(nginx["command"][-1], "proxy-protocol")
        self.assertIn("listen 443 ssl proxy_protocol;",
                      (ROOT / "deploy/nginx/start-production.sh").read_text())
        proxy_ip = proxy["networks"]["sni_proxy"]["ipv4_address"]
        nginx_ip = nginx["networks"]["sni_proxy"]["ipv4_address"]
        realip = model["configs"]["prod_sni_nginx_realip"]["content"]
        haproxy = model["configs"]["prod_haproxy_config"]["content"]
        self.assertIn(f"set_real_ip_from {proxy_ip};", realip)
        self.assertIn(f"server nginx {nginx_ip}:443 send-proxy-v2", haproxy)
        self.assertIn("req.ssl_sni -i turn.test.invalid", haproxy)
        livekit = services["prod_imboy_livekit_sni"]
        livekit_ip = livekit["networks"]["sni_proxy"]["ipv4_address"]
        config = livekit["environment"]["LIVEKIT_CONFIG"]
        self.assertIn("tls_port: 443", config)
        self.assertIn(f"{proxy_ip}/32", config)
        self.assertIn(f"server livekit {livekit_ip}:443 send-proxy-v2", haproxy)

    def test_production_full_profiles_exclude_uptrace(self):
        extras = ("monitoring", "enterprise-direct")
        for mode, count in (("prod", 14), ("prod-turn", 15), ("prod-sni", 16)):
            with self.subTest(mode=mode):
                profiles = ("prod", *extras) if mode == "prod" else ("prod", mode, *extras)
                disabled = "prod" if mode == "prod" else "prod-default-gateway"
                services = self.model(*profiles, TURN_DOMAIN="turn.test.invalid",
                                      IMBOY_PROD_DEFAULT_GATEWAY_PROFILE=disabled)["services"]
                self.assertEqual(len(services), count)
                self.assertFalse(any("uptrace" in name for name in services))
                self.assertFalse(any(name.startswith("test_") for name in services))
                self.assertEqual(sum(name.startswith("prod_imboy_nginx") for name in services), 1)
                owners = [name for name, service in services.items()
                          if any(port["published"] == "443" and port["protocol"] == "tcp"
                                 for port in service.get("ports", []))]
                expected = {"prod": "nginx", "prod-turn": "nginx_turn", "prod-sni": "haproxy"}[mode]
                self.assertEqual(owners, ["prod_imboy_" + expected])

    def test_existing_database_mounts_and_runtime_keys(self):
        model = self.model("dev", "pro", "api", IMBOY_PROD_PG_DATA_DIR="/data/docker/pro_imboy_pg18")
        services = model["services"]
        dev = next(v for v in services["imboy_pg18"]["volumes"] if v["target"] == "/var/lib/postgresql")
        self.assertEqual((dev["type"], model["volumes"][dev["source"]]["name"]), ("volume", "imboy_pg18_data"))
        pro = next(v for v in services["prod_imboy_pg18"]["volumes"] if v["target"] == "/var/lib/postgresql")
        self.assertEqual((pro["type"], pro["source"]), ("bind", "/data/docker/pro_imboy_pg18"))
        self.assertEqual(model["networks"]["pro"]["name"], "imboy-network")
        self.assertTrue(model["networks"]["pro"]["external"])
        backend = services["imboy_backend"]
        self.assertEqual(backend["environment"]["IMBOY_CLUSTER_NODES"], "[]")
        self.assertTrue(any(v["type"] == "volume" and v["target"] == "/opt/imboy/priv_runtime" for v in backend["volumes"]))
        self.assertEqual(backend["environment"]["IMBOY_LOGIN_RSA_PRIV_KEY_FILE"], "")

    def test_pro_only_needs_no_local_service_keys(self):
        (self.root / ".env").write_text("\n".join(
            f"{key}={FIXTURE_VALUES[key]}" for key in ("IMBOY_PG_PASSWORD", "IMBOY_PG_PRO_PASSWORD")) + "\n")
        model = self.model("pro")
        self.assertEqual(set(model["services"]), {"prod_imboy_pg18"})
        self.assertEqual(model["services"]["prod_imboy_pg18"]["environment"]["POSTGRES_PASSWORD"],
                         FIXTURE_VALUES["IMBOY_PG_PRO_PASSWORD"])

    def test_host_upstream_hook_resolves_hosts_and_preserves_service_dns(self):
        nginx = self.model("dev", "gateway")["services"]["imboy_nginx"]
        mount = next(v for v in nginx["volumes"]
                     if v["target"] == "/docker-entrypoint.d/19-local-host-upstream.envsh")
        hook = Path(mount["source"])
        self.assertTrue(mount["read_only"] and os.access(hook, os.X_OK))
        self.assertIn("host.docker.internal", str(nginx["extra_hosts"]))
        self.assertIn("host-gateway", str(nginx["extra_hosts"]))
        hosts = self.root / "hosts"
        bindir = self.root / "host-bin"
        bindir.mkdir()
        # Substitute only the hosts-file input; execute the hook's real awk parser.
        awk = bindir / "awk"
        awk.write_text("#!/usr/bin/env python3\nimport os, sys\n"
                       f"tool={shutil.which('awk')!r}\n"
                       "os.execv(tool, [tool] + [os.environ['HOSTS_FIXTURE'] if a == '/etc/hosts' else a for a in sys.argv[1:]])\n")
        awk.chmod(0o755)
        cases = [
            ("127.0.0.1 localhost # host.docker.internal\n172.17.0.1 alias host.docker.internal\n",
             "host.docker.internal:9801", "imboy_edge:13990", ["172.17.0.1:9801", "imboy_edge:13990"]),
            ("fd00::1 host.docker.internal\n", "host.docker.internal:9800", "host.docker.internal:13991",
             ["[fd00::1]:9800", "[fd00::1]:13991"]),
            ("", "imboy_backend:9800", "imboy_edge:13990", ["imboy_backend:9800", "imboy_edge:13990"]),
        ]
        for contents, api, edge, expected in cases:
            with self.subTest(api=api, hosts=contents):
                hosts.write_text(contents)
                result = run(["sh", "-ec", '. "$1"; printf "%s\\n%s\\n" "$API_UPSTREAM" "$EDGE_UPSTREAM"',
                              "sh", str(hook)],
                             env=environment(PATH=str(bindir) + os.pathsep + os.environ["PATH"],
                                             HOSTS_FIXTURE=str(hosts), API_UPSTREAM=api, EDGE_UPSTREAM=edge))
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.splitlines(), expected)
        hosts.write_text("127.0.0.1 localhost\n")
        result = run(["sh", "-ec", '. "$1"', "sh", str(hook)],
                     env=environment(PATH=str(bindir) + os.pathsep + os.environ["PATH"],
                                     HOSTS_FIXTURE=str(hosts), API_UPSTREAM="host.docker.internal:9800"))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Missing host.docker.internal mapping", result.stderr)

    def test_compose_preserves_literal_values_and_shell_precedence(self):
        marker = self.root / "must-not-exist"
        literal = f'$(touch {marker}) dollar$ # space = back\\slash "quotes"'
        fixture(self.root, values={"IMBOY_PG_PASSWORD": literal})
        model = self.model("dev", "api")
        # Compose config escapes dollars so its output can be used as input again.
        self.assertEqual(model["services"]["imboy_pg18"]["environment"]["POSTGRES_PASSWORD"].replace("$$", "$"), literal)
        self.assertEqual(model["services"]["imboy_backend"]["environment"]["IMBOY_PG_PASSWORD"].replace("$$", "$"), literal)
        override = "exported $ # = \\ password"
        model = self.model("dev", "api", IMBOY_PG_PASSWORD=override)
        self.assertEqual(model["services"]["imboy_backend"]["environment"]["IMBOY_PG_PASSWORD"].replace("$$", "$"), override)
        self.assertFalse(marker.exists())

    def test_first_run_only_copies_private_env(self):
        (self.root / ".env").unlink()
        result, calls = self.setup_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.root / ".env").read_bytes(), (self.root / ".env.example").read_bytes())
        self.assertEqual((self.root / ".env").stat().st_mode & 0o777, 0o600)
        self.assertFalse(any("up" in c["args"] or "build" in c["args"] for c in calls))

    def test_default_order_preserves_env_and_uses_resolved_container_key(self):
        before = (self.root / ".env").read_bytes()
        result, calls = self.setup_script(COMPOSE_PROFILES="pro,livekit", IMBOY_GARAGE_ACCESS_KEY="shell_override_key")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual((self.root / ".env").read_bytes(), before)
        self.assertIn("Selected services are ready", result.stdout)
        commands = [c["args"] for c in calls]
        starts = [i for i, a in enumerate(commands) if "up" in a]
        self.assertEqual(len(starts), 2)
        grant = next(i for i, a in enumerate(commands) if "--key" in a)
        public = next(i for i, a in enumerate(commands) if "website" in a and "--allow" in a)
        private = next(i for i, a in enumerate(commands) if "website" in a and "--deny" in a)
        self.assertLess(starts[0], grant)
        self.assertLess(private, public)
        self.assertLess(public, starts[1])
        self.assertEqual(commands[grant][-1], "shell_override_key")
        self.assertTrue(all(c["profiles"] == "" for c in calls if c["args"][0] == "compose" and "up" in c["args"]))
        self.assertNotIn("pro", commands[starts[1]])
        self.assertTrue(all("--wait" in commands[i] for i in starts))

    def test_no_build_and_infra_host_config(self):
        for options in [("--no-build",), ("--infra-only", "--no-build")]:
            with self.subTest(options=options):
                result, calls = self.setup_script(*options)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertFalse(any("build" in c["args"] for c in calls))
        self.assertEqual((self.root / "config/sys.local.config").read_bytes(), (self.root / "config/sys.config.example").read_bytes())
        self.assertTrue(any(c["upstream"] == "host.docker.internal:9800" for c in calls))

    def test_failed_health_or_bucket_never_reports_ready(self):
        for failure in ("infra", "selected", "bucket", "probe"):
            with self.subTest(failure=failure):
                log = self.root / "calls.jsonl"
                if log.exists():
                    log.unlink()
                result, calls = self.setup_script("--no-build", MOCK_FAIL=failure)
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("Selected services are ready", result.stdout)
                if failure in ("infra", "bucket"):
                    self.assertEqual(sum("up" in c["args"] for c in calls), 1)

    def test_missing_password_stops_before_start(self):
        fixture(self.root, values={"IMBOY_PG_PASSWORD": ""})
        result, calls = self.setup_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(any("up" in c["args"] for c in calls))

    def test_local_keys_resolving_empty_stop_before_start(self):
        for key in ("GARAGE_RPC_SECRET", "IMBOY_GARAGE_ACCESS_KEY", "IMBOY_GARAGE_SECRET_KEY",
                    "IMBOY_EDGE_INTERNAL_TOKEN", "LIVEKIT_API_SECRET"):
            with self.subTest(key=key):
                fixture(self.root)
                with (self.root / ".env").open("a") as stream:
                    stream.write(f"\n{key}=${{UNSET_LOCAL_KEY}}\n")
                result, calls = self.setup_script("--infra-only", UNSET_LOCAL_KEY="")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("Local internal keys resolve empty", result.stderr)
                self.assertFalse(any("up" in c["args"] for c in calls))

    def test_livekit_key_generation_preserves_host_alias_and_existing_values(self):
        for mode in ("empty", "absent", "supplied"):
            with self.subTest(mode=mode):
                text = (ROOT / ".env.example").read_text()
                if mode == "absent":
                    text = re.sub(r"^LIVEKIT_API_SECRET=.*\n", "", text, flags=re.MULTILINE)
                elif mode == "supplied":
                    text = re.sub(r"^LIVEKIT_API_SECRET=.*$", "LIVEKIT_API_SECRET=" + FIXTURE_VALUES["LIVEKIT_API_SECRET"],
                                  text, flags=re.MULTILINE)
                text += "\n" + "\n".join(f"{key}='{value}'" for key, value in FIXTURE_VALUES.items()
                                         if key != "LIVEKIT_API_SECRET") + "\n"
                env_file = self.root / ".env"
                env_file.write_text(text)
                result, _ = self.setup_script("--no-build")
                self.assertEqual(result.returncode, 0, result.stderr)
                sourced = run(["bash", "-ec", '. "$1"; printf "%s\\n%s\\n" "$LIVEKIT_API_SECRET" "$IMBOY_LIVEKIT_API_SECRET"',
                               "bash", str(env_file)], env=environment())
                self.assertEqual(sourced.returncode, 0, sourced.stderr)
                secret, alias = sourced.stdout.splitlines()
                self.assertRegex(secret, r"^[0-9a-f]{64}$")
                self.assertEqual(secret, alias)
                if mode == "supplied":
                    self.assertEqual(secret, FIXTURE_VALUES["LIVEKIT_API_SECRET"])
                before = env_file.read_bytes()
                result, _ = self.setup_script("--no-build")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(env_file.read_bytes(), before)


def runtime_smoke(image):
    """Use the workflow's already-built backend; never publish or touch existing data."""
    if os.environ.get("GITHUB_ACTIONS") != "true":
        raise RuntimeError("--docker requires an isolated GitHub Actions runner")
    if (ROOT / ".env").exists():
        raise RuntimeError("Refusing to replace an existing .env")
    for kind, name in (("container", "imboy_pg18"), ("volume", "imboy_pg18_data"),
                       ("network", "imboy-local")):
        if run(["docker", kind, "inspect", name]).returncode == 0:
            raise RuntimeError(f"Refusing to use existing {kind} {name}")
    env = environment(BACKEND_IMAGE=image,
                      COMPOSE_PROJECT_NAME="imboy-local-ci-" + os.environ["GITHUB_RUN_ID"])
    command = ["docker", "compose", "--env-file", str(ROOT / ".env"),
               "-f", str(ROOT / "docker-compose.yml")]
    for profile in ("dev", "gateway", "api", "admin", "widget", "livekit"):
        command += ["--profile", profile]

    def execute(args, timeout=1200, capture=False, check=True):
        return subprocess.run(args, cwd=ROOT, env=env, check=check, text=True,
                              capture_output=capture, timeout=timeout)

    text = (ROOT / ".env.example").read_text()
    text += "\n" + "\n".join(f"{key}={value}" for key, value in FIXTURE_VALUES.items()) + "\n"
    (ROOT / ".env").write_text(text)
    (ROOT / ".env").chmod(0o600)
    try:
        model = json.loads(execute(command + ["config", "--format", "json"], capture=True).stdout)
        edge_probe = model["services"]["imboy_edge"]["healthcheck"]["test"]
        # Exercise the exact rendered probe with no listener; the later --wait
        # exercises its positive path against the real edge process.
        refused = execute(command + ["run", "--rm", "--no-deps", "--entrypoint", "/bin/sh",
                                     "imboy_edge", "-ec", edge_probe[1]],
                          capture=True, check=False, timeout=30)
        if refused.returncode != 1:
            raise AssertionError("The edge health probe did not reject an absent HTTP listener")
        print("PASS edge health probe rejects an absent HTTP listener", flush=True)
        execute(command + ["build", "imboy_admin", "imboy_widget"])
        # The first setup phase also proves Nginx starts without API/Admin/Widget/LiveKit.
        execute(["bash", "scripts/dev_setup.sh", "--no-build", "--with-widget", "--with-livekit"], timeout=600)
        services = execute(command + ["ps", "--services", "--status", "running"], capture=True).stdout.split()
        expected = {"imboy_pg18", "garage", "imboy_nginx", "imboy_backend", "imboy_edge",
                    "imboy_admin", "imboy_widget", "imboy_livekit"}
        if set(services) != expected:
            raise AssertionError("The selected stack did not start exactly the expected services")
        gateway_host_boundary()
        gateway_object_roundtrip(command, execute)

        # Force recreation, not just restart: the symlink must lead to the persisted volume.
        key_probe = ["exec", "-T", "imboy_backend", "sha256sum",
                     "/opt/imboy/priv_runtime/dev_keys/login_rsa_pub.pem"]
        before = execute(command + key_probe, capture=True).stdout
        execute(command + ["up", "-d", "--no-deps", "--no-build", "--force-recreate",
                           "--wait", "--wait-timeout", "300", "imboy_backend"], timeout=360)
        after = execute(command + key_probe, capture=True).stdout
        if before != after:
            raise AssertionError("Local RSA identity changed after container recreation")
        host_gateway_roundtrip(command, execute, env)
        print("PASS real local stack: source builds, health, S3/public files, durable RSA and host gateway")
    except BaseException:
        # The fixture contains only synthetic credentials; redact those before diagnostics.
        def diagnose(label, args):
            try:
                result = execute(args, capture=True, check=False, timeout=30)
                output = result.stdout + result.stderr
            except (OSError, subprocess.SubprocessError) as error:
                output = str(error)
            for value in FIXTURE_VALUES.values():
                output = output.replace(value, "[REDACTED_FIXTURE]")
            print(f"==> {label}\n{output}", file=sys.stderr)

        diagnose("Container logs", command + ["logs", "--no-color", "--tail", "60"])
        diagnose("Container status", command + ["ps", "--all"])
        try:
            edge = execute(command + ["ps", "--all", "--quiet", "imboy_edge"],
                           capture=True, check=False, timeout=15).stdout.split()
        except (OSError, subprocess.SubprocessError):
            edge = []
        if edge:
            diagnose("Edge health probe history", ["docker", "inspect", "--format",
                                                   "{{json .State.Health}}", edge[0]])
        try:
            backend = execute(command + ["ps", "--all", "--quiet", "imboy_backend"],
                              capture=True, check=False, timeout=15).stdout.split()
        except (OSError, subprocess.SubprocessError):
            backend = []
        if backend:
            # Only inspect health, never the full object containing container secrets.
            diagnose("Backend health probe history", ["docker", "inspect", "--format",
                                                      "{{json .State.Health}}", backend[0]])
            diagnose("Backend readiness components", command + [
                "exec", "-T", "imboy_backend", "/opt/imboy/bin/imboy", "eval",
                "#{db => (catch healthz_handler:probe_db()), "
                "tsid => (catch elib_tsid:readiness()), "
                "cache_sync => (catch imboy_cache_sync:status()), "
                "cluster_nodes => application:get_env(imboy, cluster_nodes, []), "
                "runtime => (catch imboy_app:readiness())}."])
        raise
    finally:
        # All fixed names were proven absent; these volumes belong only to this CI fixture.
        subprocess.run(command + ["down", "--volumes", "--remove-orphans"],
                       cwd=ROOT, env=env, timeout=120)
        (ROOT / ".env").unlink()


def gateway_host_boundary():
    # Both raw Host and virtual-host selection must be bounded. Absolute-form
    # requests prove a trusted request-line host cannot smuggle an untrusted Host.
    cases = [
        ("unconfigured.example:8080", "/readyz"),
        ("api.localhost.evil.example:8080", "/readyz"),
        ("api.localhost:8081", "/readyz"),
        ("unconfigured.example:8080", "http://api.localhost:8080/readyz"),
        ("api.localhost:8080", "http://unconfigured.example:8080/readyz"),
    ]
    for host, path in cases:
        connection = http.client.HTTPConnection("127.0.0.1", 8080, timeout=5)
        try:
            connection.request("GET", path, headers={"Host": host})
            response = connection.getresponse()
            response.read()
            if response.status != 421:
                raise AssertionError("Gateway accepted an unconfigured Host or request authority")
        finally:
            connection.close()
    print("PASS gateway rejects unconfigured Host and absolute-request authorities", flush=True)


def host_gateway_roundtrip(command, execute, env):
    """The official Nginx entrypoint must render a host IP before dynamic proxying."""
    payload = b"local-host-backend-fixture"

    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_args):
            pass

        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)

    with http.server.ThreadingHTTPServer(("0.0.0.0", 0), Handler) as server:
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        env["API_UPSTREAM"] = f"host.docker.internal:{server.server_port}"
        try:
            execute(command + ["up", "-d", "--no-deps", "--no-build", "--force-recreate",
                               "--wait", "--wait-timeout", "60", "imboy_nginx"], timeout=90)
            connection = http.client.HTTPConnection("127.0.0.1", 8080, timeout=15)
            connection.request("GET", "/host-backend-probe", headers={"Host": "api.localhost:8080"})
            response = connection.getresponse()
            if response.status != 200 or response.read() != payload:
                raise AssertionError("Gateway could not reach the host backend through extra_hosts")
            connection.close()
        finally:
            env.pop("API_UPSTREAM", None)
            server.shutdown()
            thread.join(timeout=5)


def gateway_object_roundtrip(command, execute):
    """Real SigV4 PUT/GET tests raw escaping, prefix rewrite and the signed Host port."""
    host = "api.localhost:8080"
    object_path = "/ci%20smoke%2B%25/object.txt"
    payload = b"local-compose-object-fixture"
    widget_origin = "http://cs.localhost:8080"
    for origin, expected in ((widget_origin, 204), ("https://untrusted.example", 403)):
        connection = http.client.HTTPConnection("127.0.0.1", 8080, timeout=15)
        connection.request("OPTIONS", "/s3/imboy-public" + object_path, headers={
            "Host": host, "Origin": origin, "Access-Control-Request-Method": "PUT",
            "Access-Control-Request-Headers": "content-type,x-amz-checksum-sha256"})
        response = connection.getresponse()
        response.read()
        if response.status != expected or response.getheader("Access-Control-Allow-Origin") != (origin if expected == 204 else None):
            raise AssertionError("Storage preflight did not enforce the local origin allowlist")
        connection.close()

    def signed_request(method, path, body=b""):
        now = datetime.now(timezone.utc)
        date, timestamp = now.strftime("%Y%m%d"), now.strftime("%Y%m%dT%H%M%SZ")
        scope = date + "/garage/s3/aws4_request"
        digest = hashlib.sha256(body).hexdigest()
        headers = {"Host": host, "Origin": widget_origin, "x-amz-content-sha256": digest, "x-amz-date": timestamp}
        signed = "host;x-amz-content-sha256;x-amz-date"
        canonical_headers = f"host:{host}\nx-amz-content-sha256:{digest}\nx-amz-date:{timestamp}\n"
        canonical = "\n".join([method, path, "", canonical_headers, signed, digest])
        to_sign = "\n".join(["AWS4-HMAC-SHA256", timestamp, scope, hashlib.sha256(canonical.encode()).hexdigest()])
        signing_key = ("AWS4" + FIXTURE_VALUES["IMBOY_GARAGE_SECRET_KEY"]).encode()
        for part in (date, "garage", "s3", "aws4_request"):
            signing_key = hmac.new(signing_key, part.encode(), hashlib.sha256).digest()
        signature = hmac.new(signing_key, to_sign.encode(), hashlib.sha256).hexdigest()
        headers["Authorization"] = ("AWS4-HMAC-SHA256 Credential=" + FIXTURE_VALUES["IMBOY_GARAGE_ACCESS_KEY"]
                                    + "/" + scope + ", SignedHeaders=" + signed + ", Signature=" + signature)
        connection = http.client.HTTPConnection("127.0.0.1", 8080, timeout=15)
        connection.request(method, "/s3" + path, body=body, headers=headers)
        response = connection.getresponse()
        result = response.read()
        connection.close()
        if response.status != 200:
            raise AssertionError(f"Signed S3 gateway {method} failed: HTTP {response.status}")
        if response.getheader("Access-Control-Allow-Origin") != widget_origin:
            raise AssertionError("Signed object response omitted the permitted browser origin")
        return result

    for bucket in ("imboy", "imboy-public"):
        path = "/" + bucket + object_path
        signed_request("PUT", path, payload)
        if signed_request("GET", path) != payload:
            raise AssertionError("Signed S3 gateway did not return the uploaded object")
    connection = http.client.HTTPConnection("127.0.0.1", 8080, timeout=15)
    connection.request("GET", object_path, headers={"Host": "files.localhost:8080", "Origin": widget_origin})
    response = connection.getresponse()
    if response.status != 200 or response.read() != payload:
        raise AssertionError("Public bucket website did not return the uploaded object")
    if response.getheader("Access-Control-Allow-Origin") != widget_origin:
        raise AssertionError("Public website response omitted the permitted browser origin")
    connection.close()
    # The signed private GET above proves this object exists; web access must still fail.
    denied = execute(command + ["exec", "-T", "imboy_nginx", "wget", "-S", "-O", "/dev/null",
                                "--header=Host: imboy.garage.localhost", "http://garage:3902" + object_path],
                     capture=True, check=False)
    if denied.returncode == 0 or not re.search(r"HTTP/\S+ (403|404)\b", denied.stdout + denied.stderr):
        raise AssertionError("Private bucket was not denied on the Garage website endpoint")


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--docker":
        runtime_smoke(sys.argv[2])
    else:
        unittest.main(verbosity=2)
