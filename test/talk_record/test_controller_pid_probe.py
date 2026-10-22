"""Pure syscall substitutes; none of these tests reads an actual external PID."""
import ctypes
import errno
import importlib.util
import io
import json
import os
import pathlib
import time
import unittest
from unittest import mock

SPEC = importlib.util.spec_from_file_location(
    'controller_pid_probe', pathlib.Path(__file__).with_name('controller_pid_probe.py')
)
P = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(P)
PID = 12345
START = 1_700_000_000
CLOCK = (START + 10) * 1_000_000_000


def info(**changes):
    value = P.ProcBSDInfo()
    for name, entry in {
        'pbi_pid': PID, 'pbi_uid': os.getuid(), 'pbi_ruid': os.getuid(),
        'pbi_svuid': os.getuid(), 'pbi_status': 2, 'pbi_start_tvsec': START,
        'pbi_start_tvusec': 123456, **changes,
    }.items():
        setattr(value, name, entry)
    return value


def syscall(values, error=0, returned_size=None):
    values = iter(values)

    def call(pid, flavor, argument, buffer, size):
        value = next(values)
        ctypes.memmove(buffer, ctypes.byref(value), ctypes.sizeof(value))
        ctypes.set_errno(error)
        return ctypes.sizeof(value) if returned_size is None else returned_size

    return call


class ControllerProbeTests(unittest.TestCase):
    def test_valid_same_user_two_read_probe(self):
        result = P.probe_pid(PID, function=syscall([info(), info()]), clock=lambda: CLOCK)
        self.assertEqual(PID, result['pid'])
        self.assertEqual(os.getuid(), result['uid'])
        self.assertEqual(START, result['start_seconds'])
        self.assertEqual(123456, result['start_microseconds'])
        self.assertEqual(P.format_lstart(START), result['start_lstart'])

    def test_native_call_receives_dynamic_structure_size(self):
        function = mock.Mock(wraps=syscall([info()]))
        P.read_bsd_info(PID, function)
        args = function.call_args.args
        self.assertEqual((PID, P.PROC_PIDTBSDINFO, 0), args[:3])
        self.assertEqual(ctypes.sizeof(P.ProcBSDInfo), args[4])

    def test_partial_oversize_and_error_returns_fail_closed(self):
        size = ctypes.sizeof(P.ProcBSDInfo)
        for value in (-1, 0, size - 1, size + 1):
            with self.subTest(length=value), self.assertRaises(P.ProbeError):
                P.read_bsd_info(PID, syscall([info()], returned_size=value))

    def test_errno_even_with_full_record_fails_closed(self):
        for value in (errno.ESRCH, errno.EPERM, errno.EIO):
            with self.subTest(error=value), self.assertRaises(P.ProbeError):
                P.read_bsd_info(PID, syscall([info()], error=value))

    def test_stale_errno_is_cleared_before_syscall(self):
        ctypes.set_errno(errno.EIO)

        def fill(pid, flavor, argument, buffer, size):
            value = info()
            ctypes.memmove(buffer, ctypes.byref(value), ctypes.sizeof(value))
            return size

        self.assertEqual(PID, P.read_bsd_info(PID, fill).pbi_pid)

    def test_invalid_pid_rejected_before_syscall(self):
        for pid in (None, True, '123', 0, -1, 0x80000000):
            function = mock.Mock()
            with self.subTest(pid=pid), self.assertRaises(P.ProbeError):
                P.read_bsd_info(pid, function)
            function.assert_not_called()

    def test_kernel_pid_mismatch_rejected(self):
        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, function=syscall([info(pbi_pid=PID + 1)]), clock=lambda: CLOCK)

    def test_each_uid_mismatch_rejected(self):
        for name in ('pbi_uid', 'pbi_ruid', 'pbi_svuid'):
            with self.subTest(uid_field=name), self.assertRaises(P.ProbeError):
                P.probe_pid(PID, function=syscall([info(**{name: os.getuid() + 1})]), clock=lambda: CLOCK)

    def test_foreign_expected_uid_rejected_before_syscall(self):
        function = mock.Mock()
        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, expected_uid=os.getuid() + 1, function=function)
        function.assert_not_called()

    def test_setuid_caller_rejected_before_syscall(self):
        function = mock.Mock()
        with mock.patch.object(P.os, 'geteuid', return_value=os.getuid() + 1), self.assertRaises(P.ProbeError):
            P.probe_pid(PID, function=function)
        function.assert_not_called()

    def test_zombie_creation_and_unknown_status_rejected(self):
        for status in (0, 1, 5, 6):
            with self.subTest(status=status), self.assertRaises(P.ProbeError):
                P.probe_pid(PID, function=syscall([info(pbi_status=status)]), clock=lambda: CLOCK)

    def test_sleep_and_stopped_status_are_live(self):
        for status in (3, 4):
            value = info(pbi_status=status)
            with self.subTest(status=status):
                self.assertEqual(status, P.probe_pid(
                    PID, function=syscall([value, value]), clock=lambda: CLOCK,
                )['status'])

    def test_invalid_zero_start_or_microsecond_range_rejected(self):
        for changes in ({'pbi_start_tvsec': 0}, {'pbi_start_tvusec': 1_000_000}):
            with self.subTest(changes=changes), self.assertRaises(P.ProbeError):
                P.probe_pid(PID, function=syscall([info(**changes)]), clock=lambda: CLOCK)

    def test_future_start_rejected_without_clock_tolerance(self):
        value = info(pbi_start_tvsec=START + 11)
        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, function=syscall([value]), clock=lambda: CLOCK)

    def test_microsecond_future_boundary_rejected(self):
        value = info(pbi_start_tvusec=1)
        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, function=syscall([value]), clock=lambda: START * 1_000_000_000 + 999)

    def test_pid_start_microseconds_changing_between_reads_rejected(self):
        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, function=syscall([info(), info(pbi_start_tvusec=123457)]), clock=lambda: CLOCK)

    def test_exit_between_reads_is_not_accepted(self):
        function = syscall([info()])
        calls = []

        def exit_second(*args):
            calls.append(1)
            if len(calls) == 1:
                return function(*args)
            ctypes.set_errno(errno.ESRCH)
            return 0

        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, function=exit_second, clock=lambda: CLOCK)

    def test_expected_lstart_mismatch_rejected(self):
        with self.assertRaises(P.ProbeError):
            P.probe_pid(PID, expected_start='not the kernel start',
                        function=syscall([info(), info()]), clock=lambda: CLOCK)

    def test_lstart_english_spacing_is_independent_of_locale(self):
        january = time.struct_time((2026, 1, 7, 4, 5, 6, 2, 7, -1))
        october = time.struct_time((2026, 10, 23, 12, 34, 56, 4, 296, -1))
        self.assertEqual('Wed Jan  7 04:05:06 2026', P.format_lstart(0, lambda _: january))
        self.assertEqual('Fri Oct 23 12:34:56 2026', P.format_lstart(0, lambda _: october))

    def test_cli_plain_output_is_only_lstart_and_newline(self):
        with mock.patch.object(P.sys, 'argv', ['probe', '--pid', str(PID)]), \
             mock.patch.object(P, 'probe_pid', return_value={'start_lstart': 'synthetic-start'}), \
             mock.patch('sys.stdout', new_callable=io.StringIO) as output:
            P.main()
        self.assertEqual('synthetic-start\n', output.getvalue())

    def test_cli_failure_has_no_success_stdout(self):
        with mock.patch.object(P.sys, 'argv', ['probe', '--pid', str(PID)]), \
             mock.patch.object(P, 'probe_pid', side_effect=P.ProbeError('denied')), \
             mock.patch('sys.stdout', new_callable=io.StringIO) as output, \
             mock.patch('sys.stderr', new_callable=io.StringIO) as error, \
             self.assertRaises(SystemExit) as failure:
            P.main()
        self.assertEqual(1, failure.exception.code)
        self.assertEqual('', output.getvalue())
        self.assertIn('denied', error.getvalue())

    def test_non_darwin_fails_without_ps_fallback(self):
        P.native_pidinfo.cache_clear()
        try:
            with mock.patch.object(P.platform, 'system', return_value='Linux'), self.assertRaises(P.ProbeError):
                P.native_pidinfo()
        finally:
            P.native_pidinfo.cache_clear()


if __name__ == '__main__':
    unittest.main()
