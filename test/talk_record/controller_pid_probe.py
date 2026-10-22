#!/usr/bin/env python3
"""Read a same-user Darwin PID's real kernel start time without setuid ps.

ABI source: macOS SDK sys/proc_info.h struct proc_bsdinfo / PROC_PIDTBSDINFO;
libproc.h proc_pidinfo; sys/proc.h SRUN, SSLEEP, SSTOP, SZOMB. No size/offset
literal, environment mutation, ps fallback, process signals or network access.
"""
import argparse
import ctypes
import functools
import json
import os
import platform
import sys
import time

MAXCOMLEN = 16  # SDK sys/param.h; array lengths, not a structure offset.
PROC_PIDTBSDINFO = 3
LIVE_STATUSES = frozenset((2, 3, 4))
WEEKDAYS = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
MONTHS = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')


class ProbeError(ValueError):
    pass


class ProcBSDInfo(ctypes.Structure):
    # uid_t/gid_t are uint32 on this SDK. Preserve every declared field and order.
    _fields_ = [
        ('pbi_flags', ctypes.c_uint32),
        ('pbi_status', ctypes.c_uint32),
        ('pbi_xstatus', ctypes.c_uint32),
        ('pbi_pid', ctypes.c_uint32),
        ('pbi_ppid', ctypes.c_uint32),
        ('pbi_uid', ctypes.c_uint32),
        ('pbi_gid', ctypes.c_uint32),
        ('pbi_ruid', ctypes.c_uint32),
        ('pbi_rgid', ctypes.c_uint32),
        ('pbi_svuid', ctypes.c_uint32),
        ('pbi_svgid', ctypes.c_uint32),
        ('rfu_1', ctypes.c_uint32),
        ('pbi_comm', ctypes.c_char * MAXCOMLEN),
        ('pbi_name', ctypes.c_char * (2 * MAXCOMLEN)),
        ('pbi_nfiles', ctypes.c_uint32),
        ('pbi_pgid', ctypes.c_uint32),
        ('pbi_pjobc', ctypes.c_uint32),
        ('e_tdev', ctypes.c_uint32),
        ('e_tpgid', ctypes.c_uint32),
        ('pbi_nice', ctypes.c_int32),
        ('pbi_start_tvsec', ctypes.c_uint64),
        ('pbi_start_tvusec', ctypes.c_uint64),
    ]


@functools.lru_cache(maxsize=1)
def native_pidinfo():
    if platform.system() != 'Darwin':
        raise ProbeError('Darwin libproc probe required; no fallback')
    library = ctypes.CDLL('/usr/lib/libproc.dylib', use_errno=True)
    function = library.proc_pidinfo
    function.argtypes = (ctypes.c_int, ctypes.c_int, ctypes.c_uint64, ctypes.c_void_p, ctypes.c_int)
    function.restype = ctypes.c_int
    return function


def check_pid(pid):
    if type(pid) is not int or not 0 < pid <= 0x7fffffff:
        raise ProbeError('Positive native PID required')


def read_bsd_info(pid, function=None):
    check_pid(pid)
    function = native_pidinfo() if function is None else function
    info = ProcBSDInfo()
    size = ctypes.sizeof(ProcBSDInfo)
    ctypes.set_errno(0)
    length = function(pid, PROC_PIDTBSDINFO, 0, ctypes.byref(info), size)
    error = ctypes.get_errno()
    if length != size or error != 0:
        raise ProbeError('proc_pidinfo failed or returned an incomplete record (errno=%d)' % error)
    return info


def check_info(info, pid, uid, clock_ns):
    if info.pbi_pid != pid:
        raise ProbeError('Kernel PID mismatch')
    if (info.pbi_uid, info.pbi_ruid, info.pbi_svuid) != (uid, uid, uid):
        raise ProbeError('Kernel real/effective/saved UID mismatch')
    if info.pbi_status not in LIVE_STATUSES:
        raise ProbeError('Process is exited, being created or has an unknown status')
    if info.pbi_start_tvsec <= 0 or info.pbi_start_tvusec >= 1_000_000:
        raise ProbeError('Invalid kernel start timestamp')
    start_ns = info.pbi_start_tvsec * 1_000_000_000 + info.pbi_start_tvusec * 1000
    if start_ns > clock_ns:
        raise ProbeError('Kernel start timestamp is in the future')
    return (info.pbi_pid, info.pbi_uid, info.pbi_ruid, info.pbi_svuid,
            info.pbi_start_tvsec, info.pbi_start_tvusec)


def format_lstart(seconds, localtime=time.localtime):
    value = localtime(seconds)
    return ('%s %s %2d %02d:%02d:%02d %04d' % (
        WEEKDAYS[value.tm_wday], MONTHS[value.tm_mon - 1], value.tm_mday,
        value.tm_hour, value.tm_min, value.tm_sec, value.tm_year,
    ))


def probe_pid(pid, expected_uid=None, expected_start=None, function=None, clock=time.time_ns):
    check_pid(pid)
    uid = os.getuid()
    if os.geteuid() != uid:
        raise ProbeError('Setuid caller refused')
    expected_uid = uid if expected_uid is None else expected_uid
    if type(expected_uid) is not int or expected_uid != uid:
        raise ProbeError('Only the current user may be probed')
    first = read_bsd_info(pid, function)
    identity = check_info(first, pid, expected_uid, clock())
    second = read_bsd_info(pid, function)
    if check_info(second, pid, expected_uid, clock()) != identity:
        raise ProbeError('PID identity changed during probe')
    start = format_lstart(second.pbi_start_tvsec)
    if expected_start is not None and start != expected_start:
        raise ProbeError('Expected controller start does not match kernel start')
    return {
        'pid': pid, 'uid': expected_uid, 'start_lstart': start,
        'start_seconds': second.pbi_start_tvsec, 'start_microseconds': second.pbi_start_tvusec,
        'status': second.pbi_status, 'observed_at_unix_ns': clock(), 'source': 'darwin_libproc',
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--pid', required=True, type=int)
    parser.add_argument('--expected-uid', type=int)
    parser.add_argument('--expected-start')
    parser.add_argument('--json', action='store_true')
    args = parser.parse_args()
    try:
        result = probe_pid(args.pid, args.expected_uid, args.expected_start)
    except (ProbeError, OSError, AttributeError) as error:
        print('controller-probe: ' + str(error), file=sys.stderr)
        raise SystemExit(1)
    print(json.dumps(result, sort_keys=True) if args.json else result['start_lstart'])


if __name__ == '__main__':
    main()
