#!/usr/bin/env python3
"""Delta-minimize an unstable file while keeping the idempotency failure."""
import subprocess, sys, os

CHECK = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'check.sh')

def is_unstable(path):
    r = subprocess.run([CHECK, path], capture_output=True, text=True)
    return r.returncode == 1

def minimize(src):
    with open(src) as f:
        lines = f.readlines()
    # remove trailing/leading blank runs first
    n = len(lines)
    chunk = max(1, n // 2)
    while chunk >= 1:
        i = 0
        while i < len(lines):
            candidate = lines[:i] + lines[i+chunk:]
            if not candidate:
                i += chunk; continue
            with open(src, 'w') as f:
                f.writelines(candidate)
            if is_unstable(src):
                lines = candidate
            else:
                i += chunk
        chunk //= 2
    with open(src, 'w') as f:
        f.writelines(lines)

if __name__ == '__main__':
    minimize(sys.argv[1])
    print("minimized:", sys.argv[1], os.path.getsize(sys.argv[1]))
