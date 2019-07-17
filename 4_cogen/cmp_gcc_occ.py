#!/usr/bin/python3

import subprocess,sys,random

def run(cmd):
    # print(cmd)
    p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, encoding="utf-8")
    out = p.stdout.read()
    p.wait()
    return out

def compare_gcc_occ(prefix, a, b, rg):
    """
    run ./exe/prefix.gcc and ./exe/prefix.cc
    compre the outputs
    """
    args = [ rg.randint(a, b) for _ in range(12) ]
    exe_gcc = "%s.gcc" % prefix
    exe_occ = "%s.occ" % prefix
    args_s = " ".join([ str(x) for x in args ])
    out_gcc = run("%s %s" % (exe_gcc, args_s))
    out_occ = run("%s %s" % (exe_occ, args_s))
    if out_gcc == out_occ:
        print("OK: %s %s" % (prefix, args_s))
        return 1 
    else:
        print("NG: %s %s" % (prefix, args_s))
        print("  %s.gcc %s -> %s" % (prefix, args_s, out_gcc))
        print("  %s.occ %s -> %s" % (prefix, args_s, out_occ))
        return 0

def main():
    argv = sys.argv
    prefix = argv[1]
    n      = int(argv[2]) if len(argv) > 2 else 10
    a      = int(argv[3]) if len(argv) > 3 else -5
    b      = int(argv[4]) if len(argv) > 4 else 5
    seed   = argv[5] if len(argv) > 5 else "abcdefg"
    rg = random.Random()
    rg.seed(seed)
    for i in range(n):
        if compare_gcc_occ(prefix, a, b, rg) == 0:
            return 1
    return 0

sys.exit(main())

