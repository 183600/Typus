#!/usr/bin/env python3
import argparse
import subprocess
import sys
import unicodedata

def git_bytes(args):
    return subprocess.check_output(["git", *args])

def is_bad_path(path_bytes: bytes, ascii_only: bool):
    # 1) 非 UTF-8 直接视为坏路径
    try:
        s = path_bytes.decode("utf-8")
    except UnicodeDecodeError:
        return True, "non-utf8"

    # 2) 禁止 Unicode “Other” 类别字符：控制符/格式符/私用区/未分配等（Uc/Uf...）
    #    你遇到的那类“乱码目录”里常混有控制字符 (Cc)，比如 \x08 \x16 等
    for ch in s:
        cat = unicodedata.category(ch)
        if cat.startswith("C"):
            return True, f"unicode-{cat}"

    # 3) 可选：ASCII-only（禁止任何非 ASCII）
    if ascii_only:
        for ch in s:
            if ord(ch) > 0x7F:
                return True, "non-ascii"

    # 4) 出现 U+FFFD 通常意味着坏解码/拷贝残留，也判为坏
    if "\ufffd" in s:
        return True, "contains-U+FFFD"

    return False, ""

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--mode",
        choices=["fail", "clean"],
        default="fail",
        help="fail=发现就退出; clean=自动从仓库/工作区移除后继续",
    )
    ap.add_argument(
        "--ascii-only",
        action="store_true",
        help="若开启，则任何包含非 ASCII 字符的路径都视为坏路径（会禁止中文等文件名）",
    )
    args = ap.parse_args()

    tracked = git_bytes(["ls-files", "-z"]).split(b"\0")
    untracked = git_bytes(["ls-files", "--others", "--exclude-standard", "-z"]).split(b"\0")

    tracked_set = {p for p in tracked if p}
    candidates = [p for p in (tracked + untracked) if p]

    bad = []
    reasons = {}
    for p in candidates:
        bad_flag, reason = is_bad_path(p, ascii_only=args.ascii_only)
        if bad_flag:
            bad.append(p)
            reasons[p] = reason

    if not bad:
        print("guard: OK (no bad paths)")
        return 0

    print("guard: BAD PATHS detected:", file=sys.stderr)
    for p in bad:
        show = p.decode("utf-8", "backslashreplace")
        print(f"  - {show}   [{reasons.get(p,'')}]  (tracked={p in tracked_set})", file=sys.stderr)

    if args.mode == "fail":
        print("guard: mode=fail -> abort.", file=sys.stderr)
        return 2

    # mode=clean：从 index 移除 + 从工作区删除
    bad_tracked = [p for p in bad if p in tracked_set]
    bad_all = bad[:]

    if bad_tracked:
        data = b"\0".join(bad_tracked) + b"\0"
        subprocess.run(
            ["git", "rm", "-r", "--cached", "-f", "--pathspec-from-file=-", "--pathspec-file-nul"],
            input=data,
            check=False,
        )

    # 删除工作区对应路径（包含 untracked）
    data = b"\0".join(bad_all) + b"\0"
    subprocess.run(["xargs", "-0", "rm", "-rf", "--"], input=data, check=False)

    print(f"guard: cleaned {len(bad)} bad paths.")
    return 0

if __name__ == "__main__":
    sys.exit(main())
