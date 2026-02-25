#!/usr/bin/env bash
set -u

usage() {
  cat <<'EOF'
Usage:
  scripts/run-test-with-diagnostics.sh [--outdir DIR] [--interval SEC] [--] [command...]

Examples:
  scripts/run-test-with-diagnostics.sh
  scripts/run-test-with-diagnostics.sh --interval 1 -- cabal test --jobs=1

Defaults:
  command  : cabal test --jobs=1 --test-show-details=failures
  interval : 2 sec
  outdir   : diagnostics/<timestamp>
EOF
}

interval=2
outdir="diagnostics/$(date +%Y%m%d-%H%M%S)"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --outdir)
      outdir="$2"
      shift 2
      ;;
    --interval)
      interval="$2"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

if [[ $# -gt 0 ]]; then
  cmd=("$@")
else
  cmd=(cabal test --jobs=1 --test-show-details=failures)
fi

mkdir -p "$outdir"

meta_log="$outdir/meta.txt"
command_log="$outdir/command.log"
process_log="$outdir/process-sample.log"
vm_log="$outdir/vm-sample.log"
summary_log="$outdir/summary.txt"
heartbeat_file="$outdir/heartbeat.txt"

{
  echo "started_at=$(date -Iseconds)"
  echo "pwd=$PWD"
  echo "interval_sec=$interval"
  echo -n "command="
  printf '%q ' "${cmd[@]}"
  echo
  echo "--- versions ---"
  cabal --version 2>/dev/null || true
  ghc --version 2>/dev/null || true
  uname -a
} > "$meta_log"

sample_processes() {
  while :; do
    {
      echo "=== $(date -Iseconds) ==="
      ps -eo pid,ppid,comm,%cpu,%mem,rss,vsz,etime --sort=-%cpu | head -n 25
      echo
    } >> "$process_log"
    date -Iseconds > "$heartbeat_file"
    sleep "$interval"
  done
}

sample_vm() {
  while :; do
    {
      echo "=== $(date -Iseconds) ==="
      free -h
      echo "loadavg $(cat /proc/loadavg)"
      echo
    } >> "$vm_log"
    sleep "$interval"
  done
}

process_sampler_pid=""
vm_sampler_pid=""

cleanup() {
  [[ -n "$process_sampler_pid" ]] && kill "$process_sampler_pid" 2>/dev/null || true
  [[ -n "$vm_sampler_pid" ]] && kill "$vm_sampler_pid" 2>/dev/null || true
}

finalize() {
  {
    echo "finished_at=$(date -Iseconds)"
    echo "exit_code=$exit_code"
    echo "outdir=$outdir"
    echo
    echo "--- command tail ---"
    tail -n 60 "$command_log" 2>/dev/null || true
    echo
    echo "--- latest process sample ---"
    tail -n 30 "$process_log" 2>/dev/null || true
  } > "$summary_log"

  echo "Diagnostic logs written to: $outdir"
  echo "Summary: $summary_log"
}

on_exit() {
  exit_code=$?
  cleanup
  finalize
  exit "$exit_code"
}

trap on_exit EXIT
trap 'exit 130' INT TERM

sample_processes &
process_sampler_pid=$!

sample_vm &
vm_sampler_pid=$!

{
  echo "=== command start $(date -Iseconds) ==="
  "${cmd[@]}"
  echo "=== command end $(date -Iseconds) ==="
} > "$command_log" 2>&1
