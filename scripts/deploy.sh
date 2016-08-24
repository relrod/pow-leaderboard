#!/usr/bin/env bash
set -e
out="dist/build/pow-leaderboard/pow-leaderboard"
cabal build
strip "$out"
"$out" > /dev/null || (echo "Test run failed!"; exit 1)
scp "$out" origin.elrod.me:/tmp/powleaderboard
ssh origin.elrod.me "/tmp/powleaderboard > /dev/null" ||
  (echo "Test run on server failed!"; exit 1)
ssh origin.elrod.me "mv /tmp/powleaderboard /srv/webmount/ysupow.me/powleaderboard"
