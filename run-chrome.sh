#!/usr/bin/env sh

CHROME_BIN="${CHROME_BIN:-chromium}"
exec "$CHROME_BIN" --no-sandbox --headless --disable-gpu --repl $1
