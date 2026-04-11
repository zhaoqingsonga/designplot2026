#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

PORT=3939

echo "=========================================="
echo "田间试验设计 designplot2026"
echo "=========================================="
echo "工作目录: $SCRIPT_DIR"
echo "端口: $PORT"
echo "本地访问: http://localhost:$PORT"
echo "=========================================="

if [[ "$(uname)" == "Darwin" ]]; then
    IP=$(ipconfig getifaddr en0 2>/dev/null || ipconfig getifaddr en1 2>/dev/null || echo "请手动查看")
    echo "局域网访问: http://$IP:$PORT"
fi

echo ""
echo "启动 R Shiny 应用..."
echo "=========================================="

exec Rscript run_lan.R "host=0.0.0.0" "port=$PORT"
