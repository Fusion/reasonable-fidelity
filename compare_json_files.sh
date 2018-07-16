#!/bin/bash
diff <(jq -S . "$1") <(jq -S . "$2")
