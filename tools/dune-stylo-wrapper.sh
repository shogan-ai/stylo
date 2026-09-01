#!/bin/bash

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

case $1 in
    "--impl"|"--intf")
        shift 1
        ;;

    *)
        echo "Unexpected first argument: $1"
        exit 1
esac

if [[ -f "$ROOT/stylo.opam" ]]; then
    # we are running locally in the workspace, so don't look for stylo in path
    STYLO="$ROOT/bin/main.exe"

    # also, leave vendor/ and test/ alone
    case $1 in
        vendor/*|test/*)
            cat $1 # reemit as is
            exit 0
            ;;
    esac
else
    STYLO=stylo
fi

$STYLO style --idempotence-check --ast-check --width 80 "$1"
