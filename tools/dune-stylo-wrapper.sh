#!/bin/bash

case $1 in
    "--impl"|"--intf")
        shift 1
        ;;

    *)
        echo "Unexpected first argument: $1"
        exit 1
esac

stylo style --idempotence-check --ast-check --width 80 $1
