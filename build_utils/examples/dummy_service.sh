#!/bin/bash

SELF=$(basename $0)

function usage() {
    echo "Usage: $SELF start | foreground | help"
}

case $1 in
    start)
        echo "Started"
        echo "Finished"
        ;;
    foreground)
        echo "Started"
        /bin/cat /dev/zero
        ;;
    help)
        usage
        exit 0
        ;;
    *)
        usage
        exit 1
        ;;
esac

