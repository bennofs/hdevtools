#!/bin/sh

set -e

SOCK=`mktemp -u`

$HDEVTOOLS check --socket=$SOCK Child.hs

PARENT=`$HDEVTOOLS modulefile --socket=$SOCK Parent`
PARENT_FILE=`readlink -f ./Parent.hs`


[ "$PARENT" = "$PARENT_FILE" ]

$HDEVTOOLS --socket=$SOCK --stop-server
