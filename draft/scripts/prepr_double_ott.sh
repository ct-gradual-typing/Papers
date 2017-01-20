#!/bin/bash

FILE=$1

perl -p -i'.backup' -e 's/<<(.*?)>>/[[$1]]/g;' $1


