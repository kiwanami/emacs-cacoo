#!/bin/sh

NAME=cacoo
VER=2.1.2

FULLNAME=$NAME-$VER

mkdir -p $FULLNAME
cp cacoo.el cacoo-plugins.el cacoo-pkg.el $FULLNAME
tar cvf $FULLNAME.tar $FULLNAME/
