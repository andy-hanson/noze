#! /bin/sh
scons --quiet && ./bin/noze run test/test.nz -- $@
