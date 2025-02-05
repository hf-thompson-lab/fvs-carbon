#!/bin/sh
R -q -e "targets::tar_prune()"
R -q -e "targets::tar_make()"
