#!/bin/sh

# Remove artifacts that no longer align with targets in the pipeline
R -q -e "targets::tar_prune()"

# Build the artifacts in the pipeline
R -q -e "targets::tar_make()"
