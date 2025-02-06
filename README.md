# fvs-carbon

Examine the use of FVS for modeling carbon, especially for carbon offset markets.

Distributed under the MIT license; see the file `LICENSE`.

# Maturity

This project is a work in progress.

# Packages

Install `targets` and `tarchetypes`; `targets` should take care of everything
else. In the event that it doesn't, required packages are listed at the top of
`_targets.R`.

You will need `pandoc` installed and a working `latex` distribution;
if you are able to knit Rmd to PDF, you should be all set.

You will need a working FVS installation, with the FVSne.exe program installed.
Currently this is hard-coded to look in `C:\FVS\FVSbin\` for `FVSne.exe`.

# Building

Run `build.sh` on MacOS or Linux; `build.cmd` on Windows. The RStudio project
is pre-configured so that `Build All` will do the right thing on Windows.

Alternatively, run `targets::tar_make()` directly.

Note that this project runs FVS on thousands of plots in many configurations;
on a single processor this will take tens of hours. Work is underway to
parallelize processing, with early effort showing that running 60-way parallel
is practical.
