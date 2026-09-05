# RGNRS

  [![R-CMD-check](https://github.com/EnquistLab/RGNRS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EnquistLab/RGNRS/actions/workflows/R-CMD-check.yaml)

R package for accessing the Geographic Name Resolution Service

## Description
The GNRS package for R (a.k.a. the RGNRS) provides users with access to the Geographic Name Resolution Service (GNRS) API in R.
The Geographic Name Resolution Service takes in the names of political units and standardizes them following https://www.geonames.org/ .  More information on the GNRS can be found on the BIEN website (https://bien.nceas.ucsb.edu/bien/tools/gnrs/), and code underlying the GNRS can be found on Github at https://github.com/ojalaquellueva/gnrs.

## Working offline

**Beta.** The offline engine is a new port of the resolution cascade the web
service runs, resolving against the service's own reference tables.  On the
package's validation sets it returns the same resolved political divisions as
the web service for the large majority of rows, and the same identifiers, so
local and service results can be joined; `vignette("GNRS_offline")` reports
the figures and goes through the differences.  It has not been independently
reviewed, so for work where the answer matters check a sample against
`GNRS()`.

Names can be resolved without an internet connection, against a locally cached
copy of the reference data:

```r
GNRS_local_build()   # one-off download, about 220 MB and a few minutes
results <- GNRS_local(gnrs_testfile)
GNRS_local_status()  # what is built, and at which version
GNRS_local_citations()
```

The service's reference was built from GADM 3.6 in 2020.  To resolve against
the current GADM release instead, with current `gid_*` identifiers and the
divisions added since, layer it on with `GNRS_local_build("gadm")`, or
`GNRS_local_build(c("points", "gadm"))` to have the links checked against
GADM's geometry with GeoNames' coordinates; see the
vignette for what changes.

`GNRS_local()` takes the same input as `GNRS()` and returns the same columns.
It is a separate function rather than an option on `GNRS()` because the two do
not always give the same answer: the alternate names are downloaded from
GeoNames directly and are newer than the service's copy, and the service
caches earlier answers, which the local version does not.
