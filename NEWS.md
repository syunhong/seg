# seg 0.6-0

## Spatial data handling

* Spatial data are now handled with 'sf'. Functions accept `sf`/`sfc`
  objects (points or polygons) as well as coordinate matrices and `ppp`
  objects. 'sp' objects are still accepted and are converted with
  `sf::st_as_sf()`.
* The dependencies on 'sp', 'spdep', 'spgrass6' and 'rgdal' have been
  removed. 'splancs' is still used for kernel smoothing.

## Classes

* The S4 classes `SegLocal`, `SegSpatial` and `SegDecomp` are now S3 classes.
  Components are extracted with `$` instead of `@` (e.g., `x$d`), and the
  projection is an 'sf' `crs` object in `x$crs` (previously the slot
  `proj4string`).
* The `spplot()` methods and the coercion methods to 'sp' classes have been
  replaced by `st_as_sf()` methods.

## New features

* `spseg()` and `spatseg()` now return the local values of the dissimilarity,
  relative diversity and information theory indices (component `local`),
  whose population-weighted mean equals the global index.
* `plot()` on the result of `spseg()` maps these local values on the input
  polygons or points, to show where segregation is strong or weak. Arguments
  of `plot.sf()` such as `main`, `xlab`, `ylab`, `axes`, `breaks` and `pal`
  can be used.

## dissim()

* `dissim(x, adjust = TRUE)` now computes Morrill's D(adj) and Wong's D(w) and
  D(s) with 'sf', without GRASS. In 0.5-7 the automatic adjustment failed
  silently, so `dm`, `dw` and `ds` were always `NA`.
* The perimeter used for D(s) is still the length of the boundary shared with
  other units (not the total perimeter), following Wong (1993). This is now
  documented.
* The arguments `p2n.args`, `n2m.args`, `wVECT.args` and `v2n.args` have been
  removed.
* The warning that the elements of `nb` do not sum to one is no longer given
  for differences due to floating-point rounding.

## Bug fixes

* `spseg(smoothing = "equal")` failed or assigned population to the wrong
  polygons when some polygons contained no grid points. Such polygons are now
  dropped with a warning.
* `plot()` on a `SegLocal` object plotted the wrong column when `which.col`
  was given.

## Other changes

* Kernel smoothing (`spseg(smoothing = "kernel")`, `deseg()`) now warns that
  non-integer population counts are truncated.
* `localenv()` with a `dist` or `nb` object, `isp()` and `dissim()` are
  vectorised and faster. `localenv()` with an `nb` object no longer needs
  'spdep'.
* `seg()` and `whiteseg()` now give a deprecation warning; use `dissim()` and
  `isp()`.
* The C code uses registered routines and R's memory allocation.
* The index values are unchanged from version 0.5-7 (up to floating-point
  rounding).
