# Regression tests against values computed with seg 0.5-7
library(seg)
library(sf)

data(segdata)
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]
x <- st_sf(segdata[, 1:2], geometry = grd)
tol <- 1e-9

idx <- function(a) c(a$d, a$r, a$h, as.vector(a$p))

# spseg, no smoothing
a <- spseg(x)
stopifnot(inherits(a, "SegSpatial"), inherits(a, "SegLocal"))
stopifnot(all.equal(idx(a), c(0.897859091203, 0.826493121823, 0.793878481835,
                              0.923394318402, 0.025535227199, 0.076605681598,
                              0.974464772801), tolerance = tol))
stopifnot(all.equal(idx(a), idx(spseg(x, useC = FALSE))))

# spseg, equal and kernel smoothing
b <- spseg(x, smoothing = "equal", nrow = 50, ncol = 50)
stopifnot(all.equal(idx(b), c(0.865620705294, 0.771322242608, 0.745766456715,
                              0.882601564350, 0.039132811883, 0.117398435650,
                              0.960867188117), tolerance = tol))
w <- matrix(c(1.5, 1.5, 1.5, 8.5, 8.5, 8.5, 8.5, 4.5, 5.5, 4.5, 5.5, 1.5),
            ncol = 2, byrow = TRUE)
k <- spseg(x, smoothing = "kernel", window = w, nrow = 20, ncol = 20)
stopifnot(all.equal(idx(k), c(0.814030281492, 0.685743818462, 0.648503957210,
                              0.819024900993, 0.083853875640, 0.180975099007,
                              0.916146124360), tolerance = tol))

# local values average to the global indices
for (o in list(a, b, k)) {
  wt <- rowSums(o$data) / sum(o$data)
  stopifnot(all.equal(colSums(wt * o$local), c(d = o$d, r = o$r, h = o$h)))
}
sfa <- st_as_sf(a)
stopifnot(inherits(sfa, "sf"), nrow(sfa) == 100)
sfk <- st_as_sf(k)
pop <- tapply(rowSums(k$data), factor(k$id, 1:100), sum)
pop[is.na(pop)] <- 0
stopifnot(all.equal(sum(pop / sum(pop) * sfk$d, na.rm = TRUE), k$d))

# plotting
pdf(NULL)
plot(a)
plot(a, index = "information", main = "H", xlab = "x", ylab = "y",
     axes = TRUE)
plot(k)
plot(spseg(st_coordinates(st_centroid(grd)), segdata[, 1:2]))
plot(localenv(x))
invisible(dev.off())

# other measures
stopifnot(all.equal(isp(x), 1.618513685424, tolerance = tol))
stopifnot(all.equal(as.vector(deseg(x, nrow = 50, ncol = 50)),
                    c(0.312637773667, 0.125, 0.5), tolerance = tol))
stopifnot(all.equal(conprof(segdata[, 7:8], graph = FALSE)$d,
                    0.670749697065, tolerance = tol))

# dissimilarity and its adjustments
dm <- sapply(1:6, function(i)
  dissim(grd, segdata[, (2 * i - 1):(2 * i)], adjust = TRUE)$dm)
stopifnot(all.equal(dm, c(0.944444444444, 0.827777777778, 0.5,
                          0.789327485380, 0.664992641648, 0.966666666667),
                    tolerance = tol))
r <- dissim(x, adjust = TRUE)
stopifnot(all.equal(c(r$d, r$dw, r$ds), c(1, 0.944444444444, 0.947222222222),
                    tolerance = tol))
rook <- st_relate(grd, pattern = "F***1****", sparse = FALSE) * 1
stopifnot(all.equal(dissim(data = segdata[, 1:2], nb = rook / sum(rook))$user,
                    r$dm))

# localenv with nb and dist objects
nb <- structure(lapply(st_relate(grd, pattern = "F***1****"), as.integer),
                class = "nb")
e1 <- localenv(x, sprel = nb)
stopifnot(all.equal(e1$env[1, ], colMeans(segdata[c(2, 11), 1:2])))
e2 <- localenv(x, sprel = dist(st_coordinates(st_centroid(grd))))
stopifnot(all.equal(e2$env, localenv(x)$env))
