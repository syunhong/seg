# ------------------------------------------------------------------------------
# Function 'dissim()'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# Depends: -
# ------------------------------------------------------------------------------
dissim <- function(x, data, nb, adjust = FALSE, p2n.args, n2m.args, 
                   verbose = FALSE) {
  
  # Get processed data from chksegdata()
  tmp <- chksegdata(x)
  coords <- tmp$coords
  data <- tmp$data
  proj4string <- tmp$proj4string
  
  if (ncol(data) > 2) {
    warning("'data' has more than two columns; only the first two are used",
            call. = FALSE)
    data <- data[,1:2]
  }
  
  if (any(data < 0))
    stop("negative value(s) in 'data'", call. = FALSE)
  
  colsum <- colSums(data)  # Optimized for performance
  if (any(colsum <= 0))
    stop("the sum of each column in 'data' must be > 0", call. = FALSE)
  
  out <- list(d = NA, dm = NA, dw = NA, ds = NA, user = NA)
  
  # Duncan and Duncan's index of dissimilarity
  b <- data[,1] / sum(data[,1])  # Ensured matrix indexing
  w <- data[,2] / sum(data[,2])
  out$d <- as.vector(sum(abs(b - w)) / 2)
  
  if (!missing(x) && adjust) {  # Logical operation fixed
    userpkg <- .packages(all.available = TRUE)
    if ("spdep" %in% userpkg) {
      tmp <- tryCatch(.use_contiguity(x, data, queen = TRUE, verbose),
                      error = function(e) print(e))
      if (is.numeric(tmp))
        out$dm <- out$d - tmp
      else if (verbose)
        message("failed to calculate D(adj)")
      
      if ("terra" %in% userpkg) {
        tmp <- tryCatch(.use_common_boundary(x, data, verbose),
                        error = function(e) print(e))
        if (is.numeric(tmp[1]))
          out$dw <- out$d - tmp[1]
        else if (verbose)
          message("failed to calculate D(w)")
        if (is.numeric(tmp[2]))
          out$ds <- out$d - tmp[2]
        else if (verbose)
          message("failed to calculate D(s)")
      }
    }
  }
 
  if (!missing(nb) && !is.null(nb) && is.list(nb))  # Added nb validation
    out$user <- out$d - .d.adjust(data, nb)
  
  return(out)
}
