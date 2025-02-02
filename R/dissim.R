# ------------------------------------------------------------------------------
# Function 'dissim()'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# Depends: -
# ------------------------------------------------------------------------------
dissim <- function(x, data, adjust = FALSE, use_queen = FALSE, verbose = FALSE) {

  # If a sf object 'x' is provided:
  if (!missing(x)) {
    if (verbose)
      tmp <- chksegdata(x, data)
    else
      tmp <- suppressMessages(chksegdata(x, data))
    
    coords <- tmp$coords
    data <- tmp$data
  }

  if (ncol(data) > 2) {
    warning("'data' has more than two columns; only the first two are used",
            call. = FALSE)
    data <- data[,1:2]
  }
  
  # 'data' is supposed to contain population. It cannot be negative values.
  # Makes an error if any of the values is less than 0.
  if (any(data < 0))
    stop("negative value(s) in 'data'", call. = FALSE)
  
  colsum <- apply(data, 2, sum)
  if (any(colsum <= 0))
    stop("the sum of each column in 'data' must be > 0", call. = FALSE)
  
  out <- list(d = NA, dm = NA, dw = NA, ds = NA)
  
  # ****************************************************************************
  #
  # Duncan and Duncan's index of dissimilarity
  #
  # ****************************************************************************
  b <- data[,1] / sum(data[,1])  # Blacks
  w <- data[,2] / sum(data[,2])  # Whites
  out$d <- as.vector(sum(abs(b - w)) / 2)
  
  # ****************************************************************************
  #
  # Adjust the original D index if 'adjust' is TRUE and 'x' is given.
  #
  # The function attempts the following adjustments.
  #   (1) Morrill (1991) if 'spdep' is available,
  #   (2) Wong (1993) if 'spdep' and 'terra' are available,
  #   (3) Custom adjustment if an user defined 'nb' is given.
  #
  # ****************************************************************************
  if (!missing(x) & adjust) {
    
    userpkg <- .packages(all.available = TRUE) # Find out packages available
    
    if ("spdep" %in% userpkg) {
      tmp <- tryCatch(.use_contiguity(x, data, use_queen, verbose),
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

  return(out)
}
