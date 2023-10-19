
# Test data
Ncases <- 100
NNAs <- 10
Nxs <- seq_len(Ncases)
Ncols <- 3
Nids <- seq_len(Ncols)

vdata1 <- Nxs + runif(n = Ncases)
vdata1NAs <- vdata1
vdata1NAs[sample(Ncases, NNAs)] <- NA

vdata2 <- matrix(Nxs + runif(n = Ncases * Ncols), ncol = Ncols)
vdata2NAs <- vdata2
vdata2NAs[sample(Ncases * Ncols, NNAs * Ncols)] <- NA

x <- list(vdata1, vdata1NAs, vdata2, vdata2NAs)

tol <- sqrt(.Machine[["double.eps"]])

# Unit tests
test_that("gof", {
  for (k in seq_along(x)) {
    obs <- x[[k]]
    n <- NCOL(obs)
    nids <- seq_len(n)

    #--- Best case
    sim <- obs

    tmp_rmse <- rmse(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_identical(tmp_rmse[k], 0))
    tmp_mae <- mae(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_identical(tmp_mae[k], 0))
    tmp_nse <- NSE(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_identical(tmp_nse[k], 1))


    #--- Average case
    tmp <- if (n == 1) mean(obs, na.rm = TRUE) else colMeans(obs, na.rm = TRUE)
    sim <- matrix(tmp, nrow = Ncases, ncol = n, byrow = TRUE)

    tmp_rmse <- rmse(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_gt(tmp_rmse[k], 0))
    tmp_mae <- mae(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_gt(tmp_mae[k], 0))
    tmp_nse <- NSE(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_equal(tmp_nse[k], 0, tolerance = tol))


    #--- Poor case
    sim <- -obs

    tmp_rmse <- rmse(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_gt(tmp_rmse[k], 0))
    tmp_mae <- mae(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_gt(tmp_mae[k], 0))
    tmp_nse <- NSE(obs, sim, na.rm = TRUE)
    lapply(nids, function(k) expect_lt(tmp_nse[k], 0))
  }
})
