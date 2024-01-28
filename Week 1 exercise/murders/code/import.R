filename <- "../data/murders.csv"
dat <- read.csv(filename)

out <- "../rdas/dat.rda"
dat <- save(dat, file = out)
