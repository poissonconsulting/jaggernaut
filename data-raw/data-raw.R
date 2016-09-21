library(devtools)

falcon <- data.frame(Year = 1974:2013)

set.seed(105)
falcon$Pairs <- rpois(nrow(falcon), 20)
falcon$R.pairs <- rbinom(nrow(falcon), falcon$Pairs, 0.6)
falcon$Eyasses <- rbinom(nrow(falcon), falcon$R.pairs, 0.4)

use_data(falcon, overwrite = TRUE)
