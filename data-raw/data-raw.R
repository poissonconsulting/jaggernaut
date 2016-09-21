library(devtools)

peregrine <- data.frame(Year = 1964:2003)

set.seed(105)
peregrine$Pairs <- rpois(nrow(peregrine), 20)
peregrine$R.pairs <- rbinom(nrow(peregrine), peregrine$Pairs, 0.6)
peregrine$Eyasses <- rbinom(nrow(peregrine), peregrine$R.pairs, 0.4)

use_data(peregrine, overwrite = TRUE)
