### Generate random 'games' data
## number of data points 
N <- 231

## Forward fold (Gaussian)
# How far away from your toes can you reach? (Negative values for flexible people)
# mean and sd are a guess.
ffold <- rnorm(N, mean = 3, sd = 7)

## Paper balls (Poisson)
# 6 paper balls. How many can you get in the wastepaper basket?
# lambda will vary according to distance from basket)
pballs <- rpois(N, lambda = 3.1)

## Coloured balls
# 6 balls in a bag, 2 of colour X and 4 of colour Y. 
# Roll a die, the number is the number of trials you get.
# Record the number of 'successes' (choose colour X) and 'failures' (choose colour Y)
ntrials <- ceiling(runif(N) * 6) # random number of trials for each person
cballs_win <- rbinom(N, ntrials, 1/3) # random wins
cballs_lose <- ntrials - cballs_win # losses


## Data frame
games <- data.frame(ffold, pballs, cballs_win, cballs_lose)
write.csv(games, "data/games.csv")
