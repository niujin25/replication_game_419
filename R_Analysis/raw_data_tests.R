## Set directory (if needed)
## setwd("XXX")
## Import csv data
library(data.table)
exp_raw_data <- as.data.table(read.csv("Data/Data_Exp1234_raw.csv"))
## Experiments are listed 0 to 3 in the raw file which is not consistent with 
exp_raw_data[, Experiment := Experiment + 1]

library(testdat)

## =============================================================================
## Experiment 1 raw data test
## =============================================================================
## First experiment had 60 participants
expect_equal(length(unique(exp_raw_data[Experiment == 1, participant])), 60)

## In the first experiment, participants undergo 216 trials split into 3 equal
## sessions
expect_range(data = exp_raw_data[Experiment == 1], trial, min = 1, max = 72)
expect_range(data = exp_raw_data[Experiment == 1], trial, min = 1, max = 71)

expect_range(data = exp_raw_data[Experiment == 1], session, min = 1, max = 3)
expect_range(data = exp_raw_data[Experiment == 1], session, min = 1, max = 2)

## =============================================================================
## Experiment 2 raw data test
## =============================================================================
## Second experiment had 221 participants
expect_equal(length(unique(exp_raw_data[Experiment == 2, participant])), 221)

## The second experiment was divided into 3 parts of up to 32 trials. If the 
## participant ran out of endowment before the 32 trials, then the remaining
## trials were canceled. 
expect_range(data = exp_raw_data[Experiment == 2], trial, min = 1, max = 96)
expect_range(data = exp_raw_data[Experiment == 2], trial, min = 1, max = 95)

## Sessions were not recorded for experiment 2...

## =============================================================================
## Experiment 3 raw data test
## =============================================================================
## Third experiment had 426 participants
expect_equal(length(unique(exp_raw_data[Experiment == 3, participant])), 426)

## Participants completed two parts. In each part they received an endowment of
## $5 from which they would lose $1 with a probability of one-third if a ''loss
## pattern'' appeared. The part finished when the endowment was exhausted (after
## 5 losses) or after 32 trials.
## !!!
## Should only see a max of 64 trials, but participants record a max of 96 
## trials...
## !!!
expect_range(data = exp_raw_data[Experiment == 3], trial, min = 1, max = 64)
expect_range(data = exp_raw_data[Experiment == 3], trial, min = 1, max = 96)

## Test sessions (numbered 0 through 2 instead of 1 through 3 for experiment 3)
expect_range(data = exp_raw_data[Experiment == 3], session, min = 0, max = 2)

## =============================================================================
## Experiment 4 raw data test
## =============================================================================
## Second experiment had 221 participants
expect_equal(length(unique(exp_raw_data[Experiment == 4, participant])), 407)

## The second experiment was divided into 3 parts of up to 32 trials. If the 
## participant ran out of endowment before the 32 trials, then the remaining
## trials were canceled. 
expect_range(data = exp_raw_data[Experiment == 4], trial, min = 1, max = 96)
## Should throw an error
expect_range(data = exp_raw_data[Experiment == 4], trial, min = 1, max = 95)

## Test sessions(numbered 0 through 2 instead of 1 through 3 for experiment 4
## because one session was a 'neutral' session )
expect_range(data = exp_raw_data[Experiment == 4], session, min = 0, max = 2)
expect_range(data = exp_raw_data[Experiment == 4], session, min = 0, max = 2)