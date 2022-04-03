library(tidyverse)
library(matlab)

begin <- 30023
end <- 99999
my_step <- 1000

numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1]

numbers[isprime(numbers)==1 & between(numbers %% 10000, 2000, 2100)]
