library(tidyverse)
library(matlab)

begin <- 50631
end <- 100000
my_step <- 10000

numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1]

numbers[isprime(numbers)==1 & between(numbers %% 1000, 800, 900)]
