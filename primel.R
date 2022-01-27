library(tidyverse)
library(matlab)

begin <- 12005
end <- 13000
my_step <- 1

numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1]
