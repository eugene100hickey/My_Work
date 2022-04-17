library(tidyverse)
library(matlab)

begin <- 94001
end <- 95000
my_step <- 10


numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1]

numbers[isprime(numbers)==1 & between(numbers %% 10000, 2000, 2100)]

maps::world.cities |> 
  filter(country.etc == "Italy") |> 
  ggplot(aes(long, lat, size = log(pop))) + 
  geom_point(alpha = 0.2) + 
  theme_void()
