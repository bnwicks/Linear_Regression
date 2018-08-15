### Introduction to Regression   
## Baseball as a Motivating Example   
# Bases on Balls or Stolen Bases?
library(tidyverse)
library(Lahman)
ds_theme_set()

# HR per game v Runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# HR per game v Runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# BB per game v Runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# teams with more at-bats per game have more runs per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Correlation
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarise(mean(father), sd(father), mean(son))

galton_heights %>%
  ggplot(aes((father)/2,son)) +
  geom_point(alpha = 0.5)

galton_heights %>% summarise(cor(father, son))

## Sample Correlation is a Random Variable
set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarise(cor(father, son))
R

# Monte Carlo
B <- 1000
n <- 25
R <- replicate(B, {
  sample_n(galton_heights, n, replace = TRUE) %>%
    summarise(r=cor(father, son)) %>%
    .$r
})

data.frame(R) %>%
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color ="black")
  
})

mean(R)
sd(R)

# Conditional Average
conditional_avg <- 
  galton_heights %>% 
  filter(round(father) ==  72) %>%
  summarise(avg = mean(son)) %>%
  .$avg

conditional_avg

galton_heights %>%
  mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()


r <- galton_heights %>%
  summarise(r = cor(father,son)) %>%
  .$r

galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# Anscombe's Quartet/Stratification
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)
