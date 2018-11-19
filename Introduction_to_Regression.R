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

## Confounding: Are BBs More Predictive?
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H - HR - X2B - X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarise(cor(BB,HR), cor(Singles, HR), cor(BB, Singles), cor(BB/G,R/G), cor(HR/G,R/G))

dat <-
  Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G)

dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

dat %>%
  summarise(slope = cor(BB_per_game, R_per_game) )

# Coefficent for Home Runs
dat <-
  Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R/G)

dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") 

dat %>%
  summarise(slope = cor(HR_per_game, R_per_game) * sd(R_per_game)/sd(HR_per_game))

# Coefficent for Bases on Balls
dat <-
  Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G,
         HR_per_game = HR/G,
         R_per_game = R/G)

dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") 

dat %>%
  summarise(slope = cor(BB_per_game, R_per_game) * sd(R_per_game)/sd(BB_per_game))


## Linear Models
# LSS Function
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>%
  ggplot(aes(beta1,rss)) +
  geom_line(aes(beta1, rss), col=2)

fit <- lm(son ~ father, data = galton_heights)
summary(fit)

# Number of runs per game based on the number of bases on balls
# LSS Function
dat <-
  Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game= BB/G,
         HR_per_game = HR/G,
         R_per_game = R/G)

## LSE are Random Variables
B <- 1000
N <- 50

lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

library(gridExtra)
p1 <- lse %>%
  ggplot(aes(beta_0)) +
  geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>%
  ggplot(aes(beta_1)) +
  geom_histogram(binwidth = 0.1, color = "black")

grid.arrange(p1,p2,ncol = 2)

sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary

# Questions
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
summary(lse)

## Further Questions
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

##
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

