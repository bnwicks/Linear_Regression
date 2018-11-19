### Tibbles, do, and broom  

library(tidyverse)
library(broom)
library(Lahman)


dat <-
  Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G)

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

## Broom
dat %>% 
  group_by(HR) %>% 
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

## Building a Better Offensive Metric for Baseball
fit_1 <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit_1, conf.int = TRUE)


fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR /G,
         R=R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

coeffs <- tidy(fit, conf.int = TRUE)
coeffs

Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR /G,
         R=R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>% 
  ggplot(aes(R, R_hat, label = teamID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text()

## Regression Fallacy
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

ROY <- ROY %>%
  filter(yearID ==  rookie_year | yearID == rookie_year+1) 

ROY <- ROY %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophmore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

ROY

## Data dredging

N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

res <- sim_data %>%
  group_by(group) %>%
  summarise(r = cor(X,Y)) %>%
  arrange(desc(r))
res

sim_data %>%
  filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(X,Y)) +
  geom_point() +
  geom_smooth(method = "lm")

sim_data %>%
  #filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))
