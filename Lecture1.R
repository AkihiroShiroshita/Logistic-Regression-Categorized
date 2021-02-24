## Preparation
library(tidyverse)
library(naniar)
library(visdat)
library(tableone)
library(broom)
library(car)
library(COUNT)
library(mice)
library(mitools)
dat <- read_csv("logistic_data_final.csv",
                locale = locale(encoding = "SHIFT-JIS"),
                col_types = cols(
                  id = col_double(),
                  death = col_logical(),
                  intubation = col_logical(),
                  Age = col_double(),
                  Eo = col_double(),
                  BUN = col_double(),
                  RR = col_double(),
                  Sbp = col_double(),
                  cons = col_logical(),
                  hospital = col_factor()
                ),
                guess_max = 1000, 
                na = "NA")
dat %>% glimpse()
vis_miss(dat)
miss_var_summary(dat)
## Complete case analysis  
### Table one  
vars <- c("Id", "death", "intubation", "Age", "Eo", "BUN", "RR", "Sbp", "cons", "hospital")
factorVars <- c("death", "intubation", "cons", "hospital")
table1 <- CreateTableOne(vars = vars, data = dat, includeNA = TRUE, factorVars = factorVars)
table1 %>% 
  print(nonnormal = c("Age", "Eo", "BUN", "RR", "Sbp")) %>% 
  write.csv(file = "table1.csv")
### Logistic regression 
dat_c <- dat %>% 
  na.omit()
dat_c <- dat_c %>% 
  mutate(Age = if_else(dat_c$Age > 65, 1, 0)) %>% 
  mutate(Eo = if_else(dat_c$Eo > 50, 1, 0)) %>% 
  mutate(BUN = if_else(dat_c$BUN > 19.6, 1, 0)) %>% 
  mutate(RR = if_else(dat_c$RR >= 30, 1, 0)) %>% 
  mutate(Sbp = if_else(dat_c$Sbp < 90, 1, 0))
dat_c <- dat_c %>% 
  mutate(across(Eo:RR, .fns = ~{as.logical(.)}))
dat_c %>% glimpse()
vars <- c("death", "intubation", "Age", "Eo", "BUN", "RR", "Sbp", "cons")
factorVars <- c("death", "intubation", "Age", "Eo", "BUN", "RR", "Sbp", "cons")
table2 <- CreateTableOne(vars = vars, data = dat_c, includeNA = TRUE, factorVars = factorVars)
table2 %>% 
  print(nonnormal = c("Age", "Eo", "BUN", "RR", "Sbp")) %>% 
  write.csv(file = "table2.csv")
fit <- glm(death ~ Age + Eo + BUN + RR + Sbp + cons, family = binomial, data = dat_c)
tidy(fit, exponentiate = TRUE, conf.int = TRUE)
modelfit(fit)
## Supplement
dat_supple <- dat %>% 
  na.omit()
fit <- glm(death ~ Age + Eo + BUN + RR + Sbp + cons, family = binomial, data = dat_supple)
## Model assumption
### Linearity assumption
prob <- predict(fit, type = "response")
pred.class <- ifelse(prob > 0.5, "positive", "negative")
pred.class %>% glimpse()
dat_n <- dat_supple %>% 
  select_if(is.numeric)
predictors <- colnames(dat_n)
dat_n <- dat_n %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
dat_n %>% glimpse()
ggplot(dat_n, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
### Influential values 
plot(fit, which = 4, id.n = 3)
model.data <- augment(fit) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = death), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)
### Multicolinearity  
vif(fit)
## Multiple imputation
demo <- mice(dat, maxit = 0)
demo$method
demo$predictorMatrix
prematrix <- (1 - diag(1, ncol(dat)))
prematrix[c(1,3),] <- prematrix[,c(1,3)] <- 0
prematrix
dat100 <- mice(dat, m = 100, maxit = 20, print = FALSE, predictorMatrix = prematrix, seed = 1116) #My birthday seed
plot(dat100)
dat_stack <- complete(dat100, action="long") %>% 
  as_tibble()
dat_stack %>% glimpse()
dat_stack <- dat_stack %>% 
  group_by(.imp) %>% 
  mutate(Age = if_else(Age > 65, 1, 0)) %>% 
  mutate(Eo = if_else(Eo > 50, 1, 0)) %>% 
  mutate(BUN = if_else(BUN > 19.6, 1, 0)) %>% 
  mutate(RR = if_else(RR >= 30, 1, 0)) %>% 
  mutate(Sbp = if_else(Sbp < 90, 1, 0)) %>% 
  mutate(across(death:RR, .fns = ~{as.logical(.)}))
dat_results <- dat_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~glm(death ~ Age + Eo + BUN + RR + Sbp + cons, family = binomial, data = .))) 
dat_results
combined_results <- MIcombine(dat_results$fit, call=NULL)
messy <- summary(combined_results)
exp(messy[, 1:4])