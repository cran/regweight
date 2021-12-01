## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5) 

## ----setup--------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)
library(regweight)
data("LaLonde", package = "CBPS")

df <- filter(LaLonde, (exper == 1 && treat == 1) || (exper == 0 && treat == 0))

model <- lm(
  log(re78 + 1) ~ treat + age + educ + black + hisp + married + nodegr + log(re74 + 1) + log(re75 + 1) + re74.miss, 
  df
)
summary(model)

## -----------------------------------------------------------------------------
rw_mod <- calculate_weights(model, "treat")
hist(rw_mod) + scale_x_continuous("Weight")

## -----------------------------------------------------------------------------
plot(rw_mod, df$married) + scale_x_continuous("Married", breaks = c(0,1))

## -----------------------------------------------------------------------------
plot(rw_mod, df$nodegr) + scale_x_continuous("No degree", breaks = c(0,1))

## -----------------------------------------------------------------------------
plot(rw_mod, df$black) + scale_x_continuous("Black", breaks = c(0,1))

## -----------------------------------------------------------------------------
plot(rw_mod, df$age) + scale_x_continuous("Age")

## -----------------------------------------------------------------------------
plot(rw_mod, df$re74) + scale_x_continuous("Income (1974)")

## ----fig.width = 6------------------------------------------------------------
state_shapes <- USAboundaries::us_states()
state_shapes <- filter(state_shapes, !(state_abbr %in% c("HI", "PR", "AK")))
# Necessary due to https://github.com/r-spatial/sf/issues/1419
suppressWarnings(st_crs(state_shapes) <- 4326)
pr_state <- seq(1, 10, length = nrow(state_shapes))
pr_state <- pr_state / sum(pr_state)
df$geometry <- sample(state_shapes$geometry, nrow(df), replace = TRUE, prob = pr_state)

plot(rw_mod, df$geometry)

## -----------------------------------------------------------------------------
summary(
  rw_mod,
  df %>% select(re74, re74.miss, age, married, nodegr),
  output = "html"
)

