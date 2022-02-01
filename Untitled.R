library(deSolve)
library(tidyverse)
library(gganimate)
library(gifski)

# install.packages("devtools")
devtools::install_github("thomasp85/transformr")

ode_func <-  function(t, state, parms){
  with(as.list(c(state, parms)),{
    dx <- a * x - b * x * y 
    dy <- d * x * y - g* y
    list(c(dx, dy))
  })  # end with(as.list ...
}
params <- c('a' = 1.1, 'b' = 0.4, 'd' = 0.1, 'g' = 0.4)
state1 <- c(x = 20, y = 10)
state2 <- c(x = 19, y = 10)
ts_pred <- seq(0, 1000, length.out=300)
x_error <- rnorm(length(ts_pred), 1, 0.8)
y_error <- rnorm(length(ts_pred), 1, 0.8)


sol_ode1 <- data.frame("pred1" = ode(y=state1 , times=ts_pred, func = ode_func, parms = params))%>%
  mutate(ratio = pred1.y,
         Prey = pred1.x,
         Time = pred1.time,
         tag = rep('sys1', 300) )%>%
  select(-contains('pred'))

sol_ode2 <- data.frame("pred2" = ode(y=state2 , times=ts_pred, func = ode_func, parms = params))%>%
  mutate(ratio = pred2.y,
         Prey = pred2.x,
         Time = pred2.time,
         tag = rep('sys2', 300))%>%
  select(-contains('pred'))

sol_ode <- rbind(sol_ode1, sol_ode2)

p2 <- ggplot(sol_ode) + 
 # geom_line(aes(x = Prey1, y = ratio1), col=2) +
  geom_line(aes(x = Prey, y = ratio, col=tag)) +
  scale_y_log10() +
  theme_bw() + theme(legend.position = c(0.9, 0.9), legend.background = element_blank()) +
  transition_reveal(Time)

animate(p2, renderer = gifski_renderer(), fps=20)

p2_df <- data.frame("Time"= seq(1, 20, length.out = 20), "pos" = seq(20, 400, length.out = 20))


p2 <- ggplot(p2_df, aes(x=Time, y=pos))+
  geom_line()+
  transition_reveal(Time)

anim_save('tr_gif.gif', animation = p2, path = "anim_plots")
