# Smith, W. (2026). Modeling C. irus response to L. polyphyllus. GitHub. https://github.com/barilliumcode/frosted/blob/58d72a14c2ff1ffaebaa6787a0b94741a90dbff3/Model.R

# Used to look at 3 different scenarios:
# 1. Normal condition. Initial state = A population of 2,500 C. irus has 50000m^2 of L. perennis.
# End result = steady populations of C. irus and L. perennis.
# 2. L. polyphyllus invasion. Initial state = A population of 2,500 C. irus has 50000m^2 of...
# L. perennis and 10m^2 of L. polyphyllus. End result = L. polyphyllus outcompetes L. perennis...
# and the C. irus population plumets.
# 3. Only L. polyphyllus. Initial state = A population of 2,500 C. irus has 50000m^2 of L. polyphyllus.
# End result = C. irus populations drop to 0. 

# Adapted from https://strimas.com/post/lotka-volterra/

#---------- Loading packages
install.packages("tidyverse") #pivot_longer to reshape data
install.packages("deSolve") #ode
install.packages("ggplot2") #graphing
library(tidyverse)
library(deSolve)
library(ggplot2)

#---------- Key to PARAMETER language used below:
# S = sundial lupine; L = large leaf lupine; decay = exponential decay... 
# of butterfly in the absence of plants; benefit = benefit to butterfly of... 
# interaction w/ plant; growth = logistic growth of plants in absence of...
# butterflies; consumed = rate at which plant is consumed; k = carrying capacity

pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 2, Lconsumed = 0.000018)

#---------- Key to INITIAL STATE language used below:
# x = frosted elfin pop, y = sundial lupine pop, z = large leaf lupine pop
init <- c(x = 2500, y = 50000, z=10) #sets initial states for lv_results

time <- seq(0, 50, by = 1) #sets time sequence for lv_results

#---------- Equations for modeling lv_results
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results <- ode(init, time, deriv, pars)

class(lv_results) #pivot_ function doesnt work with matrix
lv_results=as.data.frame(lv_results) #convert to df
class(lv_results) #check if df now

long_df<-pivot_longer(lv_results,
                      cols=matches("x|y|z"),
                      names_to="Variable",
                      values_to="Value")

#---------- Graphing
ggplot(long_df, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,100000)+
  theme_classic()+
  labs(x="Time", y= "Populations")
