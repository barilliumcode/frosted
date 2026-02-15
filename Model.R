# modeled after https://strimas.com/post/lotka-volterra/

#---------- Loading packages
install.packages("tidyverse") #pivot_longer to reshape data
install.packages("deSolve") #ode
install.packages("ggplot2") #graphing
library(tidyverse)
library(deSolve)
library(ggplot2)
#------
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1.2, Sconsumed = 0.0001, 
          k = 80000, Lbenefit = 0.00002, Lgrowth = 1.1, Lconsumed = 0.0002)

# coexistence 
pars <- c(decay = 0.7, Sbenefit = 0.00004, Sgrowth = 1.5, Sconsumed = 0.00025, 
          k = 80000, Lbenefit = 0.00002, Lgrowth = 1.2, Lconsumed = 0.0002)
#------
#---------- Key to PARAMETER language used below:
# S = sundial lupine; L = large leaf lupine; decay = exponential decay... 
# in the absence of plants; benefit = benefit to butterfly of... 
# interaction w/ plant; growth = logistic growth in absence of...
# butterflies; consumed = rate at which plant is consumed; 

pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 2, Lconsumed = 0.000018)

#fitness of the plant if its eaten
#start with L as 0 to look at steady state to narrow in on s parameters, keep constant, then add L parameters
#and then what happens to the butterflies when just LL? 

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
  labs(x="Time", y= "Populations")+
  theme(legend.position="none")

