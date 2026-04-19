# Smith, W. (2026). Modeling C. irus response to L. polyphyllus. GitHub. https://github.com/barilliumcode/frosted/blob/58d72a14c2ff1ffaebaa6787a0b94741a90dbff3/Model.R

# Adapted from https://strimas.com/post/lotka-volterra/

#---------- Loading packages --------------------
install.packages("tidyverse") #pivot_longer to reshape data
install.packages("deSolve") #ode
install.packages("ggplot2") #graphing
install.packages("FME") #global sensitivity analysis
library(tidyverse)
library(deSolve)
library(ggplot2)
library(FME)

#---------- Key to PARAMETER language used below:
# S = sundial lupine; L = large leaf lupine; decay = exponential decay... 
# in the absence of plants; benefit = benefit to butterfly of... 
# interaction w/ plant; growth = logistic growth in absence of...
# butterflies; consumed = rate at which plant is consumed; 
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 74000, Lbenefit = 0.0000000167, Lgrowth = 1542.67, Lconsumed = 0.05)
#---------- Key to INITIAL STATE language used below:
# x = frosted elfin pop, y = sundial lupine pop, z = large leaf lupine pop
init <- c(x = 2500, y = 50000, z=10) #sets initial states for lv_results
time <- seq(0, 200, by = 1) #sets time sequence for lv_results
#---------- EQUATIONS for modeling lv_results --------------------
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results <- ode(init, time, deriv, pars)

#---------- GRAPHING lv_results --------------------
# change results from a wide format to long format for graphing
class(lv_results) #pivot_ function doesnt work with matrix
lv_results=as.data.frame(lv_results) #convert to df
class(lv_results) #check if df now
long_df<-pivot_longer(lv_results,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")
ggplot(long_df, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,3000)+
  theme_classic()+
  labs(x="Time", y= "Populations")
ggplot(long_df, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(45000,80000)+
  xlim(0,20)+
  theme_classic()+
  labs(x="Time", y= "Populations")

# ------------------- LOCAL SENSITIVITY ---------
#need to input model specifically to run FME global sens analysis later
lv_model <- function(pars, times = seq(0, 200, by = 1)){
  # initial state
  state <- c(x = 2500, y=50000, z=100)
  # derivative
  deriv <- function(t, state, pars) {
    with(as.list(c(state, pars)), {
      d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
      d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
      d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
      return(list(c(x = d_x, y = d_y, z = d_z)))})}
  # solve
  ode(y=state, times = times, func = deriv, parms = pars)
}
lv_results <- lv_model(pars = pars, times = seq(0, 50, by = 1))
lv_results
lv_sens_loc<-sensFun(lv_model, parms = pars)
summary(lv_sens_loc) #larger peaks = more influential the parameter to pop.
# pivot to plot with ggplot
lv_loc_long <- lv_sens_loc %>% 
  gather(parameter, sensitivity, -x, -var)
options(scipen = 999) #fixes y axis values
ggplot(lv_loc_long, aes(x = x, y = sensitivity)) +
  geom_line(aes(colour = parameter, linetype = var)) +
  scale_color_brewer("Parameter", palette = "Set1") +
  theme_classic()+
  labs(x = "Time", y = "Model Sensitivity") + 
  theme(legend.position = "bottom")

# ------------------- RUNNING SIMULATIONS ---------

# Used to look at 5 different scenarios:
# 1.) A population of C. irus with only SL present. 
# 2.) A population of C. irus with both SL and LL 
##### as non-zero numbers. 
# 3.) A population of C. irus with both SL and LL
##### as non-zero numbers, and LL has a smaller 
##### growth rate than SL.
# 4.) A population of C. irus with both SL and LL 
##### as non-zero numbers, and LL benefits the
##### butterfly more than observed in feeding exp.
# 5.) A population of C. irus with only LL present.

# Scenario 1 Normal Condition a.k.a. equilibrium
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 74000, Lbenefit = 0.0000000167, Lgrowth = 1542.67, Lconsumed = 0.05)
init <- c(x = 2500, y = 50000, z=0) #sets initial states for lv_results1
time <- seq(0, 200, by = 1) #sets time sequence for lv_results1
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results1 <- ode(init, time, deriv, pars)
class(lv_results1) #pivot_ function doesnt work with matrix
lv_results1=as.data.frame(lv_results1) #convert to df
class(lv_results1) #check if df now
long_df1<-pivot_longer(lv_results1,
                      cols=matches("x|y|z"),
                      names_to="Variable",
                      values_to="Value")
ggplot(long_df1, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(40000,60000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
ggplot(long_df1, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,3000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))

# Scenario 2 Introduced Condition
# (LL outcompetes + displaces SL, which causes FE pop. crash)
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 74000, Lbenefit = 0.0000000167, Lgrowth = 1542.67, Lconsumed = 0.05)
init <- c(x = 2500, y = 50000, z=10) #sets initial states for lv_results2
time <- seq(0, 200, by = 10) #sets time sequence for lv_results2
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results2 <- ode(init, time, deriv, pars)
class(lv_results2) #pivot_ function doesnt work with matrix
lv_results2=as.data.frame(lv_results2) #convert to df
class(lv_results2) #check if df now
long_df2<-pivot_longer(lv_results2,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")
ggplot(long_df2, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(40000,75000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
ggplot(long_df2, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,3000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))

# Scenario 3 Altered L growth 
# (if Lgrowth >386, it displaces SL)
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 74000, Lbenefit = 0.0000000167, Lgrowth = 500, Lconsumed = 0.05)
init <- c(x = 2500, y = 50000, z=10) #sets initial states for lv_results3
time <- seq(0, 200, by = 1) #sets time sequence for lv_results3
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results3 <- ode(init, time, deriv, pars)
class(lv_results3) #pivot_ function doesnt work with matrix
lv_results3=as.data.frame(lv_results3) #convert to df
class(lv_results3) #check if df now
long_df3<-pivot_longer(lv_results3,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")
ggplot(long_df3, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(5000,75000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
ggplot(long_df3, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,3000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))

# Scenario 4 Altered benefit from eating LL 
# (FE population persists even though SL decreases)
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 74000, Lbenefit = 0.0000001, Lgrowth = 1542.67, Lconsumed = 0.05)
init <- c(x = 2500, y = 50000, z=10) #sets initial states for lv_results4
time <- seq(0, 200, by = 1) #sets time sequence for lv_results4
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results4 <- ode(init, time, deriv, pars)
class(lv_results4) #pivot_ function doesnt work with matrix
lv_results4=as.data.frame(lv_results4) #convert to df
class(lv_results4) #check if df now
long_df4<-pivot_longer(lv_results4,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")
ggplot(long_df4, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(45000,75000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
ggplot(long_df4, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,6000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))

# Scenario 5 Inverse
# (if LL > 650k m^2, then FE population persists)
# z = 50k
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 74000, Lbenefit = 0.0000000167, Lgrowth = 1542.67, Lconsumed = 0.05)
init <- c(x = 2500, y = 0, z=50000) #sets initial states for lv_results5
time <- seq(0, 200, by = 1) #sets time sequence for lv_results5
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results5 <- ode(init, time, deriv, pars)
class(lv_results5) #pivot_ function doesnt work with matrix
lv_results5=as.data.frame(lv_results5) #convert to df
class(lv_results5) #check if df now
long_df5<-pivot_longer(lv_results5,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")
ggplot(long_df5, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(50000,75000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
ggplot(long_df5, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0000,2500)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
# z = 650k
pars <- c(decay = 0.01, Sbenefit = 0.0000002, Sgrowth = 1541.67, Sconsumed = 0.2, 
          k = 650000, Lbenefit = 0.0000000167, Lgrowth = 1542.67, Lconsumed = 0.05)
init <- c(x = 2500, y = 0, z=650000) #sets initial states for lv_results5
time <- seq(0, 200, by = 1) #sets time sequence for lv_results5
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results5 <- ode(init, time, deriv, pars)
class(lv_results5) #pivot_ function doesnt work with matrix
lv_results5=as.data.frame(lv_results5) #convert to df
class(lv_results5) #check if df now
long_df5<-pivot_longer(lv_results5,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")
options(scipen = 999) #fixes y axis values
ggplot(long_df5, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(5000,650000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
ggplot(long_df5, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,5000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Helvetica"))
