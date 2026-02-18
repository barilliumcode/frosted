# Smith, W. (2026). Modeling C. irus response to L. polyphyllus. GitHub. https://github.com/barilliumcode/frosted/blob/58d72a14c2ff1ffaebaa6787a0b94741a90dbff3/Model.R

# Used to look at 5 different scenarios:
# 1.) Normal condition: A population of C. irus with only L. perennis present. 
# 2.) Introduced condition: A population of C. irus with both L. perennis and 
##### L. polyphyllus as non-zero numbers. 
# 3.) Inverted condition: The normal condition is inverted. A population of 
##### C. irus now with only L. polyphyllus present. 
# 4.) Introduced benefits butterfly condition: A population of C. irus with 
##### both L. perennis and L. polyphyllus as non-zero numbers. Additionally, 
##### C. irus have adapted to benefit more from eating L. polyphyllus. 
# 5.) Competitive native condition: A population of C. irus with both 
##### L. perennis and L. polyphyllus as non-zero numbers. Additionally,  
##### L. perennis have adapted to better compete with L. polyphyllus.

# Adapted from Strimas-Mackey, M. (2020). Lotka-Volterra Predator Prey Model. https://strimas.com/post/lotka-volterra/

#---------- Loading packages --------------------
install.packages("tidyverse") #pivot_longer to reshape data
install.packages("deSolve") #ode
install.packages("ggplot2") #graphing
library(tidyverse)
library(deSolve)
library(ggplot2)

#---------- Key to PARAMETER language used below:
# S = sundial lupine; L = large leaf lupine; decay = exponential decay... 
# in the absence of plants; benefit = benefit to butterfly of... 
# interaction w/ plant; growth = logistic growth in absence of...
# butterflies; consumed = rate at which plant is consumed; 
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1.4, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 1.5, Lconsumed = 0.00018)

#---------- Key to INITIAL STATE language used below:
# x = frosted elfin pop, y = sundial lupine pop, z = large leaf lupine pop
init <- c(x = 2500, y = 50000, z=100) #sets initial states for lv_results
time <- seq(0, 500, by = 1) #sets time sequence for lv_results
#---------- EQUATIONS for modeling lv_results --------------------
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results <- ode(init, time, deriv, pars)
# change results from a wide format to long format for graphing
class(lv_results) #pivot_ function doesnt work with matrix
lv_results=as.data.frame(lv_results) #convert to df
class(lv_results) #check if df now
long_df<-pivot_longer(lv_results,
                      cols=matches("x|y|z"),
                      names_to="Variable",
                      values_to="Value")
#---------- GRAPHING --------------------
ggplot(long_df, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,100000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none")

# ------------------- 

# Scenario 1 Normal Condition
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 1.5, Lconsumed = 0.00018)
init <- c(x = 2500, y = 50000, z=0) #sets initial states for lv_results1
time <- seq(0, 50, by = 1) #sets time sequence for lv_results1
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

# Scenario 2 Introduced Condition
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 1.5, Lconsumed = 0.00018)
init <- c(x = 2500, y = 50000, z=100) #sets initial states for lv_results2
time <- seq(0, 50, by = 1) #sets time sequence for lv_results2
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

# Scenario 3 Inverted Condition
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 1.5, Lconsumed = 0.00018)
init <- c(x = 2500, y = 0, z=50000) #sets initial states for lv_results3
time <- seq(0, 50, by = 1) #sets time sequence for lv_results3
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

# Scenario 4 Introduced benefits butterfly condition
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000010, Lgrowth = 1.5, Lconsumed = 0.00018)
init <- c(x = 2500, y = 50000, z=100) #sets initial states for lv_results4
time <- seq(0, 50, by = 1) #sets time sequence for lv_results4
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

# Scenario 5 Competitive native condition
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1.4, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 1.5, Lconsumed = 0.00018)
init <- c(x = 2500, y = 50000, z=100) #sets initial states for lv_results5a
time <- seq(0, 50, by = 1) #sets time sequence for lv_results5a
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results5a <- ode(init, time, deriv, pars)
class(lv_results5a) #pivot_ function doesnt work with matrix
lv_results5a=as.data.frame(lv_results5a) #convert to df
class(lv_results5a) #check if df now
long_df5a<-pivot_longer(lv_results5a,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")

# Scenario 5 Competitive native condition **further time steps**
pars <- c(decay = 0.7, Sbenefit = 0.000014, Sgrowth = 1.4, Sconsumed = 0.00018, 
          k = 80000, Lbenefit = 0.000005, Lgrowth = 1.5, Lconsumed = 0.00018)
init <- c(x = 2500, y = 50000, z=100) #sets initial states for lv_results5b
time <- seq(0, 200, by = 1) #sets time sequence for lv_results5b
deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- -decay*x + Sbenefit*x*y+Lbenefit*x*z
    d_y <- Sgrowth*y*(1-((y+z)/k)) - Sconsumed*x*y
    d_z <- Lgrowth*z*(1-((y+z)/k)) - Lconsumed*x*z
    return(list(c(x = d_x, y = d_y, z = d_z)))})}
lv_results5b <- ode(init, time, deriv, pars)
class(lv_results5b) #pivot_ function doesnt work with matrix
lv_results5b=as.data.frame(lv_results5b) #convert to df
class(lv_results5b) #check if df now
long_df5b<-pivot_longer(lv_results5b,
                       cols=matches("x|y|z"),
                       names_to="Variable",
                       values_to="Value")

#---------- Graphing all scenarios together
par(mfrow=c(2,3)) #not working??
ggplot(long_df1, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,80000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Times New Roman"))
ggplot(long_df2, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,80000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Times New Roman"))
ggplot(long_df3, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,80000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Times New Roman"))
ggplot(long_df4, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,80000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Times New Roman"))
ggplot(long_df5a, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,80000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Times New Roman"))
ggplot(long_df5b, aes(time, Value, shape=Variable, colour=Variable))+
  geom_point()+
  ylim(0,80000)+
  theme_classic()+
  labs(x="Time", y= "Populations")+
  theme(legend.position="none", text = element_text(family = "Times New Roman"))
