
# ================ FROM WEB ========================
# https://rpubs.com/Jeet1994/Prey-predator-model
#
install.packages("deSolve")
library(deSolve)

# parameters
alpha = 1
beta = 0.2
delta = 0.5
gamma = 0.2

alpha=0.1
beta=1.01

#pars <- c(alpha = 1, beta = 0.2, delta = 0.5, gamma = 0.2)
pars <- c(alpha=alpha, beta=beta, delta = delta, gamma=gamma)

# initial state 
init <- c(x = 1, y = 2)
# times
times <- seq(0, 100, by = 1)

deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- alpha * x - beta * x * y
    d_y <- delta * beta * x * y - gamma * y
    return(list(c(x = d_x, y = d_y)))
  })
}

deriv2 <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- x*(1-y)
    d_y <- gamma/alpha * y * (x-1)	
    return(list(c(x = d_x, y = d_y)))
  })
  
  deriv3 <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- alpha * x - beta * x * y
    d_y <- delta * beta * x * y - gamma * y
    return(list(c(x = d_x, y = d_y)))
  })
}

}

coef1=gamma*beta/(delta*beta*alpha)


lv_results <- ode(init, times, deriv, pars)

lv_model <- function(pars, times = seq(0, 50, by = 1)) {
  # initial state 
  state <- c(x = 1, y = 2)
  # derivative
  deriv <- function(t, state, pars) {
    with(as.list(c(state, pars)), {
      d_x <- alpha * x - beta * x * y
      d_y <- delta * beta * x * y - gamma * y	
      return(list(c(x = d_x, y = d_y)))
    })
  }
  deriv2 <- function(t, state, pars) {
    with(as.list(c(state, pars)), {
      d_x <- x*(1-y)
      d_y <- gamma/alpha * y * (x-1)
      return(list(c(x = d_x, y = d_y)))
    })
  }
  # solve
  ode(y = state, times = times, func = deriv, parms = pars)
}

lv_results <- lv_model(pars = pars, times = seq(0, 1000, by = 0.25))

lv_results %>% 
  data.frame() %>% 
  gather(var, pop, -time) %>% 
  mutate(var = if_else(var == "x", "Prey", "Predator")) %>% 
  ggplot(aes(x = time, y = pop)) +
    geom_line(aes(color = var)) +
    scale_color_brewer(NULL, palette = "Set1") +
    labs(title = "Lotka-Volterra predator prey model",
         subtitle = paste(names(pars), pars, sep = " = ", collapse = "; "),
         x = "Time", y = "Population density")
		 
#rm(list = ls())
