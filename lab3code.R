# Advanced machine learning

#Lab 3


#1
set.seed(987654321)
timestep <- 100
num_particles <- 100

transition_model <- function(initial_model, timestep){
  z_current <- vector(length=timestep)
  z_current[1] <- initial_model # initial model
  for(i in 1:(timestep+1)){
   z_current[i+1] <- (rnorm(n=1, mean=z_current[i], sd=1) + rnorm(n=1, mean=z_current[i]+1, sd=1) +rnorm(n=1, mean=z_current[i]+2, sd=1))/3
  }
  z_current[2:(timestep+1)]
}

z_states <- transition_model(runif(1,0,100), timestep)

emission_prob <- function(z, sd){
  (rnorm(n=1, mean=z, sd=sd) + rnorm(n=1, mean=z-1, sd=sd) + rnorm(n=1, mean=z+1, sd=sd))/3
}

emission_model <- function(timestep, z_states, sd=1){
  x_current <- vector(length=timestep)
  for(i in 1:timestep){
    x_current[i]<- emission_prob(z=z_states[i], sd=sd)
      #(rnorm(n=1, mean=z_states[i], sd=sd) + rnorm(n=1, mean=z_states[i]-1, sd=sd) + rnorm(n=1, mean=z_states[i]+1, sd=sd))/3
  }
  #x_current <- apply(z_states, 2, function(z){emission_prob(z, sd)})
  x_current
}


x_obs <- emission_model(timestep, z_states)


particle_filter <- function(x_obs, z_states, num_particles, timestep, sd, correction=TRUE){
  Z_bar <- matrix(ncol=num_particles, nrow=timestep+1) #particles -prediction
  #initialization
  Z_bar[1,] <- runif(num_particles, 0, 100) #Z0
  Z <- matrix(ncol=num_particles, nrow=timestep) #particles -correction
  w <- matrix(ncol = num_particles, nrow=timestep) #importance weights
  for(i in 1:timestep){
    for(j in 1:num_particles){
      Z_bar[i+1,j] <- transition_model(Z_bar[i,j],1) #transition?
      w[i,j] <- dnorm(x_obs[i], Z_bar[i+1,j], sd) #emission model
    }
    if(correction){
      Z[i,] <- sample(Z_bar[i+1,], size=num_particles, replace = TRUE, prob=w[i,])
    }
  }
  return(list("Z_bar"=Z_bar[-1,], "Z"=Z))
}

particles1<-particle_filter(x_obs, z_states, num_particles, timestep, sd=1)
exp_location <- rowMeans(particles1$Z)


#2

#sd=5 in emission
x_obs2 <- emission_model(timestep, z_states, sd = 5)
particles2 <- particle_filter(x_obs2, z_states, num_particles, timestep, sd=5)
exp_location2 <- rowMeans(particles2$Z)

#sd=50 in emission
x_obs3 <- emission_model(timestep, z_states, sd = 50)
particles3 <- particle_filter(x_obs3, z_states, num_particles, timestep, sd=50)
exp_location3 <- rowMeans(particles3$Z)


#3

particles4 <- particle_filter(x_obs, z_states, num_particles, timestep, sd=1, correction = FALSE)
exp_location4 <- rowMeans(particles4$Z_bar)


### Comparison of Questions 1,2,3:

what_time <- c(1, timestep*0.25, 0.75*timestep, timestep)
comp_location <- matrix(ncol = length(what_time), nrow=5)
comp_location[1,]<-z_states[what_time]
comp_location[2,]<-exp_location[what_time]
comp_location[3,]<-exp_location2[what_time]
comp_location[4,]<-exp_location3[what_time]
comp_location[5,]<-exp_location4[what_time]
colnames(comp_location) <- what_time
rownames(comp_location) <- c("True", "exp1", "exp2", "exp3", "exp4")
comp_location

plot(z_states, type="l", ylab="Position", xlab="Time step", 
     main="Comparison of true location and expected locations", ylim = c(20,170))
lines(exp_location, col="blue")
lines(exp_location2, col="green")
lines(exp_location3, col="red")
lines(exp_location4, col="purple")
legend("bottomright",c("true position","expected, sd=1","expected, sd=5","expected, sd=50","no correction"),
       col = c("black","blue","green","red","purple"), lty = c(3, 1, 1, 1, 1, 1), lwd=c(3, rep(2,5)))

plot(x_obs, pch=19)
lines(z_states, col="red")
lines(exp_location, col="blue")
lines(exp_location4, col="purple")
legend("bottomright",c("observation","true position","expected, sd=1","no correction"),
       col = c("black","red","blue","purple"), lty = c(3, 1, 1, 1), lwd=c(3, 2, 2, 2))

plot(x_obs2, pch=19)
lines(z_states, col="red")
lines(exp_location2, col="blue")
legend("bottomright",c("observation","true position","expected, sd=5"),
       col = c("black","red","blue"), lty = c(3, 1, 1), lwd=c(3, 2, 2))

plot(x_obs3, pch=19)
lines(z_states, col="red")
lines(exp_location3, col="blue")
legend("bottomright",c("observation","true position","expected, sd=50"),
       col = c("black","red","blue"), lty = c(3, 1, 1), lwd=c(3, 2, 2))

