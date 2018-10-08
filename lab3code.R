# Advanced machine learning

#Lab 3


#1
#set.seed(987654321)
set.seed(12345)
timestep <- 100
num_particles <- 100
# 
# transition_model <- function(initial_model, timestep){
#   z_current <- vector(length=timestep)
#   z_current[1] <- initial_model # initial model
#   for(i in 1:(timestep+1)){
#     which_mean<-sample(1:3, size = 1)
#     z_current[i+1] <- rnorm(n=1, mean=z_current[i]+which_mean-1, sd=1)
#   }
#   z_current[2:(timestep+1)]
# }

# 
# emission_model <- function(timestep, z_states, sd=1){
#   x_current <- vector(length=timestep)
#   for(i in 1:timestep){
#     which_mean<-sample(1:3, size = 1)
#     x_current[i]<- rnorm(n=1, mean=z_states[i]+which_mean-2, sd=sd)
#   }
#   x_current
# }z_states <- transition_model(runif(1,0,100), timestep)


generate_xz <- function(timestep, sd){
  x_current <- vector(length=(timestep+1))
  x_current[1] <- 0
  z_current <- vector(length=(timestep+1))
  z_current[1] <- runif(1,0,100)
  for(i in 2:(timestep+1)){
    which_mean<-sample(1:3, size = 1)
    z_current[i] <- rnorm(n=1, mean=z_current[i-1]+which_mean-1, sd=1)
    x_current[i]<- rnorm(n=1, mean=z_current[i]+which_mean-2, sd=sd)
  }
  return(list("obs"=x_current[2:(timestep+1)], "states"=z_current[2:(timestep+1)]))
}

x_and_z <- generate_xz(timestep, sd=1)
x_obs <- x_and_z$obs
z_states <- x_and_z$states


particle_filter <- function(x_obs, num_particles, timestep, sd, correction=TRUE){
  Z_bar <- matrix(ncol=num_particles, nrow=timestep) #particles -prediction
  #initialization
  Z_bar[1,] <- runif(num_particles, 0, 100) #Z0
  Z <- matrix(ncol=num_particles, nrow=timestep) #particles -correction
  Z[1,] <- Z_bar[1,]
  w <- matrix(ncol = num_particles, nrow=timestep) #importance weights
  w[1,] <- rep(1,num_particles)
  for(i in 2:timestep){
    for(j in 1:num_particles){
      which_mean<-sample(1:3, size = 1)
      Z_bar[i,j] <- rnorm(n=1, mean=Z[i-1,j]+which_mean-1, sd=1)
        #transition_model(Z_bar[i,j],1) #transition?
      w[i,j] <- (dnorm(x_obs[i], Z_bar[i,j], sd)+dnorm(x_obs[i], Z_bar[i,j]-1, sd)+dnorm(x_obs[i], Z_bar[i,j]+1, sd))/3 #emission model
    }
    if(correction){
      Z[i,] <- sample(Z_bar[i,], size=num_particles, replace = TRUE, prob=w[i,])
    }
    else{
      Z[i,] <- Z_bar[i,]
    }
  }
  #return(list("Z_bar"=Z_bar[-1,], "Z"=Z))
  Z
}

particles1<-particle_filter(x_obs, num_particles, timestep, sd=1)
exp_location <- rowMeans(particles1)


#2

#sd=5 in emission
x_and_z2 <- generate_xz(timestep, sd=5)
x_obs2 <- x_and_z2$obs
z_states2 <- x_and_z2$states
particles2 <- particle_filter(x_obs2, num_particles, timestep, sd=5)
exp_location2 <- rowMeans(particles2)

#sd=50 in emission
x_and_z3 <- generate_xz(timestep, sd = 50)
x_obs3 <- x_and_z3$obs
z_states3 <- x_and_z3$states
particles3 <- particle_filter(x_obs3, num_particles, timestep, sd=50)
exp_location3 <- rowMeans(particles3)


#3

particles4 <- particle_filter(x_obs, num_particles, timestep, sd=1, correction = FALSE)
exp_location4 <- rowMeans(particles4)


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

z_states <- x_and_z$states

# plot(z_states, type="l", ylab="Position", xlab="Time step", 
#      main="Comparison of true location and expected locations", ylim = c(20,170))
# lines(exp_location, col="blue")
# lines(exp_location2, col="green")
# lines(exp_location3, col="red")
# lines(exp_location4, col="purple")
# legend("bottomright",c("true position","expected, sd=1","expected, sd=5","expected, sd=50","no correction"),
#        col = c("black","blue","green","red","purple"), lty = c(3, 1, 1, 1, 1, 1), lwd=c(3, rep(2,5)))

plot(x_obs, pch=19, main="sd=1", xlab="Time step", ylab="Position")
lines(z_states, col="red")
lines(exp_location, col="blue")
lines(exp_location4, col="purple")
legend("bottomright",c("observation","true position","expected, sd=1","no correction"),
       col = c("black","red","blue","purple"), lty = c(3, 1, 1, 1), lwd=c(3, 2, 2, 2))

# plot(z_states-exp_location)
# plot(z_states-exp_location2)
# plot(z_states-exp_location3)
# plot(z_states-exp_location4)

plot(x_obs2, pch=19, main="sd=5", xlab="Time step", ylab="Position")
lines(z_states2, col="red")
lines(exp_location2, col="blue")
legend("bottomright",c("observation","true position","expected, sd=5"),
       col = c("black","red","blue"), lty = c(3, 1, 1), lwd=c(3, 2, 2))

plot(x_obs3, pch=19, main="sd=50", xlab="Time step", ylab="Position")
lines(z_states3, col="red")
lines(exp_location3, col="blue")
legend("bottomright",c("observation","true position","expected, sd=50"),
       col = c("black","red","blue"), lty = c(3, 1, 1), lwd=c(3, 2, 2))

