## Question 1

set.seed(987654321)
# M - number of particals
# Iter - number of time steps T
# z is latent variable
# x is the observation

## Function that creates the SSM model
create_SSM <- function(z0,TT, sdX=1){
    Zt <- c(z0)
    Xt <- c((rnorm(1,mean=z0,sd=1)+rnorm(1,mean=(z0-1),sd=1)+rnorm(1,mean=(z0+1),sd=1))/3)
    
    for (i in 2:TT){
        Zt[i] <- (rnorm(1,mean=Zt[i-1],sd=1)+rnorm(1,mean=(Zt[i-1]+1),sd=1)+rnorm(1,mean=(Zt[i-1]+2),sd=1))/3 
        Xt[i] <- c((rnorm(1,mean=Zt[i],sd=sdX)+rnorm(1,mean=(Zt[i]-1),sd=sdX)+rnorm(1,mean=(Zt[i]+1),sd=sdX))/3)
    }
    return(list(sates=Zt,observations=Xt))   
    
}

generateX <- function(Zt, TT, sdX=1){
  Xt <- c()
  for (i in 1:TT){
    Xt[i] <- c((rnorm(1,mean=Zt[i],sd=sdX)+rnorm(1,mean=(Zt[i]-1),sd=sdX)+rnorm(1,mean=(Zt[i]+1),sd=sdX))/3)
  }
  return(Xt)
}

## Particle filter function
particles <- function(Obs, M,Iter, sdX=1, correction=TRUE){
    Xt <- Obs

    #Initialise particles for the latent variable
    Zt <- matrix(ncol=Iter,nrow=M)
    wt <- matrix(ncol=Iter,nrow=M)
    Zt_bar <- matrix(ncol=Iter,nrow=M)
    #fill the initial time step
    Zt[,1] <- runif(M,0,100)
    # Add arbitrary values in order for the indexing to work later
    Zt_bar[,1] <- rep.int(1,M) 
    wt[,1] <- rep.int(1,M)

    for (t in 2:Iter){
        # Prediction
        for (m in 1:M){
            # sample from the transition model (current Z_t depends on the Z_{t-1})
            Zt_bar[m,t] <- (rnorm(1,mean=Zt[m,t-1],sd=1)+rnorm(1,mean=(Zt[m,t-1]+1),sd=1)+rnorm(1,mean=(Zt[m,t-1]+2),sd=1))/3
            #Calculate weights
            wt[m,t] <- (dnorm(Xt[t],mean=Zt_bar[m,t],sd=sdX)+dnorm(Xt[t],mean=(Zt_bar[m,t]-1),sd=sdX)+dnorm(Xt[t],mean=(Zt_bar[m,t]+sdX),sd=1))/3
        }
        # Correction
        if (correction){
            Zt[,t]<- sample(Zt_bar[,t], M, replace = TRUE, prob = wt[,t])
        }else{
            Zt[,t] <- Zt_bar[,t]
        }
        
    }
    return(list(Zt=Zt,Zt_bar=Zt_bar,wt=wt))
    
}

# Create the model
my_SSM <- create_SSM(runif(1,0,100),100)

# Filter particles
results_q1 <- particles(my_SSM$observations,100,100)

#dim(results_q1$Zt)

hist(results_q1$Zt[,1],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=1")
abline(v=my_SSM$sates[1],col="red")

hist(results_q1$Zt[,35],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=35")
abline(v=my_SSM$sates[35],col="red")

hist(results_q1$Zt[,75],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=75")
abline(v=my_SSM$sates[75],col="red")

hist(results_q1$Zt[,100],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=100")
abline(v=my_SSM$sates[100],col="red")



## Question2

# Create observations for the model with sd=5
my_SSM_2a <-list()
my_SSM_2a$sates <- my_SSM$sates
my_SSM_2a$observations <- generateX(my_SSM$sates,100,sdX = 5)
# Filter particles, sd=5
results_q2a <- particles(my_SSM_2a$observations,100,100,sdX = 5)

# Create the model, sd=50
my_SSM_2b <-list()
my_SSM_2b$sates <- my_SSM$sates
my_SSM_2b$observations <- generateX(my_SSM$sates,100,sdX = 50)
# Filter particles, sd=50
results_q2b <- particles(my_SSM_2b$observations,100,100,sdX = 50)

my_SSM_2b$sates[1:5]
my_SSM_2b$observations[1:5]

results_q2b$Zt_bar[,1:5]
results_q2b$wt[,1:5]
results_q2b$Zt[,1:5]

# Plot results from 2a, sd =5
hist(results_q2a$Zt[,1],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=1, sd=5")
abline(v=my_SSM_2a$sates[1],col="red")

hist(results_q2a$Zt[,35],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=35, sd=5")
abline(v=my_SSM_2a$sates[35],col="red")

hist(results_q2a$Zt[,75],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=75, sd=5")
abline(v=my_SSM_2a$sates[75],col="red")

hist(results_q2a$Zt[,100],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=100, sd=5")
abline(v=my_SSM_2a$sates[100],col="red")


# Plot results from 2b, sd =50
hist(results_q2b$Zt[,1],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=1, sd=50",xlim=c(min(min(results_q2b$Zt[,1],my_SSM_2b$sates[1]))-2,max(max(results_q2b$Zt[,1]),my_SSM_2b$sates[1])+2))
abline(v=my_SSM_2b$sates[1],col="red")

hist(results_q2b$Zt[,35],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=35, sd=50",
     xlim=c(min(min(results_q2b$Zt[,35],my_SSM_2b$sates[35]))-2,max(max(results_q2b$Zt[,35]),my_SSM_2b$sates[35])+2))
abline(v=my_SSM_2b$sates[35],col="red")

hist(results_q2b$Zt[,75],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=75, sd=50",
     xlim=c(min(min(results_q2b$Zt[,75],my_SSM_2b$sates[75]))-2,max(max(results_q2b$Zt[,75]),my_SSM_2b$sates[75])+2))
abline(v=my_SSM_2b$sates[75],col="red")

hist(results_q2b$Zt[,100],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=100, sd=50",
     xlim=c(min(min(results_q2b$Zt[,100],my_SSM_2b$sates[100]))-2,max(max(results_q2b$Zt[,100]),my_SSM_2b$sates[100])+2))
abline(v=my_SSM_2b$sates[100],col="red")

## Question 3

# Filter particles
results_q3 <- particles(my_SSM$observations,100,100,correction = FALSE)


hist(results_q3$Zt[,1],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=1, w=1")
abline(v=my_SSM$sates[1],col="red")

hist(results_q3$Zt[,35],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=35, w=1")
abline(v=my_SSM$sates[35],col="red")

hist(results_q3$Zt[,75],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=75, w=1")
abline(v=my_SSM$sates[75],col="red")

hist(results_q3$Zt[,100],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=100, w=1")
abline(v=my_SSM$sates[100],col="red")


################################

exp_loc_q1 <- colMeans(results_q1$Zt)
exp_loc_q2a <- colMeans(results_q2a$Zt)
exp_loc_q2b <- colMeans(results_q2b$Zt)
exp_loc_q3 <- colMeans(results_q3$Zt)


plot(my_SSM$sates, col="black", type="l",ylab="Position", xlab="Time step", 
     main="Comparison of observations, true location, expected location", ylim = c(20,170))
lines(exp_loc_q1, col="blue")
lines(exp_loc_q2a, col="green")
lines(exp_loc_q2b, col="grey")
lines(exp_loc_q3, col="purple")
legend("bottomright",c("true position","expected, sd=1","expected, sd=5","expected, sd=50","no correction"),
       col = c("black","blue","green","grey","purple"), lty = c(1, 1, 1, 1, 1), lwd=c(rep(2,5)))


plot(my_SSM$observations, pch=20, ylab="Position", xlab="Time step", 
     main="Comparison of observations, true location, expected location", ylim = c(20,170))
lines(my_SSM$sates, col="red")
lines(exp_loc_q1, col="blue")
legend("bottomright",c("observations","true position","expected, sd=1"),
       col = c("black","red","blue"), lty = c(3,1, 1), lwd=c(3,rep(2,2)))


plot(my_SSM_2a$observations, pch=20, ylab="Position", xlab="Time step", 
     main="Comparison of observations, true location, expected location", ylim = c(20,170))
lines(my_SSM$sates, col="red")
lines(exp_loc_q2a, col="blue")
legend("bottomright",c("observations","true position","expected, sd=5"),
       col = c("black","red","blue"), lty = c(3,1, 1), lwd=c(3,rep(2,2)))

plot(my_SSM_2b$observations, pch=20, ylab="Position", xlab="Time step", 
     main="Comparison of observations, true location, expected location", ylim = c(20,170))
lines(my_SSM$sates, col="red")
lines(exp_loc_q2b, col="blue")
legend("bottomright",c("observations","true position","expected, sd=50"),
       col = c("black","red","blue"), lty = c(3,1, 1), lwd=c(3,rep(2,2)))


