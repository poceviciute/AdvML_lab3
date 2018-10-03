## Question 1

set.seed(12345)
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


## Particle filter function
particles <- function(Obs, M,Iter, sdX=1, correction=TRUE){
    Xt <- Obs
    Z0 <- c()

    #Initialise particles for the latent variable
    for (m in 1:M){
     Z0[m]<- runif(1,0,100)
  }
    
    Zt <- matrix(ncol=Iter,nrow=M)
    wt <- matrix(ncol=Iter,nrow=M)
    Zt_bar <- matrix(ncol=Iter,nrow=M)
    #fill the initial time step
    Zt[,1] <- Z0
    # Add arbitrary values in order for the indexing to work later
    Zt_bar[,1] <- rep.int(2,M) 
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

# Create the model, sd=5
my_SSM_2a <- create_SSM(runif(1,0,100),100,sdX = 5)
# Filter particles, sd=5
results_q2a <- particles(my_SSM_2a$observations,100,100,sdX = 5)

# Create the model, sd=50
my_SSM_2b <- create_SSM(runif(1,0,100),100,sdX = 50)
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
hist(results_q2b$Zt[,1],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=1, sd=50")
abline(v=my_SSM_2b$sates[1],col="red")

hist(results_q2b$Zt[,35],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=35, sd=50")
abline(v=my_SSM_2b$sates[35],col="red")

hist(results_q2b$Zt[,75],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=75, sd=50")
abline(v=my_SSM_2b$sates[75],col="red")

hist(results_q2b$Zt[,100],freq = FALSE,xlab = "Zt",main="Distribution of particles, t=100, sd=50")
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

