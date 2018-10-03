## Question 1

# M - number of particals
# Iter - number of time steps T
# z is latent variable
# x is the observation

particles <- function(M,Iter){
    
    #Initialise particles for the latent variable
    Z0 <- c()
    X0 <- c()
    w0 <- c()
    for (m in 1:M){
     Z0[m]<- runif(1,0,100)
     # Inirial particals for observations
     X0[m] <- (rnorm(1,mean=Z0[m],sd=1)+rnorm(1,mean=(Z0[m]-1),sd=1)+rnorm(1,mean=(Z0[m]+1),sd=1))/3
     # Initial weights
     w0[m] <- (dnorm(X0[m],mean=Z0[m],sd=1)+dnorm(X0[m],mean=(Z0[m]-1),sd=1)+dnorm(X0[m],mean=(Z0[m]+1),sd=1))/3
    }
    
    Zt <- matrix(ncol=Iter,nrow=M)
    Xt <- matrix(ncol=Iter,nrow=M)
    wt <- matrix(ncol=Iter,nrow=M)
    Zt_bar <- matrix(ncol=Iter,nrow=M)
    #fill the t=0 step
    Zt[,1] <- Z0
    Zt_bar[,1] <- Z0
    Xt[,1] <- X0
    wt[,1] <- w0
    
    for (t in 2:Iter){
        # Prediction
        for (m in 1:M){
            # sample from the transition model (current Z_t depends on the Z_{t-1})
            Zt_bar[m,t] <- (rnorm(1,mean=Zt[m,t-1],sd=1)+rnorm(1,mean=(Zt[m,t-1]+1),sd=1)+rnorm(1,mean=(Zt[m,t-1]+2),sd=1))/3
            #sample from emission model (x depends on the current Z_t)
            Xt[m,t] <- (rnorm(1,mean=Zt_bar[m,t],sd=1)+rnorm(1,mean=(Zt_bar[m,t]-1),sd=1)+rnorm(1,mean=(Zt_bar[m,t]+1),sd=1))/3
            #Calculate weights
            wt[m,t] <- (dnorm(Xt[m,t],mean=Zt_bar[m,t],sd=1)+dnorm(Xt[m,t],mean=(Zt_bar[m,t]-1),sd=1)+dnorm(Xt[m,t],mean=(Zt_bar[m,t]+1),sd=1))/3
        }
        # Correction
            Zt[,t]<- sample(Zt_bar[,t], M, replace = TRUE, prob = wt[,t])
        
    }
    return(list(Zt=Zt,Zt_bar=Zt_bar,wt=wt))
    
}

a <- particles(10,5)
a$Xt[1,1]
hist(a$Zt[,2])

## Question 2