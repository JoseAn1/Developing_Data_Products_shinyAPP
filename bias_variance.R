## Bias-Variance Trade off

#===============================================================================
# There is a general pattern in the plot of MSE's -Mean Squared Error- versus the
# model flexibility: U shape in the Testing set MSE and monotone decrease in the
# Training set MSE. This is a basic property in Statistical Learning.
#
# This script will try to show this property, for any data set or statistical
# method we choose. Additionally, we will be able to discover the reason of the
# Testing MSE U shape: The bias-variance trade off.
#
# In order to obtain the test MSE and its components,we will assume that the
# population features are knowed and we will be able to built the samples by
# simulation.
#
# References:
# 
# -Cap 2: & 2.2.2. "The Bias-Variance Trade-Off" from the book of Gareth James
#  et alter "An Introduction to Statistical Learning" Ed Springer 2014
#  http://www-bcf.usc.edu/~gareth/ISL/
#   
# -https://theclevermachine.wordpress.com/2013/04/21/
#  model-selection-underfitting-overfitting-and-the-bias-variance-tradeoff/
#===============================================================================

library(ggplot2);library(splines);library(dplyr);library(tidyr)
set.seed(1234)

# general values
 
p=0.6   # training/test ratio.
s=50    # simulations number.
k=12    # maximum value of the knots number. Spline is the statistical
        # algorithm for estimating the adjust function. 

# fdata function: generating  4 data set from the next inputs and assumptions:

# assumptions and definitions for the population:
# x : predictor variable: Uniform distribution 
# y : outcome variable: y = nu_cx + epsilon
# nu_cx : E(y|x)= f(x) conditional mean of y for each x, defined with f. 
# f: knowed true function: f=cos(x)
# epsilon: random error in y for each x.It is a normal distribution with mean =
# 0, and sd = sigma. epsilon -> N(0,sigma)

# y_hat: prediction for y = f_hat (x). f_hat obtained with the spline algorithm.

# inputs:
# n : sample size
# x_min: minimum value for x
# x_max: maximum value for x
# sigma: sd in the random error.



fdata<-function(n=100,x_min=-2,x_max=3,sigma=0.5){  # inputs
    x<-round(runif(n,x_min,x_max),3)    # sample x from uniform population
    t=round(0.6*n)                      # training size from the sample 


    
    # 1- Simulating k spline models ( j = 1 to k)
    training_km_sim<-vector("list",length=k)
    testing_km_sim<-vector("list",length=k)
    training_km<-vector("list",length=k)
    testing_km<-vector("list",length=k)
    for (j in 1:k){
        
        # simulating s samples ( i = 1 to s): for each one of the s simulations.
        y_hat_m<-matrix(nrow=t,ncol=s)                # matrix for train y_hat
        e2_m<-matrix(nrow=t,ncol=s)                   # matrix for train error2 
        
        y_hat_ts_m<-matrix(nrow=(n-t),ncol=s)         # matrix for test y_hat
        e2_ts_m<-matrix(nrow=(n-t),ncol=s)            # matrix for test error2 
        
        for (i in 1:s){
            # simulating data sets
            f<-function(x){cos(x)}                    # true function definition
            y<-round(f(x)+rnorm(n,0,sigma),3)         # sample y
            nu_cx<-round(f(x),3)                      # conditional mean
            samp<-data.frame(x,nu_cx,y)               # sample data frame
                 
            # splitting training and testing sets
            training<-samp[1:t,]
            testing<-samp[(t+1):n,]
            
            # adjusting training set with spline method for knot = j.
            fit<-lm(y~ns(x,j),training)             
            
            # predicting with fit. 
            
             # in training set
            y_hat_m[,i]<-predict(fit)                # training prediction                          
            e2_m[,i]<-(predict(fit)-training$y)^2    # squared error from
                                                     # training set
            
             # in testing set. 
            y_hat_ts_m[,i]<-predict(fit,newdata=testing)# testing prediction                    
            e2_ts_m[,i]<-(predict(fit,newdata=testing)-testing$y)^2# squared 
                                                    # error from testing set 
                                                       
        }
        # storing the results of the simulations in a list.
        training_km_sim[[j]]<-y_hat_m               # training prediction simul.
        testing_km_sim[[j]]<-y_hat_ts_m             # testing prediction simul.        
        
        
        # results for the whole of the simulation
        
          # in training set.
        training$y_hat<-apply(y_hat_m,1,mean)       # simulations mean for y_hat
        training$y_hat_var<-apply(y_hat_m,1,var)    # variance for the above
        training$e2<-apply(e2_m,1,mean)             # simulations mean for e2
        training$bias2<-(training$y_hat-training$nu_cx)^2# bias^2 of the y_hat 
        
          # testing set.
        testing$y_hat<-apply(y_hat_ts_m,1,mean)     # simulations mean for y_hat
        testing$y_hat_var<-apply(y_hat_ts_m,1,var)  # variance for the above.
        testing$e2<-apply(e2_ts_m,1,mean)           # simulations mean for e2
        testing$bias2<-(testing$y_hat-testing$nu_cx)^2  # bias^2 of the y_hat
        
        training_km[[j]]<-training
        testing_km[[j]]<-testing
    }
    

    # 2.-getting training mse and its components for each one of the k models.
    
    mse<-vector("numeric",k)
    y_hat_var<-vector("numeric",k)
    bias2<-vector("numeric",k)
    sigma2<-vector("numeric",k)
  
    for (j in 1:k){
        mse[j]<-mean(training_km[[j]]$e2)            # mean squared error (mse)
        y_hat_var[j]<-mean(testing_km[[j]]$y_hat_var)# variance
        bias2[j]<-mean(testing_km[[j]]$bias2)        # bias^2
        sigma2[j]<-sigma^2                           # sigma^2 
    }
    
     # storing the results.
    res<-data.frame(knots=1:k,mse,y_hat_var,bias2,sigma2)
    res$dif<-res$mse-(res$y_hat_var+bias2+sigma2)   # difference between mse 
                                                    # sum of the components  
    res_tr<-gather(res,comp,value,mse:sigma2)       # changing wide to long data.  
    
    
    # 3.-getting testing mse and its components for each one of the k models.
    
    mse<-vector("numeric",k)
    y_hat_var<-vector("numeric",k)
    bias2<-vector("numeric",k)
    sigma2<-vector("numeric",k)
    
    for (j in 1:k){
        mse[j]<-mean(testing_km[[j]]$e2)              # mean squared error (mse)
        y_hat_var[j]<-mean(testing_km[[j]]$y_hat_var) # variance
        bias2[j]<-mean(testing_km[[j]]$bias2)         # bias^2
        sigma2[j]<-sigma^2                            # sigma^2
    }
    
     # storing the results.
    res<-data.frame(knots=1:k,mse,y_hat_var,bias2,sigma2)
    res$dif<-res$mse-(res$y_hat_var+bias2+sigma2)   # difference between mse 
                                                    # sum of the components  
    res_ts<-gather(res,comp,value,mse:sigma2)       # changing wide to long data.

    
    # 4.- getting y_hat for each one of the k models.
    
      # in training set
    res_y_hat<-matrix(NA,nrow=t,ncol=k)
    for (j in 1:k){
        res_y_hat[,j]<-training_km[[j]][4]$y_hat
    }
    res_y_hat<-data.frame(res_y_hat)
    colnames(res_y_hat)<-c(1:k)
    res_y_hat<-cbind(x=training$x,res_y_hat)
    res_y_hat<-gather(res_y_hat,knots,y_hat,-x)    # changing wide to long data.

      # in testing set
    res_y_hat_ts<-matrix(NA,nrow=(n-t),ncol=k)
    for (j in 1:k){
        res_y_hat_ts[,j]<-testing_km[[j]][4]$y_hat
    }
    res_y_hat_ts<-data.frame(res_y_hat_ts)
    colnames(res_y_hat_ts)<-c(1:k)
    res_y_hat_ts<-cbind(x=testing$x,res_y_hat_ts)
    res_y_hat_ts<-gather(res_y_hat_ts,knots,y_hat,-x)#changing wide to long data.   
    
    # 5.- getting y_hat simulations for each one of the k models.
    
    # in training set
    
    res_y_hat_sim<-do.call(rbind.data.frame,training_km_sim)
    colnames(res_y_hat_sim)<-c(1:s)
    res_y_hat_sim$x<-res_y_hat$x
    res_y_hat_sim$knots<-res_y_hat$knots
    res_y_hat_sim<-gather(res_y_hat_sim,simul,y_hat,-c(x,knots))
    
    # in testing set
    
    res_y_hat_ts_sim<-do.call(rbind.data.frame,testing_km_sim)
    colnames(res_y_hat_ts_sim)<-c(1:s)
    res_y_hat_ts_sim$x<-res_y_hat_ts$x
    res_y_hat_ts_sim$knots<-res_y_hat_ts$knots
    res_y_hat_ts_sim<-gather(res_y_hat_ts_sim,simul,y_hat,-c(x,knots))   
    
    # 6.- getting the best j model: The one with lowest testing mse.
      kso<-res_ts[res_ts$value == min(res_ts[res_ts$comp == "mse",4]),1]    

# function results in a list.    
return(list(training_km_sim=training_km_sim,testing_km_sim=testing_km_sim,
            training_km=training_km,testing_km=testing_km,
            res_tr=res_tr,res_ts=res_ts,
            res_y_hat=res_y_hat,res_y_hat_ts=res_y_hat_ts,
            res_y_hat_sim=res_y_hat_sim,res_y_hat_ts_sim=res_y_hat_ts_sim,
            kso=kso))    
}    
    
# Making plots

# running the fdata function
dat<-fdata()


# fres function: generating plots from the data sets returned by fdata()

fres<-function(training_km_sim=dat$training_km_sim,
               testing_km_sim=dat$testing_km_sim, 
               training=dat$training_km[[1]],testing=dat$testing_km[[1]],
               res_tr=dat$res_tr,res_ts=dat$res_ts,
               res_y_hat=dat$res_y_hat,res_y_hat_ts=dat$res_y_hat_ts,
               res_y_hat_sim=dat$res_y_hat_sim,
               res_y_hat_ts_sim=dat$res_y_hat_ts_sim,
               ks=dat$kso){   

    # 1- true and estimate predictions for each one of the k models 
    
      # in training set
    
    # selecting a j model.
    # for the y_hat mean
    res_y_hat<-res_y_hat[res_y_hat$knots == ks,]             
    res_g<-training[,1:3]
    res_g$estim_func_y<-round(res_y_hat$y_hat,3)
    colnames(res_g)[2]<-"true_func_y"
    
    # for the y_hat simulation
    res_y_hat_sim<-res_y_hat_sim[res_y_hat_sim$knots == ks,] 
    
    # scatterplot  
    g1<-ggplot(res_g,aes(x,y))+geom_point(shape=1)+
        geom_line(aes(x=x,y=true_func_y,colour="true function"),size=1)+
        geom_line(aes(x=x,y=estim_func_y,colour="estimate function"),size=1)+
        geom_line(data=res_y_hat_sim,aes(x=x,y=y_hat,group=simul),
                  colour ="red",alpha=1/8)+
        ggtitle(paste("Training set. Spline model with knots:",ks,
                      "\nEstimate simulations in clear red lines"))
       
    
      # in testing set
    
    # selecting a j model.
    res_y_hat_ts<-res_y_hat_ts[res_y_hat_ts$knots == ks,] 
    res_g_ts<-testing[,1:3]
    res_g_ts$estim_func_y<-round(res_y_hat_ts$y_hat,3)
    colnames(res_g_ts)[2]<-"true_func_y"
    
    # for the y_hat simulation
    res_y_hat_ts_sim<-res_y_hat_ts_sim[res_y_hat_ts_sim$knots == ks,] 
    
    # scatterplot  
    g2<-ggplot(res_g_ts,aes(x,y))+geom_point(shape=1)+
        geom_line(aes(x=x,y=true_func_y,colour="true function"),size=1)+
        geom_line(aes(x=x,y=estim_func_y,colour="estimate function"),size=1)+
        geom_line(data=res_y_hat_ts_sim,aes(x=x,y=y_hat,group=simul),
                  colour ="red",alpha=1/8)+
        ggtitle(paste("Testing set. Spline model with knots:",ks))
    
    
    
    # Training and Testing MSE for each one of the k models
    
    res_1<-res_tr[res_tr$comp == "mse",];res_1$comp<-"mse Training"
    res_2<-res_ts[res_ts$comp == "mse",];res_2$comp<-"mse Testing"
    res_3<-res_ts[res_ts$comp == "sigma2",]
    res_tr_ts<-rbind(res_1,res_2,res_3)
    save(res_tr_ts,file="g3.Rdata")
    res_tr_ks<-res_tr_ts[res_tr_ts$knots == ks,4]
    g3<-ggplot(res_tr_ts,aes(x=knots,y=value,colour=comp))+geom_line()+
        geom_vline(aes(xintercept=ks),size=1)+
        ggtitle(paste("MSE (Mean Squared Error)\nknots:",ks,
                      ",    mse Training:",round(res_tr_ks[1],4),
                      "     mse Testing:",round(res_tr_ks[2],3)))+
        scale_x_continuous(breaks=seq(1,k,1))+ylab("mse")+
        xlab("Flexibility (knots)")
                      
    # Testing MSE components
    res_ts_ks<-res_ts[res_ts$knots == ks,4]
    g4<-ggplot(res_ts,aes(x=knots,y=value,colour=comp))+geom_line()+
        geom_vline(aes(xintercept=ks),size=1)+
        ggtitle(paste("Testing MSE components\nknots:",ks,
                  ", mse :",round(res_ts_ks[1],4),
                  "         sigma2:",round(res_ts_ks[4],3),
                  ", bias2:",round(res_ts_ks[3],3),
                  ", variance:",round(res_ts_ks[2],3),sep=""))+
        scale_x_continuous(breaks=seq(1,k,1))+ylab("mse and components")+
        xlab("Flexibility (knots)")
    
# function results in a list.
return(list(g1=g1,g2=g2,g3=g3,g4=g4,
            res_g=res_g,res_g_ts=res_g_ts))
}

# running the fres function
res<-fres()
# seeing the outputs

res$res_g
res$res_g_ts
res$g1
res$g2
res$g3
res$g4

