########
## .r ##
########

## goal: perform Bayesian data analysis of stream lake dataset 
##       to find relationship between temperature and DBO on network structure

## author: Willem Bonnaffe (w.bonnaffe@gmail.com)

## update log:
## 20-03-2022 - created v0_0
## 27-03-2022 - created v0_1
##            - created simple linear model
## 12-04-2022 - created v0_2
##            - implemened inference of missing values
## 05-05-2022 - created v0_3_1
##            - implemented random effects (variances) for different hydrographic regions
##            - added altitude as fixed effect
## 09-05-2022 - renamed file model 4 v0_1
##            - added storage of chains
## 18-06-2022 - created v0_2
##            - implemented adaptive monte-carlo instead of differential-evolution monte-carlo
## 20-05-2022 - renamed file m_avgTL_v0_0
##            - created v0_1
##            - cleaned code
## 02-07-2022 - renamed file m_conne_v0_1
##            - created v0_2
##            - added species richness and interaction between temperature and DBO
##            - included connectance as response
## 04-07-2022 - created v0_3
##            - added assessment of spatial autocorrelation
## 05-07-2022 - used log normal to model missing variance prior
## 07-07-2022 - renamed model_1
##            - model selection => removed temp*dbo
##            - model selection => removed dbo^2


###############
## FUNCTIONS ##
###############

## std function
std = function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T)

#
###

##############
## INITIATE ##
##############

## library
library(Rcpp)
sourceCpp("cpp/DEMCpp_v0.1.cpp")
source("f_AMC_v0_2.r")
source("f_HBM_v0_6.r")

## load dataset
load("data/dataset_lake_stream_v0_1.rda")
data = dataset_lake_stream

## path to out
pto = "out_conne_model_2"
system(paste("mkdir ",pto,sep=""))

## response
response = "Connectance"

#
###

############
## MODEL  ##
############

## goal: infer missing values in addition to the linear model

## remove missing hydrographic bassin
data = data[-which(is.na(data$CdBH)),]

## response variable
# Y = data$max_troph_lvl
# Y = data$w_trph_lvl_avg
Y = std(data$connectance)

## explanatory variables
year    = std(data$year)
temp    = std(data$temp)
dbo     = std(data$dbo)
alt     = std(data$altitude)
type    = (data$type == "lake")*1
rich    = std(data$richness)
long    = data$long
latt    = data$lat

## marix of explanatory variables
X_obs   = cbind(1,year,temp,temp^2,type,type*temp,dbo,type*dbo,alt,rich)
X_mis   = cbind(1,year,temp,temp^2,type,type*temp,  1,type*1  ,alt,rich)
n_beta  = ncol(X_obs)

## missing values
idx_mis = which(is.na(dbo))
n_mis   = length(idx_mis)

## uneven variances (random effects)
stringToInteger = function(string)
{
	tmp = 1:length(unique(string))
	names(tmp) = unique(string)
	tmp = tmp[string]
	return(tmp)
}
n_sd_lik       = length(unique(data$CdBH))
idx_sd_lik_obs = stringToInteger(data$CdBH[-idx_mis])
idx_sd_lik_mis = stringToInteger(data$CdBH[idx_mis])

## splitting data into observed and missing
Y_obs   = Y[-idx_mis]
X_obs   = X_obs[-idx_mis,]
Y_mis   = Y[idx_mis]
X_mis   = X_mis[idx_mis,]

## predictive model
Yhat = function(X,beta) X%*%beta

## likelihood of response given observed variables
logLik_obs = function(Y_obs,X_obs,beta,sd_lik)
{
    return(sum(dnorm(Y_obs,Yhat(X_obs,beta),sd_lik[idx_sd_lik_obs],log=T)))
}

## likelihood of response given missing variables
logLik_mis = function(Y_mis,X_mis,x_mis,beta,sd_lik)      
{
    X_mis[,7:8] = X_mis[,7:8] * cbind(x_mis,x_mis)
    logLik_mis  = sum(dnorm(Y_mis,Yhat(X_mis,beta),sd_lik[idx_sd_lik_mis],log=T))
    return(logLik_mis)
}

## probability density of missing variables
logLat_mis = function(x_mis,mu_mis,sd_mis)
{
	return(sum(dnorm(x_mis,mu_mis,sd_mis,log=T)))
}

## log priors
logPri     = function(beta,sd_lik,mu_mis,sd_mis) 
{
    logPri = sum(dnorm(beta,       0,10,log=T)) + 
             sum(dnorm(log(sd_lik),0,1, log=T)) + 
                 dnorm(mu_mis,     0,.1, log=T)  + 
                 dnorm(log(sd_mis),0,.1, log=T) 
    return(logPri)
}

## posterior density distribution
logPos     = function(Y_obs,X_obs,Y_mis,X_mis,x_mis,beta,sd_lik,mu_mis,sd_mis)
{  
    logPos = logLik_obs(Y_obs,X_obs,beta,sd_lik) + 
             logLik_mis(Y_mis,X_mis,x_mis,beta,sd_lik) + 
             logLat_mis(x_mis,mu_mis,sd_mis) + 
             logPri(beta,sd_lik,mu_mis,sd_mis)
    return(logPos)
}

## wrappers
logPosWrap  = function(omega) 
{
	sd_lik  = omega[1:n_sd_lik]
	sd_mis  = omega[n_sd_lik + 1:1]
	mu_mis  = omega[n_sd_lik + 1 + 1:1]
	beta    = omega[n_sd_lik + 1 + 1 + 1:n_beta] 
	x_mis   = omega[n_sd_lik + 1 + 1 + n_beta + 1:n_mis] 
	return(logPos(Y_obs,X_obs,Y_mis,X_mis,x_mis,beta,sd_lik,mu_mis,sd_mis))
}
wrap       = function(omega) c(log(omega[1:(n_sd_lik + 1)]),omega[-c(1:(n_sd_lik + 1))])
unwrap     = function(omega) c(exp(omega[1:(n_sd_lik + 1)]),omega[-c(1:(n_sd_lik + 1))])
dTarget    = function(omega) logPosWrap(unwrap(omega))

#
###

###########
## TRAIN ##
###########

## optimisation
chain = NULL
for(i in 1:3)
{
	omega_0        = rnorm(n_sd_lik + 2 + n_beta + n_mis,0,0.1)
    res            = optim(par=omega_0,fn=function(x)-dTarget(x),method="BFGS",control=list(trace=TRUE,maxit=100)) 
    chain          = rbind(chain,c(res$value,unwrap(res$par)))
}

## MaP
omega_map = wrap(chain[which.min(chain[,1]),-1])
# load("out/model_4_maxTL_chain.RData")
# omega_map = wrap(chainList.argmaxPost(chainList))

## pilot chain
chainList = list()
for(i in 1:1)
{
	omega_0        = omega_map # rnorm(n_sd_lik + 2 + n_beta + n_mis,0,.1)
	chain          = DEMCpp(list("dTarget" = dTarget, "Theta_0" = omega_0, "epsilon" = 0.001, "nIt" = 5000000))$chainList
    # chain          = AMC(dTarget   = dTarget,
    #                  Theta_0   = omega_0,
    #                  scale_p_0 = 4*2.38/sqrt(length(omega_0)),
    #                  Sigma_p_0 = diag(1,length(omega_0)),
    #                  itVect    =  c(10000,100000),
    #                  adaptShape= T,
    #                  msg       = T)[-1,]
	chain[,-1]     = t(apply(chain[,-1],1,unwrap))
	chainList[[i]] = chain
}

## burn and thin
chainList_thinned = chainList.thin(chainList.burn(chainList,1:3000000))
save(file=paste(pto,"/chain.RData",sep=""),chainList_thinned)
 
#
###

#############
## FIGURES ##
#############

## goal:

## load chain
load(paste(pto,"/chain.RData",sep=""))

## 
# pdf(paste(pto,"/results.pdf",sep=""))

## PLOT PREDICTIONS TEMPERATURE ##
png(paste(pto,"/fig_1.png",sep=""))
#
plot(X_obs[,3],Y_obs,pch=16,col=adjustcolor("black",alpha=0.5),xlab="Temperature (SU)",ylab=response,main=paste(response," ~ temperature",sep=""))
for(i in 1:2)
{
    ## predictions
    x        = seq(min(temp),max(temp),0.1)
    X_pred   = cbind(1,0,x,x^2,(i-1),(i-1)*x,0,0,0,0)
    pred     = chainList.apply(chainList_thinned,function(x_) Yhat(X_pred,x_[-1][n_sd_lik + 1 + 1 + 1:n_beta]))
    polygon(x=c(X_pred[,3],rev(X_pred[,3])),y=c(pred$f_q0.05,rev(pred$f_q0.95)),border=NA,col=adjustcolor(i+1,alpha=0.5))
    # lines(X_pred[,3],Yhat(X_pred,beta_map),col=i+1)
    lines(X_pred[,3],pred$f_mean,col=i+1)
}
#
legend("topright",legend=c("Stream","Lake"),lty=1,col=1:2+1,bty="n")
#
dev.off()

## PLOT PREDICTIONS TEMPERATURE ##
png(paste(pto,"/fig_2.png",sep=""))
#
plot(X_obs[,7],Y_obs,pch=16,col=adjustcolor("black",alpha=0.5),xlab="DBO (SU)",ylab=response,main=paste(response," ~ DBO",sep=""))
for(i in 1:2)
{
    ## predictions
    x        = seq(min(dbo,na.rm=T),max(dbo,na.rm=T),0.1)
    X_pred   = cbind(1,0,0,0,(i-1),0,x,(i-1)*x,0,0)
    pred     = chainList.apply(chainList_thinned,function(x_) Yhat(X_pred,x_[-1][n_sd_lik + 1 + 1 + 1:n_beta]))
    polygon(x=c(X_pred[,7],rev(X_pred[,7])),y=c(pred$f_q0.05,rev(pred$f_q0.95)),border=NA,col=adjustcolor(i+1,alpha=0.5))
    # lines(X_pred[,7],Yhat(X_pred,beta_map),col=i+1)
    lines(X_pred[,7],pred$f_mean,col=i+1)
}
#
legend("topright",legend=c("Stream","Lake"),lty=1,col=1:2+1,bty="n")
#
dev.off()

## VISUALISE MISSING VS OBSERVED DBO ##
png(paste(pto,"/fig_3.png",sep=""))
#
x = density(dbo,na.rm=T)$x
y = density(dbo,na.rm=T)$y; y=y/max(y)
plot(x,y,type="l",col="white",xlab="DBO (SU)",ylab="Density (SU)",main=paste(response," ~ DBO distribution",sep=""))
polygon(x=c(x,rev(x)),y=c(rep(0,length(y)),rev(y)),col=adjustcolor("blue",0.4),border=NA)
#
dbo_mis = chainList.argmaxPost(chainList_thinned)[n_sd_lik + 1 + 1 + n_beta + 1:n_mis]
x = density(dbo_mis,na.rm=T)$x
y = density(dbo_mis,na.rm=T)$y; y=y/max(y)
polygon(x=c(x,rev(x)),y=c(rep(0,length(y)),rev(y)),col=adjustcolor("red",0.4),border=NA)
#
legend("topright",legend=c("Observed","Missing"),col=adjustcolor(c("blue","red"),0.4),lty=1,bty="n")
#
dev.off()

## VERIFY MODEL ASSUMPTIONS ##
x_mis_      = apply(chainList_thinned[[1]][,-1][,n_sd_lik + 1 + 1 + n_beta + 1:n_mis],2,mean)
X_mis_      = X_mis
X_mis_[,7:8] = X_mis_[,7:8] * cbind(x_mis_,x_mis_)
chainList_  = list(chainList_thinned[[1]][,-1][,n_sd_lik + 1 + 1 + 1:n_beta])
Yhat_obs    = chainList.apply(chainList_,function(x)Yhat(X_obs,x))$f_mean
Yhat_mis    = chainList.apply(chainList_,function(x)Yhat(X_mis_,x))$f_mean
res_obs     = Y_obs - Yhat_obs
res_mis     = Y_mis - Yhat_mis

## HISTOGRAM OF RESIDUALS ##
png(paste(pto,"/fig_4.png",sep=""))
#
x = density(res_obs,na.rm=T)$x
y = density(res_obs,na.rm=T)$y; y=y/max(y)
plot(x,y,type="l",col="white",xlab="Residuals",ylab="Density (SU)",main=paste(response," ~ residuals",sep=""))
polygon(x=c(x,rev(x)),y=c(rep(0,length(y)),rev(y)),col=adjustcolor("blue",0.4),border=NA)
#
x = density(res_mis,na.rm=T)$x
y = density(res_mis,na.rm=T)$y; y=y/max(y)
polygon(x=c(x,rev(x)),y=c(rep(0,length(y)),rev(y)),col=adjustcolor("red",0.4),border=NA)
#
legend("topright",legend=c("Observed","Missing"),col=adjustcolor(c("blue","red"),0.4),lty=1,bty="n")
#
dev.off()

## QQ plot
png(paste(pto,"/fig_5.png",sep=""))
#
sdVect = apply(chainList_thinned[[1]][,-1][,1:n_sd_lik],2,mean)
par(mfrow=c(3,3))
for(i in 1:n_sd_lik)
{
    x_mis_      = apply(chainList_thinned[[1]][,-1][,n_sd_lik + 1 + 1 + n_beta + 1:n_mis],2,mean)
    X_mis_      = X_mis
    X_mis_[,7:8] = X_mis_[,7:8] * cbind(x_mis_,x_mis_)
    chainList_  = list(chainList_thinned[[1]][,-1][,n_sd_lik + 1 + 1 + 1:n_beta])
    Yhat_obs    = chainList.apply(chainList_,function(x)Yhat(X_obs,x))$f_mean
    Yhat_mis    = chainList.apply(chainList_,function(x)Yhat(X_mis_,x))$f_mean
    res_obs     = Y_obs - Yhat_obs
    res_mis     = Y_mis - Yhat_mis
    #
    res_obs_th  = rnorm(length(res_obs),0,sdVect[i])
    res_mis_th  = rnorm(length(res_mis),0,sdVect[i])
    #
    plot(-1:1,xlim=c(-1,1)*4*sd(res_obs_th),ylim=c(-1,1)*4*sd(res_obs),xlab="Theoretical quantiles",ylab="Residuals",main=paste(response," ~ bassin ",i,sep=""),cex=0)
    lines(sort(res_obs_th),sort(res_obs),col=adjustcolor("blue",.4),type="p")
    lines(sort(res_mis_th),sort(res_mis),col=adjustcolor("red",.4),type="p")
    lines((-1:1)*4*sd(res_obs_th),(-1:1)*4*sd(res_obs),lty=2)
    #
    legend("bottomright",legend=c("Observed","Missing"),col=adjustcolor(c("blue","red"),0.4),lty=1,bty="n")
}
par(mfrow=c(1,1))
#
dev.off()

## VISUALISE PARAMETER POSTERIOR DISTRIBUTIONS ##
chain_           = cbind(chainList_thinned[[1]][,1],chainList_thinned[[1]][,-1][,n_sd_lik + 1 + 1 + 1:n_beta])
colnames(chain_) = c("P","1","year","temp","temp^2","type","type*temp","dbo","type*dbo","alt","rich")
png(paste(pto,"/fig_6.png",sep="")); chainList.postPlot(list(chain_),1000); dev.off()
png(paste(pto,"/fig_7.png",sep="")); chainList.bayesPlot(list(chain_),main=paste(response," ~ estimates ",sep="")); dev.off()
pdf(paste(pto,"/fig_8.pdf",sep="")); chainList.tracePlot(list(chain_)); dev.off()
#
## summary table
summaryTable_     = chainList.summaryTab(list(chain_))[[1]]
summaryTable     = cbind(rownames(summaryTable_),summaryTable_)
colnames(summaryTable) = c("name",colnames(summaryTable_))
write.table(summaryTable,file=paste(pto,"/summary.csv",sep=""),sep=",",row.names=F,quote=F)


## VISUALISE VARIANCES POSTERIOR DISTRIBUTIONS ##
chain_           = cbind(chainList_thinned[[1]][,1],chainList_thinned[[1]][,-1][,1:n_sd_lik])
colnames(chain_) = c("P",paste("sd_",1:n_sd_lik,sep=""))
png(paste(pto,"/fig_9.png",sep="")); chainList.postPlot(list(chain_),1000); dev.off()
png(paste(pto,"/fig_10.png",sep="")); chainList.bayesPlot(list(chain_)); dev.off()
pdf(paste(pto,"/fig_11.pdf",sep="")); chainList.tracePlot(list(chain_)); dev.off()

## VISUALISE MISSING MEAN VARIANCE POSTERIOR DISTRIBUTIONS ##
chain_           = cbind(chainList_thinned[[1]][,1],chainList_thinned[[1]][,-1][,n_sd_lik + 1:2])
colnames(chain_) = c("P","sd_mis","mu_mis")
png(paste(pto,"/fig_12.png",sep="")); chainList.postPlot(list(chain_),1000); dev.off()
png(paste(pto,"/fig_13.png",sep="")); chainList.bayesPlot(list(chain_)); dev.off()
pdf(paste(pto,"/fig_14.pdf",sep="")); chainList.tracePlot(list(chain_)); dev.off()

## VISUALISE MISSING OBSERVATIONS POSTERIOR DISTRIBUTIONS ##
chain_           = cbind(chainList_thinned[[1]][,1],chainList_thinned[[1]][,-1][,n_sd_lik + 1 + 1 + n_beta + 1:n_mis][,1:10])
colnames(chain_) = c("P",paste("mis_",1:10,sep=""))
png(paste(pto,"/fig_15.png",sep="")); chainList.postPlot(list(chain_),1000); dev.off()
png(paste(pto,"/fig_16.png",sep="")); chainList.bayesPlot(list(chain_)); dev.off()
pdf(paste(pto,"/fig_17.pdf",sep="")); chainList.tracePlot(list(chain_)); dev.off()

## COMPUTE SPATIAL CORRELATIONS IN RESIDUALS ##
long_obs = long[-idx_mis]
long_mis = long[ idx_mis]
latt_obs = latt[-idx_mis]
latt_mis = latt[ idx_mis]
x_       = c(long_obs,long_mis)
y_       = c(latt_obs,latt_mis)
#
## compute distance matrix
D        = matrix(rep(0,length(x_)^2),ncol=length(x_),nrow=length(x_)) 
for(i in 1:length(x_))
{
    for(j in 1:length(y_))
    {
        D[i,j] = sqrt((x_[i] - x_[j])^2 + (y_[i] - y_[j])^2)
    }
}
res_ = c(res_obs,res_mis)
#
## compute correlation between residuals with distance
rho_    = NULL
d_      = NULL
for(i in 1:100)
{
    idx   = order(D[i,])
    res_i = res_[idx]
    x_i   = x_[idx]
    y_i   = y_[idx]
    rho_i = NULL
    d_i   = NULL
    for(j in c(seq(1,10,1),seq(10,100,10),seq(100,2000,100)))
    {
        ## correlation 
        res_il  = c(res_i,rep(NA,j))
        res_ir  = c(rep(NA,j),res_i)
        s       = !is.na(res_il*res_ir)
        rho_ij  = cor(res_il[s],res_ir[s])
        #
        ## distance
        x_il   = c(x_i,rep(NA,j))
        x_ir   = c(rep(NA,j),x_i)
        y_il   = c(y_i,rep(NA,j))
        y_ir   = c(rep(NA,j),y_i)
        s      = !is.na(x_il*x_ir)
        d_ij   = mean(sqrt((x_il[s]-x_ir[s])^2 + (y_il[s]-y_ir[s])^2))
        #
        ## concatenate
        rho_i = c(rho_i,rho_ij)
        d_i   = c(  d_i,  d_ij)
    }
    rho_ = rbind(rho_,rho_i)
    d_   = rbind(d_,d_i)
}
rho_mean = apply(rho_,2,mean)
rho_sd   = apply(rho_,2,sd)
d_mean   = apply(  d_,2,mean)
#
## visualise correlation with distance
png(paste(pto,"/fig_18.png",sep=""));
plot(d_mean,rho_mean,xlim=c(min(D),max(D)),ylim=c(0,1))
polygon(x=c(d_mean,rev(d_mean)),y=c(rho_mean+2*rho_sd,rev(rho_mean-2*rho_sd)),border=NA,col=grey(0.5,alpha=0.25))
lines(d_mean,rho_mean,col="red")
dev.off()

## 
# dev.off()

#
###
