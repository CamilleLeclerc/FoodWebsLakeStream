#####
## ##
#####

## goal:

## author: Willem Bonnaffe (w.bonnaffe@gmail.com)

## update log:
## 07-07-2022 - created v0_0
## 11-07-2022 - created v0_1
##            - split missing covariate matrix into left and right component
##            - added plot of the interaction
## 10-09-2022 - created v0_2
##            - updated figures

###############
## FUNCTIONS ##
###############

#
###

##############
## INITIATE ##
##############

## load module
# source("m1_con_load.r")
source("m1_mTL_load.r")

#
###

#############
## FIGURES ##
#############

## goal:

## load chains
chainList_ = list()
load(paste(pto,"/chain_thinned_",1,".RData",sep="")); chainList_[[1]] = chainList_thinned[[1]]
load(paste(pto,"/chain_thinned_",2,".RData",sep="")); chainList_[[2]] = chainList_thinned[[1]]
chainList_thinned = chainList_

## VISUALISE PARAMETER POSTERIOR DISTRIBUTIONS ##
chainList_  = list()
for(i in 1:length(chainList_thinned))
{
    chain_           = cbind(chainList_thinned[[i]][,1],chainList_thinned[[i]][,-1][,idx_omega_beta])
    colnames(chain_) = c("P",colnames(X_obs))
    chainList_[[i]]  = chain_    
}
pdf(paste(pto,"/fig_postPlot_beta.pdf",sep="")); chainList.postPlot(chainList_,1000); dev.off()
pdf(paste(pto,"/fig_bayesPlot_beta.pdf",sep="")); chainList.bayesPlot(chainList_,main=paste(response," ~ estimates ",sep="")); dev.off()
pdf(paste(pto,"/fig_tracePlot_beta.pdf",sep="")); chainList.tracePlot(chainList_); dev.off()

## SUMMARY TABLE ##
summaryTable_    = chainList.summaryTab(chainList_)[[1]]
summaryTable     = cbind(rownames(summaryTable_),summaryTable_)
colnames(summaryTable) = c("name",colnames(summaryTable_))
write.table(summaryTable,file=paste(pto,"/summary.csv",sep=""),sep=",",row.names=F,quote=F)
nscode   = (summaryTable$signif[-1]=="*")*1

## PLOT PREDICTIONS ##
pdf(paste(pto,"/fig_predictions.pdf",sep=""))
#
par(mfrow=c(2,2))
#
mainVect = c("a. Effect of temp. in streams","b. Effect of temp. in lakes")
colVect  = adjustcolor(c("blue","red","blue","red"),alpha=0.9)
alpha    = 0.5
k        = 1
#
X = rbind(X_obs,X_mis_l)
Y = c(Y_obs,Y_mis)
for(i in 1:2)
{
    plot(X[X[,2]==i-1,3],Y[X[,2]==i-1],pch=16,col=adjustcolor("black",alpha=0.5),xlab="Temperature",ylab=response,main=paste(mainVect[i],sep=""),bty="n",xlim=c(min(X[,3]),max(X[,3])),ylim=c(min(Y),max(Y)),xaxt="n",yaxt="n")
    ##
    Y_ = c(-2,2)
    for(j in 1:length(Y_))
    {
        ## predictions
        y        = Y_[j]
        x        = seq(min(temp),max(temp),0.1)
        pred     = chainList.apply(chainList_thinned,function(x_) Yhat(X_pred(x,y,i),x_[-1][idx_omega_beta]*nscode))
        polygon(x=c(x,rev(x)),y=c(pred$f_q0.05,rev(pred$f_q0.95)),border=NA,col=adjustcolor(colVect[k],alpha=alpha))
        lines(x,pred$f_mean,col=colVect[k],lwd=2)
        k = k + 1
    }
    #
    legend("bottomright",legend=c("Low BOD","High BOD"),lty=1,col=colVect,bty="n")
    #
    ## axis
    x  = seq(-3,3,1)
    x_ = x*temp_sd+temp_mean
    print(paste("low temp: ",-2*temp_sd + temp_mean))
    print(paste("hi temp:  ",+2*temp_sd + temp_mean))
    y  = seq(-3,3,1)
    y_ = y*Y_sd+Y_mean 
    axis(1,label=round(x_,2),at=x)
    axis(2,label=round(y_,2),at=y)

}
#
mainVect = c("c. Effect of BOD in streams","d. Effect of BOD in lakes")
alpha = 0.5
k = 1
#
xmis_ = chainList.unlist(chainList_thinned)[,-1][1,idx_omega_xmis]
# xmis_ = rnorm(n_mis,0,1)
X = rbind(X_obs,X_mis_l*X_mis_r(xmis_))
Y = c(Y_obs,Y_mis)
for(i in 1:2)
{
    plot(X[X[,2]==i-1,6],Y[X[,2]==i-1],pch=16,col=adjustcolor("black",alpha=0.5),xlab="BOD",ylab=response,main=paste(mainVect[i],sep=""),bty="n",xlim=c(min(X[,6]),max(X[,6])),ylim=c(min(Y),max(Y)),xaxt="n",yaxt="n")

    ##
    Y_ = c(-2,2)
    for(j in 1:length(Y_))
    {
        ## predictions
        y        = Y_[j]
        x        = seq(min(bod,na.rm=T),max(bod,na.rm=T),0.1)
        pred     = chainList.apply(chainList_thinned,function(x_) Yhat(X_pred(y,x,i),x_[-1][idx_omega_beta]*nscode))
        polygon(x=c(x,rev(x)),y=c(pred$f_q0.05,rev(pred$f_q0.95)),border=NA,col=adjustcolor(colVect[k],alpha=alpha))
        lines(x,pred$f_mean,col=colVect[k],lwd=2)
        k = k + 1
    }
    #
    legend("bottomright",legend=c("Low temp.","High temp."),lty=1,col=colVect,bty="n")
    #
    ## axis
    x  = seq(-3,3,1)
    x_ = x*bod_sd+bod_mean
    print(paste("min bod: ",min(x_)))
    print(paste("low bod: ",-2*bod_sd + bod_mean))
    print(paste("hi bod: " ,+2*bod_sd + bod_mean))
    y  = seq(-3,3,1)
    y_ = y*Y_sd+Y_mean 
    axis(1,label=round(x_,2),at=x)
    axis(2,label=round(y_,2),at=y)

}
#
par(mfrow=c(1,1))
#
dev.off()

# ## VISUALISE INTERACTION ##
# pdf(paste(pto,"/fig_interactions.pdf",sep=""))
# #
# par(mfrow=c(2,2))
# #
# main = c(paste(response," in streams"),paste(response," in lakes",sep=""))
# labs = c("Temperature","BOD")
# for(i in 1:2)
# {
#     ## compute effect matrix
#     x  = y = seq(-3,3,0.1)
#     n  = length(x)
#     IM = matrix(rep(0,n),nrow=n,ncol=n)
#     f  = function(x,y,i) chainList.apply(chainList_thinned,function(x_) Yhat(X_pred(x,y,i),x_[-1][idx_omega_beta]*nscode))$f_mean
#     for(j in 1:n) IM[,j] = f(x,y[j],i)
#     #
#     ## visualise matrix
#     maxAbsMinMax = max(abs(IM))
#     levels       = seq(-maxAbsMinMax,maxAbsMinMax,2*maxAbsMinMax/1000)
#     colorLevels  = rev(rainbow(1000,start=0,end=1,alpha=0.5))
#     image(IM,breaks=levels,col=colorLevels,xaxt="n",yaxt="n",xlab=labs[1],ylab=labs[2],main=main[i])
#     contour(IM,add=T)
#     #
#     ## axis
#     x  = seq(-3,3,1)
#     x_ = x*temp_sd+temp_mean
#     y  = seq(-3,3,1)
#     y_ = y*bod_sd+bod_mean
#     axis(1,label=round(x_,2),at=(x-min(x))/(max(x)-min(x)))
#     axis(2,label=round(y_,2),at=(y-min(y))/(max(y)-min(y)))
# }
# par(mfrow=c(1,1))
# #
# dev.off()

## VISUALISE MISSING VS OBSERVED BOD ##
pdf(paste(pto,"/fig_hist_missing_bod.pdf",sep=""))
#
x = density(bod,na.rm=T)$x
y = density(bod,na.rm=T)$y; y=y/max(y)
plot(x,y,type="l",col="white",xlab="BOD (SU)",ylab="Density (SU)",main=paste(response," ~ BOD distribution",sep=""))
polygon(x=c(x,rev(x)),y=c(rep(0,length(y)),rev(y)),col=adjustcolor("blue",0.4),border=NA)
#
bod_mis = chainList.argmaxPost(chainList_thinned)[idx_omega_xmis]
x = density(bod_mis,na.rm=T)$x
y = density(bod_mis,na.rm=T)$y; y=y/max(y)
polygon(x=c(x,rev(x)),y=c(rep(0,length(y)),rev(y)),col=adjustcolor("red",0.4),border=NA)
#
legend("topright",legend=c("Observed","Missing"),col=adjustcolor(c("blue","red"),0.4),lty=1,bty="n")
#
dev.off()

## VERIFY MODEL ASSUMPTIONS ##
x_mis_      = apply(chainList.unlist(chainList_thinned)[,-1][,idx_omega_xmis],2,mean)
X_mis_      = X_mis_l * X_mis_r(x_mis_)
chainList_  = list(chainList.unlist(chainList_thinned)[,-1][,idx_omega_beta])
Yhat_obs    = chainList.apply(chainList_,function(x)Yhat(X_obs,x))$f_mean
Yhat_mis    = chainList.apply(chainList_,function(x)Yhat(X_mis_,x))$f_mean
res_obs     = Y_obs - Yhat_obs
res_mis     = Y_mis - Yhat_mis

## HISTOGRAM OF RESIDUALS ##
pdf(paste(pto,"/fig_hist_residuals.pdf",sep=""))
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
pdf(paste(pto,"/fig_qqplot_residuals.pdf",sep=""))
#
sdVect = apply(chainList.unlist(chainList_thinned)[,-1][,idx_omega_sd_lik],2,mean)
par(mfrow=c(3,3))
for(i in 1:n_sd_lik)
{
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

## VISUALISE VARIANCES POSTERIOR DISTRIBUTIONS ##
chainList_  = list()
for(i in 1:length(chainList_thinned))
{
    chain_           = cbind(chainList_thinned[[i]][,1],chainList_thinned[[i]][,-1][,idx_omega_sd_lik])
    colnames(chain_) = c("P",paste("sd_",1:n_sd_lik,sep=""))
    chainList_[[i]]  = chain_    
}
pdf(paste(pto,"/fig_postPlot_sd_lik.pdf",sep="")); chainList.postPlot(chainList_,1000); dev.off()
pdf(paste(pto,"/fig_bayesPlot_sd_lik.pdf",sep="")); chainList.bayesPlot(chainList_); dev.off()
pdf(paste(pto,"/fig_tracePlot_sd_lik.pdf",sep="")); chainList.tracePlot(chainList_); dev.off()

## VISUALISE MISSING MEAN VARIANCE POSTERIOR DISTRIBUTIONS ##
chainList_  = list()
for(i in 1:length(chainList_thinned))
{
    chain_           = cbind(chainList_thinned[[i]][,1],chainList_thinned[[i]][,-1][,c(idx_omega_mu_mis,idx_omega_sd_mis)])
    colnames(chain_) = c("P","mu_mis","sd_mis")
    chainList_[[i]]  = chain_    
}
pdf(paste(pto,"/fig_postPlot_sd_mis.pdf",sep="")); chainList.postPlot(chainList_,1000); dev.off()
pdf(paste(pto,"/fig_bayesPlot_sd_mis.pdf",sep="")); chainList.bayesPlot(chainList_); dev.off()
pdf(paste(pto,"/fig_tracePlot_sd_mis.pdf",sep="")); chainList.tracePlot(chainList_); dev.off()

## VISUALISE MISSING OBSERVATIONS POSTERIOR DISTRIBUTIONS ##
chainList_  = list()
for(i in 1:length(chainList_thinned))
{
    chain_           = cbind(chainList_thinned[[i]][,1],chainList_thinned[[i]][,-1][,idx_omega_xmis][,1:10])
    colnames(chain_) = c("P",paste("mis_",1:10,sep=""))
    chainList_[[i]]  = chain_    
}
pdf(paste(pto,"/fig_postPlot_missing_bod.pdf",sep="")); chainList.postPlot(chainList_,1000); dev.off()
pdf(paste(pto,"/fig_bayesPlot_missing_bod.pdf",sep="")); chainList.bayesPlot(chainList_); dev.off()
pdf(paste(pto,"/fig_tracePlot_missing_bod.pdf",sep="")); chainList.tracePlot(chainList_); dev.off()

## VISUALISE CORRELATIONS POSTERIOR DISTRIBUTIONS ##
chainList_  = list()
for(i in 1:length(chainList_thinned))
{
    chain_           = cbind(chainList_thinned[[i]][,1],chainList_thinned[[i]][,-1][,idx_omega_rho])
    colnames(chain_) = c("P",paste("rho_",1:2,sep=""))
    chainList_[[i]]  = chain_    
}
pdf(paste(pto,"/fig_postPlot_rho.pdf",sep="")); chainList.postPlot(chainList_,1000); dev.off()
pdf(paste(pto,"/fig_bayesPlot_rho.pdf",sep="")); chainList.bayesPlot(chainList_); dev.off()
pdf(paste(pto,"/fig_tracePlot_rho.pdf",sep="")); chainList.tracePlot(chainList_); dev.off()

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
pdf(paste(pto,"/fig_spatial_autocorrelations.pdf",sep=""));
plot(d_mean,rho_mean,xlim=c(min(D),max(D)),ylim=c(0,1))
polygon(x=c(d_mean,rev(d_mean)),y=c(rho_mean+2*rho_sd,rev(rho_mean-2*rho_sd)),border=NA,col=grey(0.5,alpha=0.25))
lines(d_mean,rho_mean,col="red")
dev.off()

##
# dev.off()

#
###
