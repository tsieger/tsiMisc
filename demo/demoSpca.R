# Demonstrate the difference between PCA and SPCA and the need for
# centering prior PCA.

n1<-100
n2<-100
n<-n1+n2
x1<-data.frame(x=rnorm(n1,5,1),y=rnorm(n1,7,1))
x1$y<-x1$y-x1$x*1.5
x2<-data.frame(x=rnorm(n2,7,1),y=rnorm(n2,11,1))
x2$y<-x2$y-x2$x*1.5
x<-rbind(x1,x2)
cls<-c(rep(1,n1),rep(2,n2))
cols<-c('black','red')[cls]

layout(rbind(c(0,0,1,1,0,0),c(2,2,4,4,6,6),c(3,3,5,5,7,7)))
plot(x,pch=19,asp=1,main='orig data',xlim=range(0,range(x$x)),ylim=range(0,range(x$y)),col=cols);abline(v=0,h=0,col='gray')

# correct result - data centered by default in 'prcomp'
tmp<-txPca(x,k=2)(x)
plot(tmp,asp=1,xlim=range(0,range(tmp[,1])),ylim=range(0,range(tmp[,2])),col=cols,main='PCA over centered data');abline(v=0,h=0,col='gray')

# perhaps unwanted result - data not centered, PCs make not much sense
tmp<-txPca(x,k=2,center=FALSE)(x)
plot(tmp,asp=1,xlim=range(0,range(tmp[,1])),ylim=range(0,range(tmp[,2])),col=cols,main='PCA over non-centered data');abline(v=0,h=0,col='gray')

# correct result - data centered implicitly
tmp<-txSpca(x,k=2)(x)
# this is the same as:
#   tmp<-scale(x,center=TRUE,scale=FALSE)
#   tmp<-txSpca(tmp,k=2)(tmp)
plot(tmp,asp=1,xlim=range(0,range(tmp[,1])),ylim=range(0,range(tmp[,2])),col=cols,main=c('SPCA over centered data','SPCA (w/out classes)'));abline(v=0,h=0,col='gray')

# incorrect result - data not centered
tmp<-txSpca(x,k=2,center=FALSE)(x)
plot(tmp,asp=1,xlim=range(0,range(tmp[,1])),ylim=range(0,range(tmp[,2])),col=cols,main=c('SPCA over non-centered data','SPCA (w/out classes)'));abline(v=0,h=0,col='gray')

# correct result - data centered implicitly
tmp<-txSpca(x,cls,k=2)(x)
# this is the same as:
#   tmp<-scale(x,center=TRUE,scale=FALSE)
#   tmp<-txSpca(tmp,cls,k=2)(tmp)
plot(tmp,asp=1,xlim=range(0,range(tmp[,1])),ylim=range(0,range(tmp[,2])),col=cols,main='SPCA over centered data');abline(v=0,h=0,col='gray')

# incorrect result - data not centered
tmp<-txSpca(x,cls,k=2,center=FALSE)(x)
plot(tmp,asp=1,xlim=range(0,range(tmp[,1])),ylim=range(0,range(tmp[,2])),col=cols,main='SPCA over non-centered data');abline(v=0,h=0,col='gray')

layout(1)
