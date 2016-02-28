x<-data.frame(a=1:6,b=c('a','b','b','b','b','a'),c=c('c','c','x','d','d','x'))
x.train<-x[1:3,]
x.test<-x[4:6,]
res<-refineFactors(x.train,x.test)

expect_that(nrow(res$x1), equals(0))
expect_that(nrow(res$x2), equals(0))
