context('refineFactors')

x<-data.frame(b=c('a','b','b','b','b','a'),c=c('c','c','x','d','d','x'))
x.train<-x[1:3,]
x.test<-x[4:6,]

res<-refineFactors(x.train,x.test)
expect_that(ncol(res$x1), equals(0))
expect_that(ncol(res$x2), equals(0))

res<-refineFactors(x.train, x.test, dropSingular = FALSE)
expect_that(nrow(res$x1), equals(1))
expect_that(ncol(res$x1), equals(1))
expect_that(nrow(res$x2), equals(1))
expect_that(ncol(res$x2), equals(1))


y<-data.frame(a=1:6,b=c('a','b','b','b','b','a'),c=c('c','c','x','d','d','x'))
y.train<-y[1:3,]
y.test<-y[4:6,]

res<-refineFactors(y.train,y.test)
expect_that(nrow(res$x1), equals(3))
expect_that(ncol(res$x1), equals(1))
expect_that(nrow(res$x2), equals(3))
expect_that(ncol(res$x2), equals(1))

res<-refineFactors(y.train, y.test, dropSingular = FALSE)
expect_that(nrow(res$x1), equals(1))
expect_that(ncol(res$x1), equals(2))
expect_that(nrow(res$x2), equals(1))
expect_that(ncol(res$x2), equals(2))
