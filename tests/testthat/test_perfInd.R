n<-300000
x<-rep(c(0,1),n)
y<-rep(c(0,1),each=n)
# expect no warning
expect_silent(perfInd(x,y))

m<-as.table(matrix(n/2,2,2,dimnames=list(x=c(0,1),y=c(0,1))))
expect_that(perfInd(x,y)$table, equals(m))


# check missing categories in x,y
m<-as.table(matrix(c(1,0,0,0),2,2,dimnames=list(x=c('0','02'),y=c('0','02'))))
expect_that(perfInd(0,0)$table, equals(m))

m<-as.table(matrix(c(1,0,1,0),2,2,dimnames=list(x=c('0','1'),y=c('0','1'))))
expect_that(perfInd(c(0,0),c(0,1))$table, equals(m))

m<-as.table(matrix(c(1,0,1,0),2,2,dimnames=list(x=c('0','1'),y=c('0','1'))))
expect_that(perfInd(factor(c(0,0)),factor(c(0,1)))$table, equals(m))
