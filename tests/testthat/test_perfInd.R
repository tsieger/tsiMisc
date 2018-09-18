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

m<-as.table(matrix(c(0,0,0,0),2,2,dimnames=list(x=c('a','a2'),y=c('c','d'))))
expect_that(perfInd(factor(c(0,0),levels=c('a')),factor(c(0,1),levels=c('c','d')))$table, equals(m))

m<-as.table(matrix(c(1,0,1,0),2,2,dimnames=list(x=c('a','a2'),y=c('c','d'))))
expect_that(perfInd(factor(c('a','a'),levels=c('a')),factor(c('c','d'),levels=c('c','d')))$table, equals(m))


# check imputing missing rows/columns
m<-as.table(matrix(c(1,2),1,2,dimnames=list(x=c('FALSE'),y=c('c','d'))))
m2<-as.table(matrix(c(1,0,2,0),2,2,dimnames=list(x=c('FALSE','TRUE'),y=c('c','d'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1,2),1,2,dimnames=list(x=c('TRUE'),y=c('c','d'))))
m2<-as.table(matrix(c(0,1,0,2),2,2,dimnames=list(x=c('FALSE','TRUE'),y=c('c','d'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1,2),2,1,dimnames=list(x=c('c','d'),y=c('FALSE'))))
m2<-as.table(matrix(c(1,2,0,0),2,2,dimnames=list(x=c('c','d'),y=c('FALSE','TRUE'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1,2),2,1,dimnames=list(x=c('c','d'),y=c('TRUE'))))
m2<-as.table(matrix(c(0,0,1,2),2,2,dimnames=list(x=c('c','d'),y=c('FALSE','TRUE'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1),1,1,dimnames=list(x=c('FALSE'),y=c('FALSE'))))
m2<-as.table(matrix(c(1,0,0,0),2,2,dimnames=list(x=c('FALSE','TRUE'),y=c('FALSE','TRUE'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1),1,1,dimnames=list(x=c('FALSE'),y=c('TRUE'))))
m2<-as.table(matrix(c(0,0,1,0),2,2,dimnames=list(x=c('FALSE','TRUE'),y=c('FALSE','TRUE'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1),1,1,dimnames=list(x=c('TRUE'),y=c('FALSE'))))
m2<-as.table(matrix(c(0,1,0,0),2,2,dimnames=list(x=c('FALSE','TRUE'),y=c('FALSE','TRUE'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

m<-as.table(matrix(c(1),1,1,dimnames=list(x=c('TRUE'),y=c('TRUE'))))
m2<-as.table(matrix(c(0,0,0,1),2,2,dimnames=list(x=c('FALSE','TRUE'),y=c('FALSE','TRUE'))))
m3<-perfInd(m)$table
expect_that(m3, equals(m2))

