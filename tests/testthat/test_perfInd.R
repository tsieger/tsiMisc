n<-300000
x<-rep(c(0,1),n)
y<-rep(c(0,1),each=n)
# expect no warning
expect_silent(perfInd(x,y))

m<-as.table(array(n/2,c(2,2),list(x=c(0,1),y=c(0,1))))
expect_that(perfInd(x,y)$table, equals(m))
