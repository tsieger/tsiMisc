context('rencol')

test_that("empty rseq obtained", {
    x<-c()
    x<-rencol(x,'','')
    expect_that(x,equals(c()))
})

test_that("single column", {
    x<-data.frame(a=1:2,b=2:3)
    x<-rencol(x,'a','a2')
    expect_that(colnames(x)[1], equals('a2'))

    x<-data.frame(a=1:2,b=2:3)
    x<-rencol(x,'a','a2',1)
    expect_that(colnames(x)[1], equals('a2'))

    x<-data.frame(a=1:2,b=2:3)
    expect_error(rencol(x,'a','a2',2))
})

test_that("multi column", {
    x<-data.frame(prefix.a=1:2,prefix.b=2:3)
    x<-rencol(x,'prefix.','',multi=TRUE)
    expect_that(colnames(x), equals(c('a','b')))
})

test_that("regexp", {
    x<-data.frame(prefix.a=1:2,prefix.b=2:3)
    x<-rencol(x,'prefix.','',multi=TRUE)
    expect_that(colnames(x), equals(c('a','b')))

    x<-data.frame(prefix.a=1:2,prefix.b=2:3)
    expect_error(x<-rencol(x,'pre.','',multi=TRUE))
    x<-rencol(x,'pre.','',multi=TRUE,regexp=TRUE)
    expect_that(colnames(x), equals(c('ix.a','ix.b')))

    x<-data.frame(a=1:2,b=2:3)
    colnames(x)<-c('a(1)','b(1)')
    x<-rencol(x,'(1)','',multi=TRUE,regexp=FALSE)
    expect_that(colnames(x), equals(c('a','b')))

    x<-data.frame(a=1:2,b=2:3)
    colnames(x)<-c('a(1)','b(1)')
    x<-rencol(x,'(1)','',multi=TRUE,regexp=TRUE)
    expect_that(colnames(x), equals(c('a()','b()')))

})

test_that("dots", {
    x<-data.frame(prefix.a1=1:2,prefix.a2=2:3)
    x<-rencol(x,'prefix.a','p.a',multi=TRUE)
    expect_that(colnames(x), equals(c('p.a1','p.a2')))

    x<-data.frame(prefix.a1=1:2,prefix.a2=2:3)
    x<-rencol(x,'prefix.a1','p.a1')
    expect_that(colnames(x), equals(c('p.a1','prefix.a2')))
})
