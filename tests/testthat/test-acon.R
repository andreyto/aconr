context("acon")

test_that("acon constructor works as expected",{
  zz = acon(s=1,z=acon(v=4,w=acon(x=v*2,y=s)))
  x = acon(s=1:4,
           f=function(x) sprintf('x is %s',x),
           t=acon(x=s*2,
                  z=acon(k=x*4)
           ),
           p = zz$z
  )
  expect_equal(x$t$z$k, seq(8,32,by = 8))
  expect_equal(x$p$w$x,8)
  expect_equal(x$p$w$y,1:4)
  y = acon(x,
           s=1:16,
           f=function(x) sprintf('y is %s',x),
           t=acon(l=10))
  expect_equal(y$t$z$k, seq(8,128,by = 8))
  expect_equal(x$t$z$k, seq(8,32,by = 8))
  expect_equal(y$p$w$y,1:16)
  expect_equal(x$p$w$y,1:4)
})

