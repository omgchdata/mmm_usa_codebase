
unscale = function(s, o, center = TRUE, scale = TRUE)  {
  t <- scale(o, center = TRUE, scale = TRUE)
  if(scale & center) {
    res <- s*attr(t, "scaled:scale") + attr(t, "scaled:center")
  }
  if(scale & !center) {
    res <- s*attr(t, "scaled:scale")
  }
  if(!scale & center) {
    res <- s + attr(t, "scaled:center")
  }
  return(res)
}

d <- data.frame(
  cbind(x=runif(30, 1, 10), z=runif(30, 2, 20))
)

d$y <- 17 + d$x * 12 + d$z * 5 + rnorm(nrow(d), 0, 1)

d$s.y <- scale(d$y)

m1 <- lm(y ~ x + z, data=d) 
m2 <- lm(s.y ~ x + z, data=d)

print(coef(m1))   # the coef of the model in the original scale
print(coef(m2))
print(unscale(coef(m2)[1], d$y)) # you want to un-scale and un-center intercept
print(unscale(coef(m2)[2], d$y, center=F)) # you want only un-scale the slope coef
print(unscale(coef(m2)[3], d$y, center=F))

d$dx1 <- coef(m1)[2] * d$x  # decomp in the original scale
d$dx2 <- coef(m2)[2] * d$x  # decomp in the scaled scale
d$dz1 <- coef(m1)[3] * d$z
d$dz2 <- coef(m2)[3] * d$z


