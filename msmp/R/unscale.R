
##################################################
# unscale : reverses the mean center transformation
# s: vector with the value to unscale
# o: the vector of the original value
# center : if the mean center is performed
# scale : if the scale is performed
##################################################


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
