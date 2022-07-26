 ybar <- 162.3
 sdy <- 2.7
 n <- 12
 sp <- sqrt( ((m-1)*sdx^2 + (n-1)*sdy^2) / (m+n-2) )

 t <- ((xbar-ybar) -10) / (sp*sqrt(1/m+1/n))
 t
  1-pt(t, m+n-2)
 (xbar- ybar) - qt(0.975, m+n-2)*sp * sqrt(1/m+1/n)
 (xbar- ybar) + qt(0.975, m+n-2)*sp * sqrt(1/m+1/n)


