library(unittest, quietly = TRUE)
library(magrittr)
library(RColorBrewer)

source('R/multiplot.r')

#----------------------------------------------
# normalize_vec
#----------------------------------------------
nx0 <- normalize_vec(c(1,2),c(1,2,3,4))  %>% (\(x) x$nx)
nx1 <- c(1,1+1/3,1+2/3,2)
ok(!sum(!(nx0 == nx1)))

x0 <- c(1.0,2.0,3.0,4.3)
res <- normalize_vec(c(1,2),x0)
ok(sum(abs(res$rev_fn(res$nx) - x0)) < 1e-10)

#----------------------------------------------
# make_object
#----------------------------------------------

add_x_axis <- \(y) cbind(y, data.frame(x = y$or/(y$ab+y$or)))

cols = brewer.pal(7,"Dark2")

dat <- read.csv("R/test/data/kroll_et_al_1986_table5.csv") 
dat <- dat %>% add_x_axis

pdf("R/test/output/cell_dimension.pdf", width = 6, height = 6)
par(mar = c(4, 4, 4, 4))
plane_plot(xaxs = "i")
obj <- make_object(c(0.5,0.85), dat$x, dat$a, xlim=c(0,1))
s <- obj$get_yaxis_space()

obj$points(pch=15, col = cols[1])
obj$yaxis(side = 2, line=0.5, col = cols[1], lwd = 2, col.axis = cols[1])
obj$regression(col="gray50", lwd=2, lty="dashed")
obj$xaxis()
obj$ytext("a", side = 2, col = cols[1])

obj <- make_object(c(0.7, 0.9), dat$x, dat$beta)
obj$points(pch=18, col = cols[4])
obj$yaxis(side = 4, line=0.5, npretty = 0, col = cols[4], lwd = 2, col.axis = cols[4])
obj$regression(col="gray50", lwd=2, lty="dashed")
obj$ytext(expression(beta), side = 4,col = cols[4])
dev.off()