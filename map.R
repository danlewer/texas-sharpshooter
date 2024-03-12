library(devEMF)

runif2 <- function (n = 10, min = 0, max = 1) runif(n, min/2, max/2) + runif(n, min/2, max/2)

tab_specific_values <- function(vector, values = unique(vector)) `names<-`(rowSums(outer(values, vector, `==`)), values)

nr <- 10
nc <- 10
mean_pop <- 50
risk <- 0.01
cluster_risk <- 0.05
cluster_number <- 16

# generate data

set.seed(750)
cluster_pops <- sample(1:(nr*nc), size = mean_pop*nr*nc, replace = T)
cluster_pops <- tab_specific_values(cluster_pops, 1:(nr*nc))
cluster_ids <- rep(seq_len(nr*nc), cluster_pops)
tpop <- length(cluster_ids)

trisk <- rep(risk, tpop)
trisk[seq_along(cluster_ids)[cluster_ids == cluster_number]] <- cluster_risk
case <- rbinom(tpop, 1, trisk)
cols <- ifelse(case == 1, 'red', 'black')
case_cluster <- tapply(case, cluster_ids, sum)

pt <- function(x1, x2, n1, n2) prop.test(c(x1, x2), c(n1, n2))$p.value
sig <- which(mapply(pt, 
                    x1 = case_cluster,
                    x2 = sum(case),
                    n1 = cluster_pops,
                    n2 = sum(cluster_pops), 
                    SIMPLIFY = T) < 0.05)

# locations

xl <- rep(seq_len(nc)-1, nr)
yb <- rep(seq_len(nr)-1, each = nc)
yb_cluster <- floor((cluster_number - 1) / nc)
xl_cluster <- cluster_number - nc * yb_cluster - 1
xp <- runif2(tpop) + rep(xl, cluster_pops)
yp <- runif2(tpop) + rep(yb, cluster_pops)
yb_sigs <- floor((sig - 1) / nc)
xl_sigs <- sig - nc * yb_sigs - 1

# plot

emf('texas_map.emf', height = 6, width = 6)

plot(1, type = 'n', xlim = c(0, nc), ylim = c(0, nr), xlab = NA, ylab = NA, axes = F)
axis(1, seq_len(nc)-0.5, seq_len(nc), tick = F)
axis(2, seq_len(nr)-0.5, LETTERS[1:nr], tick = F, las = 2)

rect(xl, yb, xl + 1, yb + 1, lwd = 0.5, lty = 3)
# rect(xl_cluster, yb_cluster, xl_cluster + 1, yb_cluster + 1, border = 'red', col = 'coral')
# rect(xl_sigs, yb_sigs, xl_sigs + 1, yb_sigs + 1)

points(xp, yp, cex = 0.4, col = cols)
points(xp[case == 1], yp[case == 1], cex = 0.4, pch = 19, col = 'red')

dev.off()

case_cluster[sig]
