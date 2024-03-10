runif2 <- function (n = 10, min = 0, max = 1) runif(n, min/2, max/2) + runif(n, min/2, max/2)

nr <- 7
nc <- 12
mean_pop <- 50
risk <- 0.01
cluster_risk <- 0.05
cluster_number <- 16

# generate data

# > which(res > 5)
# [1] 144 555

res <- NULL
for(i in 1:1000) {
if(i %% 10 == 0) print (i)

set.seed(i)
cluster_pops <- rnbinom(nr * nc, mu = mean_pop, size = 7)
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

res[i] <- length(sig)
}

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

plot(1, type = 'n', xlim = c(0, nc), ylim = c(0, nr), xlab = NA, ylab = NA, axes = F)
axis(1, seq_len(nc)-0.5, seq_len(nc), tick = F)
axis(2, seq_len(nr)-0.5, LETTERS[1:nr], tick = F, las = 2)

rect(xl, yb, xl + 1, yb + 1, lwd = 0.5, lty = 3)
rect(xl_cluster, yb_cluster, xl_cluster + 1, yb_cluster + 1, border = 'red', col = 'coral')
rect(xl_sigs, yb_sigs, xl_sigs + 1, yb_sigs + 1)

points(xp, yp, cex = 0.4, col = cols)
points(xp[case == 1], yp[case == 1], cex = 0.4, col = 'red')
case_cluster[sig]

