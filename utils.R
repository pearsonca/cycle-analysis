# get the raw pairs data
# for each of the simulated pairs data
# merge that run, pairs

# run in output dir

rm(list=ls())

require(data.table)
require(igraph)
require(parallel)

background <- readRDS("input/pairs.RData")

firstday <- floor(background[,min(start)]/60/60/24)

args <- commandArgs(trailingOnly = T)
if (length(args) == 0) args <- "."
pth <- args[1]

if (file.exists(paste0(pth,"proc.tar.gz"))) untar(paste0(pth,"proc.tar.gz"))

runs <- list.files(path = pth, pattern = "-0-combos.RData", full.names = T)
pairs <- function(after, before, res) res[
  after < end & start < before, .N, keyby=list(userA, userB, reason)
] 

isos <- Filter(function(g) vcount(g) < 6, readRDS("input/isos.rds"))
isosizes <- lapply(isos, vcount)

if (length(args) < 2) args[2] <- detectCores()
cores <- args[2]

score <- function(s, e, res, t) {
  prs <- pairs(s*24*60*60, e*24*60*60, res)
  remap_ids <- data.table(user_id = prs[,unique(c(userA,userB))])[,new_user_id := .I, keyby=user_id]
  relabelled <- data.table(userA=remap_ids[prs[,list(user_id=userA)]]$new_user_id, userB=remap_ids[prs[,list(user_id=userB)]]$new_user_id)
  el <- t(as.matrix(relabelled))
  dim(el) <- NULL
  g <- graph(el, directed = T)
  subs <- mclapply(isos, function(kring, g) graph.get.subisomorphisms.vf2(g, kring), g=g, mc.cores = cores)
  mapply(
    function(cyc, sz) if (length(cyc) != 0) {
      src <- unique(t(apply(matrix(unlist(cyc), byrow = T, ncol=sz), 1, sort)))
      lsrc <- lapply(1:(dim(src)[2]), function(col) remap_ids[src[, col]]$user_id)
      lsrc$time=t
      do.call(data.table, lsrc)
    } else {
      data.table(matrix(1, ncol=sz), time=0)[0]
    },
    subs,
    isosizes
  )
}

processList <- function(run, startDay=100, window = 14, endDay=startDay + 52*window) {
  starts <- seq(startDay+window, endDay-window, by=window)
  res <- setkey(rbind(background, run[,list(userA, userB, start=login, end=logout, reason)]), start)[, start := start - firstday*60*60*24][, end := end - firstday*60*60*24]
  
  ## TODO add this pairs script
  res <- res[,c(list(swap=userA>userB), .SD)][,list(userA=ifelse(swap, userB, userA), userB=ifelse(swap, userA, userB), start, end, reason)]
  ## write starting scores
  i <- 0
  results <- score(startDay, startDay+window, res, i)
  
  for (s in seq(startDay+window, endDay-window, by=window)) {
    i <- i+1
    # print(i)
    results <- c(results, score(s, s+window, res, i))
    #for (j in 1:length(isosizes)) results[[j]] <- rbind(results[[j]], appresults[[j]])
  }
  results
}

scoreall <- function(runfile) {
  run <- readRDS(runfile)
  prc <- processList(run)
#  scr <- longitudinalScore(prc)
  newfile <- sub("combos","score", runfile)
  saveRDS(pcr, newfile)
  cat("finished",newfile,"\n")
}

chk <- lapply(runs, scoreall)

#testAnalysis <- processList(run, window=14, endDay = 100+14*52)

## base score for each pair == pairing count in interval in excess of 1
## each pairing appearance garners score of 1/cycle length
# allcolpairs <- lapply(isosizes, function(n) combn(paste("V",1:n,sep=""), 2))
# 
# longitudinalScore <- function(processed) {
#   setkey(rbindlist(
#       mcmapply(function(kres, colpairs) {
#       k <- dim(kres)[2] - 1
#       setnames(rbindlist(apply(colpairs, 2, function(col) kres[,.N,by=c(col, "time")]))[,list(N=sum(N), k=k),keyby=list(time,V1,V2)], c("V1","V2"), c("userA","userB"))
#     }, processed, allcolpairs, SIMPLIFY = F)
#   ), time, k)[,
#     list(swap=userA>userB, score=N/k), by=list(time, userA, userB)
#   ][,
#     list(userA=ifelse(swap, userB, userA), userB=ifelse(swap, userA, userB), score, time)
#   ][,
#     list(score=sum(score)), keyby=list(userA,userB,time)
#   ]
# }

# tempThing <- longitudinalScore(testAnalysis)[,
#   list(swap=userA>userB, score=N/k), by=list(time,userA, userB)
# ][,
#   list(userA=ifelse(swap,userB,userA), userB=ifelse(swap,userA,userB), score, time)
# ][,
#   list(score=sum(score)), keyby=list(userA,userB,time)
# ]

# scoreall <- function(runfile) {
#   run <- readRDS(runfile)
#   prc <- processList(run)
#   scr <- longitudinalScore(prc)
#   newfile <- sub("combos","score", runfile)
#   saveRDS(scr, newfile)
#   cat("finished",newfile,"\n")
# }
# 
# chk <- lapply(runs, scoreall)

# detectByScores <- function(dt, discount=.9) {
#   temp <- rbindlist(
#     list(dt[,sum(N/k),keyby=list(time, userA)],
#     dt[,sum(N/k),keyby=list(time, userB)])
#   )
#   temp <- temp[,list(score = sum(V1)),keyby=list(time,userA)]
#   setkey(setnames(temp, "userA", "user"), user, time)
#   users <- sort(unique(temp$userA)); tmax <- temp[,max(time)]
#   ref <- data.table(user=rep(users, each=tmax+1), time=rep(0:tmax, times=length(users)), key=c("user","time"))
#   expand <- temp[ref]
#   expand[is.na(score), score := 0]
#   expand[, list(time, score = Reduce(function(left, right) { left*discount + right }, score, accumulate=T)), by=user]
#   # temp[,list(time, score = cumsum(score)),keyby=list(userA)]
# }



# ggplot(thing) + theme_bw() +
#   aes(x=time, y=score, color=utype, alpha=utype, group=user) + geom_line() +
#   scale_color_manual(values = c(covert="red", background="black")) +
#   scale_alpha_manual(values = c(covert=1, background=.1))