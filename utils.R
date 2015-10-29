# get the raw pairs data
# for each of the simulated pairs data
# merge that run, pairs

# run in output dir

require(data.table)
require(igraph)
require(parallel)

background <- readRDS("../../input/pairs.RData")

firstday <- floor(background[,min(start)]/60/60/24)

runs <- list.files(pattern = "RData")
pairs <- function(after, before, res) res[
  after < end & start < before, .N, keyby=list(userA, userB, reason)
] 

run <- readRDS(runs[1])

ring.sizes <- 3:6

krings <- lapply(ring.sizes, graph.ring)

cores <- min(detectCores()-1, length(ring.sizes))

score <- function(s, e, res) {
  prs <- pairs(s*24*60*60, e*24*60*60, res)
  remap_ids <- data.table(user_id = prs[,unique(c(userA,userB))])[,new_user_id := .I, by=user_id]
  relabelled <- data.table(userA=remap_ids[user_id == prs$userA, new_user_id], userB=remap_ids[user_id == prs$userB, new_user_id])
  el <- as.matrix(relabelled)
  dim(el) <- NULL
  g <- graph(el, directed = F)
  mapply(
    function(cyc, sz) if (length(cyc) != 0) {
      src <- matrix(unlist(cyc), byrow = T, ncol=sz)
      data.table(unique(t(apply(src, 1, sort))), time=s)
    } else {
      data.table(matrix(1, ncol=sz), time=0)[0]
    },
    mclapply(krings, function(kring, g) graph.get.subisomorphisms.vf2(g, kring), g=g, mc.cores = cores),
    ring.sizes
  )
}

processList <- function(run, startDay=100, window = 7, endDay=startDay + 100*window) {
  cat("starting week, day ", startDay,"\n")
  res <- setkey(rbind(background, run[,list(userA, userB, start=login, end=logout, reason)]), start)[, start := start - firstday*60*60*24][, end := end - firstday*60*60*24]
  
  ## TODO add this pairs script
  res <- res[,c(list(swap=userA>userB), .SD)][,list(userA=ifelse(swap, userB, userA), userB=ifelse(swap, userA, userB), start, end, reason)]
  ## write starting scores
  
  results <- score(startDay, startDay+window, res)
  
  for (s in seq(startDay+window, endDay, by=window)) {
    cat("starting week, day ", s,"\n")
    appresults <- score(s, s+window, res)
    for (j in 1:length(ring.sizes)) results[[j]] <- rbind(results[[j]], appresults[[j]])
  }
  results
}
