library("snow")

run = function(x) {
  
  set.seed(conf.seed)
  
  in.data.disc.file = sprintf("data/rda/%s/%s.data.disc.rda", x$db, x$db)
  in.labels.file = sprintf("data/rda/%s/%s.labels.rda", x$db, x$db)
  in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", x$db, x$db, conf.nb.cv, x$rep)
  in.mbs.file = sprintf("mbs.in.x/%s/%s/%s.cv%02i.r%02i.s%02i.rda", x$mb.method.desc, x$db, x$db, conf.nb.cv, x$rep, x$split)
  
  out.lps.dir = sprintf("ilf/%s/%s/%s", x$mb.method.desc, x$ci.method.desc, x$db)
  dir.create(out.lps.dir, showWarnings=FALSE, recursive=TRUE)
  
  out.file = sprintf("%s/%s.cv%02i.r%02i.s%02i.rda", out.lps.dir, x$db, conf.nb.cv, x$rep, x$split)
  
  load(in.data.disc.file)
  load(in.labels.file)
  load(in.cv.file)
  load(in.mbs.file)
  
  train = data.disc[cv.splits != x$split, ]
  labels = labels[labels.ord]
  ys = vapply(labels, function(x) {which(colnames(train) == x)}, integer(1))
  xs = (1:ncol(train))[-ys]
  features = colnames(train)[xs]
  
  labels = as.array(labels)
  names(labels) = labels
  
  # to store the result of each CI test
  pval.mat = array(NA, dim=c(length(ys), length(ys)), dimnames=list(label=labels, label=labels))
  
#   # trick remove power rule if any to make sure CI tests are performed
#   if (!is.null(x$ci.method$test.args$power.rule)) {
#     x$ci.method$test.args$power.rule = NULL
#   }
  
  t = proc.time()
  
  # step 1 perform two conditional independence tests for each label pair
  pval.mat[labels, labels] = vapply(1:length(ys), function(y.i) {
    vapply(1:length(ys), function(y.j) {
      
      if (y.i == y.j) {
        return(NA)
      }
      
      a = ci.test(
        x=labels[y.i], y=labels[y.j], z=features[mb.mat[y.i, ]], data=train,
        test=x$ci.method$test, test.args=x$ci.method$test.args)$p.value
      
      return(a)
      
    }, numeric(1))
  }, numeric(length(ys)))
  
  ci.mat = pval.mat[, ] <= x$ci.method$alpha
  ci.mat[matrix(rep(1:length(ys), 2), ncol=2)] = TRUE
  
  # step 2 AND / OR filtering
  ilf = list(
    "OR" = ci.mat | t(ci.mat),
    "AND" = ci.mat & t(ci.mat))
  
  # step 3 decompose the label set into (irreducible) label factors, and their (minimal) feature subsets
  ilf = lapply(ilf, function(ci.mat) {
    
    lfs = list()
    fss = list()
    y.done = rep(FALSE, length(ys))
    
    while(any(!y.done)) {
      
      # pick any remaining label
      y.i = which(!y.done)[1]
      
      # breadth-first search to find a connected component
      while(any(ci.mat[y.i, ] & !y.done)) {
        
        y.j = which(ci.mat[y.i, ] & !y.done)[1]
        
        y.done[y.j] = TRUE
        ci.mat[y.i, ci.mat[y.j, ]] = TRUE
      }
      
      lf.i = length(lfs)+1
      lfs[[lf.i]] = labels[ci.mat[y.i, ]]
      fss[[lf.i]] = features[apply(mb.mat[ci.mat[y.i, ], , drop=FALSE], 2, any)]
    }
    
    return(list(lfs=lfs, fss=fss))
  })
  
  t = proc.time() - t
  
  save(pval.mat, ci.mat, ilf, t, file=out.file)
  
  write.log(sprintf("ILF - %s - %s / %s - r%02i s%02i - done", x$db, x$mb.method.desc, x$ci.method.desc, x$rep, x$split), t)
  
}#RUN

todo = list()
for (db in conf.dbs) {
  for (mb in conf.fss) {
    for (ci in conf.labels.ci) {
      for (r in 1:conf.nb.cv.reps) {
        for (s in 1:conf.nb.cv) {
          
          todo[[length(todo) + 1]] = list(
            db = db,
            rep = r,
            split = s,
            mb.method.desc = mb,
            ci.method.desc = ci,
            ci.method = conf.labels.ci.methods[[ci]]
          )
        }
      }
    }
  }
}

if(length(todo) > 0) {
  cl = makeSOCKcluster(conf.nb.cores)
  clusterEvalQ(cl, source("00.includes.R"))
  clusterEvalQ(cl, library("bnlearn"))
  clusterExport(cl, "conf.log")
  clusterExport(cl, "conf.seed")
  clusterExport(cl, "conf.nb.cv")
  if (conf.log) {
    clusterEvalQ(cl, library("synchronicity"))
    clusterExport(cl, "log.file")
    clusterExport(cl, "log.mutex")
  }
  clusterApplyLB(cl, todo, run)
  stopCluster(cl)
}
