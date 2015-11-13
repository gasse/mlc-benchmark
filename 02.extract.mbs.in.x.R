library("snow")

run = function(x) {
  
  set.seed(conf.seed)
  
  in.data.disc.file = sprintf("data/rda/%s/%s.data.disc.rda", x$db, x$db)
  in.labels.file = sprintf("data/rda/%s/%s.labels.rda", x$db, x$db)
  in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", x$db, x$db, conf.nb.cv, x$rep)
  
  out.mbs.dir = sprintf("mbs.in.x/%s/%s", x$method.desc, x$db)
  dir.create(out.mbs.dir, showWarnings=FALSE, recursive=TRUE)
  
  out.file = sprintf("%s/%s.cv%02i.r%02i.s%02i.rda", out.mbs.dir, x$db, conf.nb.cv, x$rep, x$split)
  
  load(in.data.disc.file)
  load(in.labels.file)
  load(in.cv.file)
  
  train = data.disc[cv.splits != x$split, ]
  labels = labels[labels.ord]
  ys = vapply(labels, function(x) {which(colnames(train) == x)}, integer(1))
  xs = (1:ncol(train))[-ys]
  features = colnames(train)[xs]
  
  labels = as.array(labels)
  names(labels) = labels
  
  # to store individual Markov boundaries
  mb.mat = array(FALSE, dim=c(length(ys), length(xs)), dimnames=list(label=labels, feature=features))
  
  # to store execution times
  ts = vector("list", 0)
  ts[labels] = list(system.time(NULL))
  
  t = proc.time()
  
  for (y.i in 1:length(labels)) {
    
    label = labels[y.i]
    
    ts[[label]] = proc.time()
    
    mb = switch(x$method,
      
      "k.iamb" = learn.mb(
        x=train[, c(ys[y.i], xs)], node=labels[y.i],
        method="k.iamb", k=x$method.args$k,
        test=x$method.args$test,
        alpha=x$method.args$alpha,
        test.args=x$method.args$test.args),
      
      "iamb" = learn.mb(
        x=train[, c(ys[y.i], xs)], node=labels[y.i],
        method="iamb",
        test=x$method.args$test,
        alpha=x$method.args$alpha,
        test.args=x$method.args$test.args),
      
      "fdr.iamb" = learn.mb(
        x=train[, c(ys[y.i], xs)], node=labels[y.i],
        method="fdr.iamb",
        test=x$method.args$test,
        alpha=x$method.args$alpha,
        test.args=x$method.args$test.args),
      
      stop(sprintf("unsupported FSS method: %s", x$method))
    )
    
    mb.mat[y.i, which(colnames(train)[xs] %in% mb)] = TRUE
    
    ts[[label]] = proc.time() - ts[[label]]
  }
  
  t = proc.time() - t
  
  save(mb.mat, ts, file=out.file)
  
  write.log(sprintf("MB in X - %s - %s r%02i s%02i - done", x$db, x$method.desc, x$rep, x$split), t)
  
}#RUN

todo = list()
for (db in conf.dbs) {
  for (m in conf.fss) {
    for (r in 1:conf.nb.cv.reps) {
      for (s in 1:conf.nb.cv) {
        
        todo[[length(todo) + 1]] = list(
          db = db,
          rep = r,
          split = s,
          method = conf.fss.methods[[m]]$method,
          method.desc = m,
          method.args = conf.fss.methods[[m]]$args
        )
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
