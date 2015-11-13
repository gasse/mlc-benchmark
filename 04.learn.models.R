library("snow")

run = function(x) {
  
  set.seed(conf.seed)
  
  in.data.cont.file = sprintf("data/rda/%s/%s.data.cont.rda", x$db, x$db)
  in.labels.file = sprintf("data/rda/%s/%s.labels.rda", x$db, x$db)
  in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", x$db, x$db, conf.nb.cv, x$rep)
  
  load(in.data.cont.file)
  load(in.labels.file)
  load(in.cv.file)
  
  data = data.cont # data data.disc data.cont
  
  train = data[cv.splits != x$split, ]
  test = data[cv.splits == x$split, ]
  
  labels = labels[labels.ord]
  ys = vapply(labels, function(x) {which(colnames(train) == x)}, integer(1))
  xs = (1:ncol(train))[-ys]
  features = colnames(train)[xs]
  
  labels = as.array(labels)
  names(labels) = labels
  
  out.dir = sprintf("preds/%s/%s/%s", x$model$learner, x$model$desc, x$db)
  dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
  
  out.file = sprintf("%s/%s.cv%02i.r%02i.s%02i.rda", out.dir, x$db, conf.nb.cv, x$rep, x$split)
  
  t = proc.time()
  
  pred = test[, ys]
  
  # BR : learn each label separately
  if (x$model$meta == "br") {
    
    ts = vector("list", length(ys))
    names(ts) = labels
    
    for (y.i in 1:length(ys)) {
      
      y = ys[y.i]
      
      # merge y combinations into one multi-class variable
      y.mods = labels.modalities(data[, y, drop=FALSE])
      train.y = labels.aggregation(train[, y, drop=FALSE], y.mods)
      
      res = learn.model(train[, xs, drop=FALSE], train.y, test[, xs, drop=FALSE], x$model$learner)
      
      ts[[y.i]] = res$t
      pred[, y.i] = y.mods[as.integer(as.character(res$pred)), ]
      
      res = NULL # erf trick to release memory in the JVM
    }
    
  }
  # LP : learn the whole label set at once
  else if (x$model$meta == "lp") {
    
    ts = vector("list", 1)
    
    # merge ys combinations into one multi-class variable
    ys.mods = labels.modalities(data[, ys, drop=FALSE])
    train.y = labels.aggregation(train[, ys, drop=FALSE], ys.mods)
    
    res = learn.model(train[, xs, drop=FALSE], train.y, test[, xs, drop=FALSE], x$model$learner)
    
    ts[[1]] = res$t
    pred = ys.mods[as.integer(as.character(res$pred)), ]
    
    res = NULL # erf trick to release memory in the JVM
  }
  
  # ILF : learn each (irreducible) label factor separately
  else if(x$model$meta %in% c("ilf-or", "ilf-and")) {
    
    if (is.na(x$model$fss.method)) {
      stop("A FSS method must be specified with label factor decomposition.")
    }
    
    if (is.na(x$model$ci.method)) {
      stop("A CI method must be specified with label factor decomposition.")
    }
    
    in.ilf.file = sprintf("ilf/%s/%s/%s/%s.cv%02i.r%02i.s%02i.rda", x$model$fss.method, x$model$ci.method, x$db, x$db, conf.nb.cv, x$rep, x$split)
    
    t.back = t
    load(in.ilf.file)
    t = t.back
    
    lfs = switch(x$model$meta,
      "ilf-or" = ilf$OR$lfs,
      "ilf-and" = ilf$AND$lfs)
    
    ts = vector("list", length(lfs))
    
    for (lf.i in 1:length(lfs)) {
      
      lf = which(labels %in% lfs[[lf.i]])
      
      # merge lf combinations into one multi-class variable
      lf.mods = labels.modalities(data[, ys[lf], drop=FALSE])
      train.y = labels.aggregation(train[, ys[lf], drop=FALSE], lf.mods)
      
      res = learn.model(train[, xs, drop=FALSE], train.y, test[, xs, drop=FALSE], x$model$learner)
      
      ts[[lf.i]] = res$t
      pred[, vapply(ys[lf], function(y){which(ys == y)}, integer(1))] = lf.mods[as.integer(as.character(res$pred)), ]
      
      res = NULL # trick to release memory in the JVM (extraTrees)
    }
  }
  else {
    stop(sprintf("unsupported meta learner: %s", x$model$meta))
  }
  
  t = proc.time() - t
  
  save(pred, ts, t, file=out.file)
  
  write.log(sprintf("PRED - %s - %s %s (%s %s) - r%02i s%02i - done", x$db, x$model$meta, x$model$learner, x$model$fss.method, x$model$ci.method, x$rep, x$split), t)
}#RUN

todo = list()
for (db in conf.dbs) {
  for (meta.lrnr.i in 1:nrow(conf.meta.learners)) {
    for (r in 1:conf.nb.cv.reps) {
      for (s in 1:conf.nb.cv) {
        
        todo[[length(todo) + 1]] = list(
          db = db,
          rep = r,
          split = s,
          model = as.list(conf.meta.learners[meta.lrnr.i, ])
        )
      }
    }
  }
}

if(length(todo) > 0) {
  cl = makeSOCKcluster(conf.nb.cores)
  clusterEvalQ(cl, source("00.includes.R"))
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
