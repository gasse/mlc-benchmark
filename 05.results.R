library("bnlearn")

results = NULL
for (db in conf.dbs) {
  for (meta.lrnr.i in 1:nrow(conf.meta.learners)) {
    
    desc = conf.meta.learners[meta.lrnr.i, "desc"]
    meta = conf.meta.learners[meta.lrnr.i, "meta"]
    learner = conf.meta.learners[meta.lrnr.i, "learner"]
    fss.method = conf.meta.learners[meta.lrnr.i, "fss.method"]
    ci.method = conf.meta.learners[meta.lrnr.i, "ci.method"]
    
    in.data.file = sprintf("data/rda/%s/%s.data.rda", db, db)
    
    load(in.data.file)
    
    data = data # data data.disc data.cont
    
    for (rep in 1:conf.nb.cv.reps) {
      
      in.labels.file = sprintf("data/rda/%s/%s.labels.rda", db, db)
      in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", db, db, conf.nb.cv, rep)
      
      load(in.labels.file)
      load(in.cv.file)
      
      labels = labels[labels.ord]
      ys = vapply(labels, function(x) {which(colnames(data) == x)}, integer(1))
      xs = (1:ncol(data))[-ys]
      features = colnames(data)[xs]
      
      labels = as.array(labels)
      names(labels) = labels
      
      ys = apply(array(labels), 1, function(x) {which(colnames(data) == x)})
      xs = (1:ncol(data))[-ys]
      
      for (split in 1:conf.nb.cv) {
        
        t.split = proc.time()
        
        pred.file = sprintf("preds/%s/%s/%s/%s.cv%02i.r%02i.s%02i.rda", learner, desc, db, db, conf.nb.cv, rep, split)
        
        if (!file.exists(pred.file)) {
          
          results = rbind(results, data.frame(
            db = db,
            meta = meta,
            learner = learner,
            fss.method = fss.method,
            ci.method = ci.method,
            rep = rep,
            split = split,
            
            acc = NA,
            acc.sd = NA,
            rec = NA,
            rec.sd = NA,
            prec = NA,
            prec.sd = NA,
            f1 = NA,
            f1.sd = NA,
            gacc = NA
          ))
          
          write.log(sprintf("RESULTS %s - %s %s - %s / %s - r%02i s%02i (skipped)", db, meta, learner, fss.method, ci.method, rep, split))
          next
        }
        
        load(pred.file)
        true = data.matrix(data[cv.splits == split, ys, drop=FALSE])
        pred = data.matrix(pred[, colnames(data)[ys]])
        
        tmp = pred.evaluation(true, pred)
        
        results = rbind(results, data.frame(
          db = db,
          meta = meta,
          learner = learner,
          fss.method = fss.method,
          ci.method = ci.method,
          rep = rep,
          split = split
          
          ,acc = mean(tmp[, "accuracy"])
          ,acc.sd = sd(tmp[, "accuracy"])
          ,rec = mean(tmp[, "recall"])
          ,rec.sd = sd(tmp[, "recall"])
          ,prec = mean(tmp[, "precision"])
          ,prec.sd = sd(tmp[, "precision"])
          ,f1 = mean(tmp[, "f1"])
          ,f1.sd = sd(tmp[, "f1"])
          ,gacc = tmp[1, "global.accuracy"]
        ))
        
        t.split = proc.time() - t.split
        
        write.log(sprintf("RESULTS %s - %s %s - %s / %s - r%02i s%02i - done", db, meta, learner, fss.method, ci.method, rep, split), t.split)
      }
    }
  }
}

results = results[order(results$meta), ]
results = results[order(results$db), ]

save(results, file = "results.rda")
