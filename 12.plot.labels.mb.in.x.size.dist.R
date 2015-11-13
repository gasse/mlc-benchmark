
for (plot in conf.plot) {
  for (db in conf.dbs) {
    
    in.data.file = sprintf("data/rda/%s/%s.data.rda", db, db)
    in.labels.file = sprintf("data/rda/%s/%s.labels.rda", db, db)
    
    load(in.data.file)
    load(in.labels.file)
    
    labels.origin = as.array(labels)
    names(labels.origin) = labels.origin
    
    ys = vapply(labels.origin, function(l) {which(colnames(data) == l)}, integer(1))
    xs = (1:ncol(data))[-ys]
    
    for (m in conf.fss) {
      
      mb.size = array(NA,
        dim = c(length(labels.origin), conf.nb.cv.reps, conf.nb.cv),
        dimnames = list(label = labels.origin, rep = 1:conf.nb.cv.reps, split = 1:conf.nb.cv))
      
      mb.t = array(NA,
        dim = c(length(labels.origin), conf.nb.cv.reps, conf.nb.cv),
        dimnames = list(label = labels.origin, rep = 1:conf.nb.cv.reps, split = 1:conf.nb.cv))
      
      # Load MB sizes
      for (rep in 1:conf.nb.cv.reps) {
        for (split in 1:conf.nb.cv) {
          
          in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", db, db, conf.nb.cv, rep)
          in.mb.file = sprintf("mbs.in.x/%s/%s/%s.cv%02i.r%02i.s%02i.rda", m, db, db, conf.nb.cv, rep, split)
          
          if (!file.exists(in.mb.file)) {
            break
          }
          
          load(in.cv.file)
          load(in.mb.file)
          
          labels = labels.origin[labels.ord]
          
          mb.size[labels, rep, split] = apply(mb.mat, 1, sum)
          mb.t[labels, rep, split] = vapply(ts, function(t) {t["user.self"] + t["user.child"]}, numeric(1))
        }
      }
      
      # Plot labels MB size global distribution
      if (!anyNA(mb.size)) {
        
        out.dir = sprintf("figures/labels.mb.in.x.size/%s", m)
        dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
        
        out.file = sprintf("%s/%s.global.cv%02i.reps%02i", out.dir, db, conf.nb.cv, conf.nb.cv.reps)
        main = sprintf("%s (%i labels %i feats) - global %02i cv x %02i reps\n%s - %0.2f secs per label (min %0.2f max %0.2f)",
          db, length(ys), length(xs), conf.nb.cv, conf.nb.cv.reps,
          m, mean(mb.t), min(mb.t), max(mb.t))
        
        if (plot == "png") {
          png(sprintf("%s.png", out.file), width = 2*480, height = 2*480)
        }
        else if (plot == "eps") {
          postscript(sprintf("%s.eps", out.file), horizontal=FALSE, pointsize=1/(1200), paper="special", width=10, height=10)
        }
        par(cex = 1.5)
        hist(x = mb.size, breaks = seq(from=0, to=max(mb.size, na.rm = TRUE), by=1),
          xlab = "MB size", ylab = "density",
          main = main)
        dev.off()
        
        write.log(sprintf("PLOT labels.mb.size.dist - %s - %s - global %02i cv x %02i reps - done", db, m, conf.nb.cv, conf.nb.cv.reps))
      }
      
      # Plot labels MB size distribution for each run
      for (rep in 1:conf.nb.cv.reps) {
        for (split in 1:conf.nb.cv) {
          
          if (!anyNA(mb.size[, rep, split])) {
            
            out.dir = sprintf("figures/labels.mb.in.x.size/%s/%s", m, db)
            dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
            
            out.file = sprintf("%s/%s.cv%02i.r%02i.s%02i", out.dir, db, conf.nb.cv, rep, split)
            main = sprintf("%s (%i labels %i feats) - cv%02i r%02i s%02i\n%s - %0.2f secs per label (min %0.2f max %0.2f)",
              db, length(ys), length(xs), conf.nb.cv, rep, split,
              m, mean(mb.t[, rep, split]), min(mb.t[, rep, split]), max(mb.t[, rep, split]))
            
            if (plot == "png") {
              png(sprintf("%s.png", out.file), width = 2*480, height = 2*480)
            }
            else if (plot == "eps") {
              postscript(sprintf("%s.eps", out.file), horizontal=FALSE, pointsize=1/(1200), paper="special", width=10, height=10)
            }
            par(cex = 1.5)
            hist(x = mb.size[, rep, split], breaks = seq(from=0, to=max(mb.size, na.rm = TRUE), by=1),
              xlab = "MB size", ylab = "density",
              ylim = c(0, max(apply(mb.size, c(2, 3), function(s){max(tabulate(s))}))),
              main = main)
            dev.off()
            
            write.log(sprintf("PLOT labels.mb.size.dist - %s - %s - r%02i s%02i - done", db, m, rep, split))
          }
        }
      }
    }
  }
}
