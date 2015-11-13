library("bnlearn")

filters = as.array(c("or", "and"))
names(filters) = filters

for (db in conf.dbs) {
  
  in.data.file = sprintf("data/rda/%s/%s.data.rda", db, db)
  in.labels.file = sprintf("data/rda/%s/%s.labels.rda", db, db)
  
  load(in.data.file)
  load(in.labels.file)
  
  labels.origin = as.array(labels)
  names(labels.origin) = labels.origin
  
  ys = vapply(labels.origin, function(l) {which(colnames(data) == l)}, integer(1))
  xs = (1:ncol(data))[-ys]
  
  for (mb.m in conf.fss) {
    
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
        in.mb.file = sprintf("mbs.in.x/%s/%s/%s.cv%02i.r%02i.s%02i.rda", mb.m, db, db, conf.nb.cv, rep, split)
        
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
    
    for (ci.m in conf.labels.ci) {
      
      label.lf.size = array(NA,
        dim = c(length(labels.origin), conf.nb.cv.reps, conf.nb.cv, length(filters)),
        dimnames = list(label = labels.origin, rep = 1:conf.nb.cv.reps, split = 1:conf.nb.cv, filter = filters))
      
      ilf.t = array(NA,
        dim = c(conf.nb.cv.reps, conf.nb.cv),
        dimnames = list(rep = 1:conf.nb.cv.reps, split = 1:conf.nb.cv))
      
      # Load LF sizes
      for (rep in 1:conf.nb.cv.reps) {
        for (split in 1:conf.nb.cv) {
          
          in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", db, db, conf.nb.cv, rep)
          in.dec.file = sprintf("ilf/%s/%s/%s/%s.cv%02i.r%02i.s%02i.rda", mb.m, ci.m, db, db, conf.nb.cv, rep, split)
          
          if (!file.exists(in.dec.file)) {
            break
          }
          
          load(in.cv.file)
          load(in.dec.file)
          
          ilf.t[rep, split] = t["user.self"] + t["user.child"]
          
          for (f in filters) {
            
            lf.size = vapply(ilf[[toupper(f)]]$lfs, length, integer(1))
            
            label.lf.size[labels.origin, rep, split, f] = lf.size[apply(labels.origin, 1, function(l) {
              which(vapply(ilf[[toupper(f)]]$lfs, function(lf) {
                l %in% lf
              }, logical(1)))
            })]
          }
        }
      }
      
      for (f in filters) {
        
        # Plot labels LF size global distribution
        if (!anyNA(label.lf.size[, , , f])) {
          
          out.dir = sprintf("figures/labels.lf.size.ilf-%s/%s/%s", f, mb.m, ci.m)
          dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
          
          out.file = sprintf("%s/%s.global.cv%02i.reps%02i", out.dir, db, conf.nb.cv, conf.nb.cv.reps)
          
          if (conf.plot == "png") {
            png(sprintf("%s.png", out.file), width = 2*480, height = 2*480)
          }
          else if (conf.plot == "eps") {
            postscript(sprintf("%s.eps", out.file), horizontal=FALSE, pointsize=1/(1200), paper="special", width=2, height=2)
          }
          
          par(cex = 1)
#           par(cex = 1.5, mfcol = c(2, 1))
          
#           main = sprintf("%s (%i labels %i feats) - global %02i cv x %02i reps\n%s - %0.2f secs per label (min %0.2f max %0.2f)",
#             db, length(ys), length(xs), conf.nb.cv, conf.nb.cv.reps,
#             mb.m, mean(mb.t), min(mb.t), max(mb.t))
#           
#           hist(x = mb.size, breaks = seq(from=0, to=max(mb.size, na.rm = TRUE), by=1),
#             xlab = "MB size", ylab = "density",
#             main = main)
          
          main = sprintf("%s ilf-%s (%i labels %i feats) - global %02i cv x %02i reps\n%s / %s - %0.2f secs (min %0.2f max %0.2f)",
            db, f, length(ys), length(xs), conf.nb.cv, conf.nb.cv.reps,
            mb.m, ci.m, mean(ilf.t), min(ilf.t), max(ilf.t))
          
          hist(x = label.lf.size[, , , f]
            ,xlim = c(0, max(label.lf.size[, , , f]))
            ,breaks = if(length(unique(as.integer(label.lf.size[, , , f]))) == 1) {
              seq(from=0, to=max(label.lf.size[, , , f]), by=1)
            } else {"sturges"}
#             ,breaks = seq(from=0, to=length(labels.origin), by=1)
            ,xlab = "LF size", ylab = "density"
            ,main = ""
#             ,main = main
            )
          
          dev.off()
          
          write.log(sprintf("PLOT labels.lf.size.dist - %s - %s / %s - global %02i cv x %02i reps - done", db, mb.m, ci.m, conf.nb.cv, conf.nb.cv.reps))
        }
        
        # Plot labels LF size distribution for each run
        for (rep in 1:conf.nb.cv.reps) {
          for (split in 1:conf.nb.cv) {
            
            if (!anyNA(label.lf.size[, rep, split, f])) {
              
              out.dir = sprintf("figures/labels.lf.size.ilf-%s/%s/%s/%s", f, mb.m, ci.m, db)
              dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
              
              out.file = sprintf("%s/%s.cv%02i.r%02i.s%02i", out.dir, db, conf.nb.cv, rep, split)
              
              if (conf.plot == "png") {
                png(sprintf("%s.png", out.file), width = 2*480, height = 2*480)
              }
              else if (conf.plot == "eps") {
                postscript(sprintf("%s.eps", out.file), horizontal=FALSE, pointsize=1/(1200), paper="special", width=10, height=10)
              }
              par(cex = 1.5, mfcol = c(2, 1))
              
              main = sprintf("%s (%i labels %i feats) - cv%02i r%02i s%02i\n%s - %0.2f secs per label (min %0.2f max %0.2f)",
                db, length(ys), length(xs), conf.nb.cv, rep, split,
                mb.m, mean(mb.t[, rep, split]), min(mb.t[, rep, split]), max(mb.t[, rep, split]))
              
              hist(x = mb.size[, rep, split], breaks = seq(from=0, to=max(mb.size, na.rm = TRUE), by=1),
                xlab = "MB size", ylab = "density",
                ylim = c(0, max(apply(mb.size, c(2, 3), function(s){max(tabulate(s))}))),
                main = main)
              
              main = sprintf("%s ilf-%s (%i labels %i feats) - cv%02i r%02i s%02i\n%s / %s - %0.2f secs",
                db, f, length(ys), length(xs), conf.nb.cv, rep, split,
                mb.m, ci.m, ilf.t[rep, split])
              
              hist(x = label.lf.size[, rep, split, f], breaks = seq(from=0, to=length(labels.origin), by=1),
                xlab = "LF size", ylab = "density",
                ylim = c(0, max(apply(label.lf.size[, , , f], c(2, 3), function(s){max(tabulate(s))}))),
                main = main)
              
              dev.off()
              
              write.log(sprintf("PLOT labels.lf.size.dist - %s - %s / %s - r%02i s%02i - done", db, mb.m, ci.m, rep, split))
            }
          }
        }
      }
    }
  }
}
