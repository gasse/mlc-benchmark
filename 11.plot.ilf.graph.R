library("bnlearn")

for (db in conf.dbs) {
  
  in.labels.file = sprintf("data/rda/%s/%s.labels.rda", db, db)
  
  load(in.labels.file)
  
  labels.origin = as.array(labels)
  names(labels.origin) = labels.origin
  graph.empty = model2network(paste("[", labels.origin, "]", sep="", collapse=""))
  
  for (mb.m in conf.fss) {
    for (ci.m in conf.labels.ci) {
      for (rep in 1:conf.nb.cv.reps) {
        for (split in 1:conf.nb.cv) {
          
          in.cv.file = sprintf("data/rda/%s/%s.cv.splits.%02i.r%02i.rda", db, db, conf.nb.cv, rep)
          in.dec.file = sprintf("ilf/%s/%s/%s/%s.cv%02i.r%02i.s%02i.rda", mb.m, ci.m, db, db, conf.nb.cv, rep, split)
          
          if (!file.exists(in.dec.file)) {
            break
          }
          
          load(in.cv.file)
          load(in.dec.file)
          
          labels = labels.origin[labels.ord]
          
          ci.mat = list(
            "or" = ci.mat | t(ci.mat),
            "and" = ci.mat & t(ci.mat)
          )
          
          pval.mat = list(
            "or" = pmin(pval.mat, t(pval.mat)),
            "and" = pmax(pval.mat, t(pval.mat))
          )
          
          for (f in c("or", "and")) {
            
            t = proc.time()
            
            out.dir = sprintf("figures/graph.ilf-%s/%s/%s/%s", f, mb.m, ci.m, db)
            dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
            
            out.file = sprintf("%s/%s.cv%02i.r%02i.s%02i", out.dir, db, conf.nb.cv, rep, split)
            
            graph = graph.empty
            nb.edges = sum(ci.mat[[f]]) - length(labels)
            
            strength = data.frame(
              from = factor(rep(NA, nb.edges), levels=labels.origin),
              to = factor(rep(NA, nb.edges), levels=labels.origin),
              strength = numeric(nb.edges))
            
            s.i = 0
            for (i in seq_along(labels)) {
              for (j in seq_along(labels)[-i]) {
                if (ci.mat[[f]][i, j]) {
                  graph = set.arc(graph, labels[i], labels[j])
                  s.i = s.i + 1
                  strength[s.i, "from"] = labels[i]
                  strength[s.i, "to"] = labels[j]
                  strength[s.i, "strength"] = pval.mat[[f]][i, j]
                }
              }
            }
            
            class(strength) = c(class(strength), "bn.strength")
            attr(strength, "mode") = "test"
            attr(strength, "threshold") = conf.labels.ci.methods[[ci.m]]$alpha
            
            graph = skeleton(graph)
            
            main = sprintf("ilf-%s - %s\n%s / %s", f, db, mb.m, ci.m)
            
            if (plot == "png") {
              png(sprintf("%s.png", out.file), width = 2*480, height = 2*480)
            }
            else if (plot == "eps") {
              postscript(sprintf("%s.eps", out.file), horizontal=FALSE, pointsize=1/(1200), paper="special", width=10, height=10)
            }
            
#             p = graph.par()
            # graph.par(list(edges = list(lwd = 4), nodes = list(lwd = 0.1, fontsize = 14)))
            if (nrow(strength) > 0) {
              strength.plot(graph, strength, shape="ellipse", layout="fdp", main=main)
            }
            else {
              graphviz.plot(graph, shape="ellipse", layout="fdp", main=main)
            }
#             graph.par(p)
            dev.off()
            
            t = proc.time() - t
            
            write.log(sprintf("PLOT ci.graph %s - %s / %s r%02i s%02i - ilf-%s - done", db, mb.m, ci.m, rep, split, f), t)
          }
        }
      }
    }
  }
}
