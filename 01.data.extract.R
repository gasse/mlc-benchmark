library("XML") # xmlSApply xmlRoot xmlParse xmlAttrs
library("bnlearn") # discretize
library("R.matlab") # readMat
library("RWeka") # write.arff

# ARFF to RDA
for (db in setdiff(conf.dbs, paste(conf.dbs.x2, "2", sep=""))) {
  
  set.seed(conf.seed)
  
  arff.file = sprintf("data/arff/%s/%s.arff", db, db)
  xml.file = sprintf("data/arff/%s/%s.xml", db, db)
  
  out.dir = sprintf("data/rda/%s", db)
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  
  out.labels.file = sprintf("%s/%s.labels.rda", out.dir, db)
  out.data.file = sprintf("%s/%s.data.rda", out.dir, db)
  out.data.disc.file = sprintf("%s/%s.data.disc.rda", out.dir, db)
  out.data.cont.file = sprintf("%s/%s.data.cont.rda", out.dir, db)
  
  all.done = TRUE
  
  if (!file.exists(out.labels.file) ||
      !file.exists(out.data.file) ||
      !file.exists(out.data.disc.file) ||
      !file.exists(out.data.cont.file)) {
    all.done = FALSE
  }
  for (i in 1:conf.nb.cv.reps) {
    out.cv.file = sprintf("%s/%s.cv.splits.%02i.r%02i.rda", out.dir, db, conf.nb.cv, i)
    if(!file.exists(out.cv.file)) {
      all.done = FALSE
    }
  }
  
  if (all.done) {
    write.log(sprintf("%s - skipped", db))
    next
  }
  
  t = proc.time()
  
  # Image must be converted from matlab to arff format first
  if (db == "image") {
    m = readMat("data/arff/image/miml data.mat", verbose=FALSE, fixNames=TRUE)
    
    # features
    x = t(vapply(m$bags, function(x){as.vector(x[[1]])}, double(135)))
    x = as.data.frame(x)
    
    # labels
    y = t(m$targets)
    y[y==-1] = 0 # -1/1 -> 0/1
    y = matrix(as.character(y), ncol = ncol(y))
    y = as.data.frame(y)
    
    # label names
    targets = vapply(m$class.name, function(x){c(x[[1]])}, character(1))
    
    df = cbind(x, y)
    colnames(df) = c(paste("X", 1:135, sep=""), targets)
    rownames(df) = NULL
    
    write.arff(df, "data/arff/image/image.arff")
  }
  
  # recover label names
  labels = as.character(xmlSApply(xmlRoot(xmlParse(xml.file)), function(x){xmlAttrs(x)["name"]}))
  save(labels, file=out.labels.file)
  
  data = read.arff(arff.file)
  # Genbase "protein" column is a unique identifier (662 != values for 662 obs)
  if (db == "genbase") {
    data = data[, -1]
  }
  # Slashdot values are binary factors but encoded as numeric
  if (db == "slashdot") {
    data = data.frame(lapply(data, factor, levels=c(0, 1)))
  }
  save(data, file=out.data.file)
  
  # transform everything to factor
  data.disc = discretize(data, method="quantile", breaks=2)
  save(data.disc, file=out.data.disc.file)
  
  # transform everything (but labels) to numeric
  data.cont = data
  for (col in setdiff(colnames(data.cont), labels)) {
    if (!is.numeric(data.cont[, col])) {
      data.cont[, col] = as.numeric(data.cont[, col])
    }
  }
  save(data.cont, file=out.data.cont.file)
  
  for (i in 1:conf.nb.cv.reps) {
    
    out.cv.file = sprintf("%s/%s.cv.splits.%02i.r%02i.rda", out.dir, db, conf.nb.cv, i)
    
    cv.splits = sample(rep(1:conf.nb.cv, length.out = nrow(data)))
    labels.ord = sample(length(labels))
    
    save(cv.splits, labels.ord, file = out.cv.file)
  }
  
  t = proc.time() - t
  
  write.log(sprintf("%s - done", db), t)
}

# Twin data sets
for (db in conf.dbs.x2) {
  
  set.seed(conf.seed)
  
  db2 = sprintf("%s2", db)
  
  in.dir = sprintf("data/rda/%s", db)
  
  in.labels.file = sprintf("%s/%s.labels.rda", in.dir, db)
  in.data.file = sprintf("%s/%s.data.rda", in.dir, db)
  in.data.disc.file = sprintf("%s/%s.data.disc.rda", in.dir, db)
  in.data.cont.file = sprintf("%s/%s.data.cont.rda", in.dir, db)
  
  out.dir = sprintf("data/rda/%s", db2)
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  
  out.labels.file = sprintf("%s/%s.labels.rda", out.dir, db2)
  out.data.file = sprintf("%s/%s.data.rda", out.dir, db2)
  out.data.disc.file = sprintf("%s/%s.data.disc.rda", out.dir, db2)
  out.data.cont.file = sprintf("%s/%s.data.cont.rda", out.dir, db2)
  
  
  all.done = TRUE
  
  if (!file.exists(out.labels.file) ||
      !file.exists(out.data.file) ||
      !file.exists(out.data.disc.file) ||
      !file.exists(out.data.cont.file)) {
    all.done = FALSE
  }
  for (i in 1:conf.nb.cv.reps) {
    out.cv.file = sprintf("%s/%s.cv.splits.%02i.r%02i.rda", out.dir, db2, conf.nb.cv, i)
    if(!file.exists(out.cv.file)) {
      all.done = FALSE
    }
  }
  
  if (all.done) {
    write.log(sprintf("%s - skipped", db2))
    next
  }
  
  t = proc.time()
  
  load(in.labels.file)
  load(in.data.file)
  load(in.data.disc.file)
  load(in.data.cont.file)
  
  labels = colnames(data[, c(labels, labels)])
  save(labels, file=out.labels.file)
  
  s = sample(nrow(data))
  
  data = cbind(data, data[s, ])
  colnames(data) = colnames(data[, colnames(data)]) # trick
  save(data, file=out.data.file)
  
  data.disc = cbind(data.disc, data.disc[s, ])
  colnames(data.disc) = colnames(data.disc[, colnames(data.disc)])
  save(data.disc, file=out.data.disc.file)
  
  data.cont = cbind(data.cont, data.cont[s, ])
  colnames(data.cont) = colnames(data.cont[, colnames(data.cont)])
  save(data.cont, file=out.data.cont.file)
  
  for (i in 1:conf.nb.cv.reps) {
    
    out.cv.file = sprintf("%s/%s.cv.splits.%02i.r%02i.rda", out.dir, db2, conf.nb.cv, i)
    
    cv.splits = sample(rep(1:conf.nb.cv, length.out = nrow(data)))
    labels.ord = sample(length(labels))
    
    save(cv.splits, labels.ord, file = out.cv.file)
  }
  
  t = proc.time() - t
  
  write.log(sprintf("%s - done", db2), t)
}
