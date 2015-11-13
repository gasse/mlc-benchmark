source("00.includes.R")

conf.seed = 1
conf.nb.cores = 2

conf.nb.cv = 2
conf.nb.cv.reps = 5

conf.fss = "iamb.sp-mi.pr0.a0.0001" # "fdr.iamb.mi.pr0.a0.0001"
conf.labels.ci = "ci.sp-mi.pr0.a0.0001" # "ci.sp-mi.pr0.a0.0001"

conf.base.learner = "rf"

conf.plot = "png" # c("png", "eps")

conf.dbs = c(NULL  
  ,"emotions"
  ,"image"
  ,"scene"
#   ,"yeast"
#   ,"slashdot"
#   ,"genbase"
#   ,"medical"
#   ,"enron"
#   ,"bibtex"
#   ,"corel5k"
)

conf.dbs.x2 = c(NULL
  ,"emotions"
  ,"image"
  ,"scene"
#   ,"yeast"
#   ,"slashdot"
#   ,"genbase"
#   ,"medical"
#   ,"enron"
#   ,"bibtex"
#   ,"corel5k"
)

if(length(conf.dbs.x2) > 0) {
  conf.dbs = c(conf.dbs, paste(conf.dbs.x2, "2", sep=""), NULL)
}

stopifnot(all(conf.fss %in% names(conf.fss.methods)))
stopifnot(all(conf.labels.ci %in% names(conf.labels.ci.methods)))

conf.log = TRUE
if (conf.log) {
  t = format(Sys.time(), "%Y%m%d_%H%M%S")
  log.file = sprintf("%s.log", t)
  log.mutex = sprintf("mlc-benchmark.main.%s", t)
}

conf.meta.learners = rbind(
  NULL
  
  ,data.frame(
    desc = "BR",
    meta = "br",
    learner = conf.base.learner,
    fss.method = NA,
    ci.method = NA,
    stringsAsFactors = FALSE)
  
  ,data.frame(
    desc = "LP",
    meta = "lp",
    learner = conf.base.learner,
    fss.method = NA,
    ci.method = NA,
    stringsAsFactors = FALSE)
  
  ,data.frame(
    desc = sprintf("ILF-OR %s - %s", conf.fss, conf.labels.ci),
    meta = "ilf-or",
    learner = conf.base.learner,
    fss.method = conf.fss,
    ci.method = conf.labels.ci,
    stringsAsFactors = FALSE)
  
  ,data.frame(
    desc = sprintf("ILF-AND %s - %s", conf.fss, conf.labels.ci),
    meta = "ilf-and",
    learner = conf.base.learner,
    fss.method = conf.fss,
    ci.method = conf.labels.ci,
    stringsAsFactors = FALSE)
)

write.log(paste("conf.dbs:", paste(conf.dbs, collapse=" ")), ts=FALSE)
write.log(paste("conf.fss:", paste(conf.fss, collapse=" ")), ts=FALSE)
write.log(paste("conf.labels.ci:", paste(conf.labels.ci, collapse=" ")), ts=FALSE)
write.log(paste("conf.base.learner:", conf.base.learner), ts=FALSE)
write.log(paste("conf.seed:", conf.seed), ts=FALSE)
write.log(paste("conf.nb.cores:", conf.nb.cores), ts=FALSE)
write.log(paste("conf.nb.cv:", conf.nb.cv), ts=FALSE)
write.log(paste("conf.nb.cv.reps:", conf.nb.cv.reps), ts=FALSE)
write.log("", ts=FALSE)


write.log("EXTRACT.DATA")
source("01.data.extract.R")

write.log("EXTRACT.MBS.IN.X")
source("02.extract.mbs.in.x.R")

write.log("EXTRACT.ILF")
source("03.extract.ilf.R")

write.log("LEARN.MODELS")
source("04.learn.models.R")

write.log("MAKE.RESULTS")
source("05.results.R")


load("results.rda")

# Hamming loss
hloss = aggregate(1 - results[, "acc"], by = results[, c("meta", "db"), drop=F], FUN = mean)
array(hloss$x,
      dim = c(nlevels(hloss$meta), nlevels(hloss$db)),
      dimnames = lapply(hloss[, c("meta", "db")], levels))

# Subset 0-1 loss
hloss = aggregate(1 - results[, "gacc"], by = results[, c("meta", "db"), drop=F], FUN = mean)
array(hloss$x,
      dim = c(nlevels(hloss$meta), nlevels(hloss$db)),
      dimnames = lapply(hloss[, c("meta", "db")], levels))


write.log("PLOT.ILF.GRAPH")
source("11.plot.ilf.graph.R")

write.log("PLOT.LABELS.MB.IN.X.SIZE.DIST")
source("12.plot.labels.mb.in.x.size.dist.R")

write.log("PLOT.LABELS.LF.SIZE.DIST")
source("13.plot.labels.lf.size.dist.R")
