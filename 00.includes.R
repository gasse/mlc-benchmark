require("synchronicity")

conf.fss.methods = list(
  "fdr.iamb.mi.pr5.a0.01" = list(
    method = "fdr.iamb",
    args = list(
      alpha = 0.01,
      test = "mi", test.args = list(power.rule=5, df.adjust=FALSE)
    )
  ),
  "fdr.iamb.mi.pr0.a0.05" = list(
    method = "fdr.iamb",
    args = list(
      alpha = 0.05,
      test = "mi", test.args = list(power.rule=0, df.adjust=FALSE)
    )
  ),
  "iamb.mi.pr5.dfa.a0.01" = list(
    method = "iamb",
    args = list(
      alpha = 0.01,
      test = "mi", test.args = list(power.rule=5, df.adjust=TRUE)
    )
  ),
  "iamb.mi.pr5.dfa.a0.0001" = list(
    method = "iamb",
    args = list(
      alpha = 0.0001,
      test = "mi", test.args = list(power.rule=5, df.adjust=TRUE)
    )
  ),
  "iamb.mi.pr0.dfa.a0.001" = list(
    method = "iamb",
    args = list(
      alpha = 0.001,
      test = "mi", test.args = list(power.rule=0, df.adjust=TRUE)
    )
  ),
  "iamb.sp-mi.pr5.a0.01" = list(
    method = "iamb",
    args = list(
      alpha = 0.01,
      test = "sp-mi", test.args = list(power.rule=5, B=100)
    )
  ),
  "iamb.sp-mi.pr0.a0.0001" = list(
    method = "iamb",
    args = list(
      alpha = 0.0001,
      test = "sp-mi", test.args = list(B=100)
    )
  ),
  "iamb.sp-mi.pr0.a0.001" = list(
    method = "iamb",
    args = list(
      alpha = 0.001,
      test = "sp-mi", test.args = list(power.rule=0, B=100)
    )
  )
)

conf.labels.ci.methods = list(
  "ci.mi.pr0.a0.0001" = list(
    test = "mi",
    alpha = 0.0001,
    test.args = list(power.rule=0, df.adjust=FALSE)
  ),
  "ci.mi.pr0.a0.005" = list(
    test = "mi",
    alpha = 0.005,
    test.args = list(power.rule=0, df.adjust=FALSE)
  ),
  "ci.mi.pr0.a0.001" = list(
    test = "mi",
    alpha = 0.001,
    test.args = list(power.rule=0, df.adjust=FALSE)
  ),
  "ci.mi.pr0.dfa.a0.01" = list(
    test = "mi",
    alpha = 0.01,
    test.args = list(power.rule=0, df.adjust=TRUE)
  ),
  "ci.mi.pr0.dfa.a0.005" = list(
    test = "mi",
    alpha = 0.005,
    test.args = list(power.rule=0, df.adjust=TRUE)
  ),
  "ci.mi.pr0.dfa.a0.001" = list(
    test = "mi",
    alpha = 0.001,
    test.args = list(power.rule=0, df.adjust=TRUE)
  ),
  "ci.mi.pr0.dfa.a0.0001" = list(
    test = "mi",
    alpha = 0.0001,
    test.args = list(power.rule=0, df.adjust=TRUE)
  ),
  "ci.sp-mi.pr0.a0.0001" = list(
    test = "sp-mi",
    alpha = 0.0001,
    test.args = list(power.rule=0, B=100)
  ),
  "ci.sp-mi.pr0.a0.01" = list(
    test = "sp-mi",
    alpha = 0.01,
    test.args = list(power.rule=0, B=100)
  ),
  "ci.sp-mi.pr0.a0.001" = list(
    test = "sp-mi",
    alpha = 0.001,
    test.args = list(power.rule=0, B=100)
  )
)

write.log = function(trace, t.elapsed=NULL, ts=TRUE) {
  
  t = format(Sys.time(), "%Y.%m.%d_%H:%M:%S")
  file = ""
  
  if (exists("conf.log") && conf.log) {
    
    require("synchronicity")
    
    m = boost.mutex(log.mutex)
    lock(m)
    dir.create("log", recursive = TRUE, showWarnings = FALSE)
    file = sprintf("log/%s", log.file)
  }
  
  if (ts) {
    cat(sprintf("%s - ", t), file=file, append=TRUE)
  }
  
  cat(sprintf("%s", trace), file=file, append=TRUE)
  
  if (!is.null(t.elapsed)) {
    cat(sprintf(" (%f / %f)", t.elapsed["user.self"] + t.elapsed["user.child"], t.elapsed["elapsed"]), file=file, append=TRUE)
  }
  
  cat("\n", file=file, append=TRUE)
  
  if (exists("conf.log") && conf.log) {
    unlock(m)
  }
  
  return(invisible())
}

learn.model = function(train.x, train.y, test.x, learner) {
  
  pred = NULL
  
  const.input = TRUE
  i = 0
  while(const.input && i < ncol(train.x)) {
    i = i+1
    const.input = all(train.x[-1, i] == train.x[-nrow(train.x), i])
  }
  
  const.output = all(train.y[-1] == train.y[-length(train.y)])
  
  # special cases:
  # - one class only in training (constant)
  # - no available input
  # - no discriminant input (RF bug : randomForest(data.frame(c("0", "0")), factor(c("0", "1"))) )
  if (const.output || ncol(train.x) < 1 || const.input) {
    
    t = system.time(NULL)
    model = NULL
    
    if (!is.null(test.x)) {
      pred = rep(levels(train.y)[which.max(table(train.y))], nrow(test.x))
    }
  }
  else {
    
    if (learner == "rf") {
      
      require("randomForest")
      
      t = proc.time()
      model = randomForest(x=train.x, y=factor(train.y), ntree=100, importance = FALSE, proximity = FALSE)
      t = proc.time() - t
    }
    else if (learner == "erf") {
      
      options(java.parameters = "-Xmx16g")
      require("rJava")
      require("extraTrees")
      .jinit(force.init = TRUE, parameters="-Xmx16g")
      
#       .jpackage(name = "extraTrees")
      
      t = proc.time()
      model = extraTrees(x=train.x, y=factor(train.y), ntree=100, numRandomCuts = 1) # numRandomCuts = 3
      t = proc.time() - t
    }
    else if (learner == "svm") {
      
      require("e1071")
      
      d = cbind(train.x, train.y)
      f = as.formula(paste(as.name(colnames(d)[ncol(d)]), "~", ".")) # y against all
      
      t = proc.time()
      #       model = svm(x=train.x, y=factor(train.y), kernel = "polynomial", cost = 100, degree=2)
      #       model = svm(formula=f, data=d, kernel = "polynomial", cost = 100, degree=2)
      #       model = best.svm(f, data=d, kernel="polynomial", cost=c(1, 10, 100), degree=c(1, 2, 3),
      #                        tunecontrol=tune.control(sampling="cross", nrepeat=5, cross=2))
      
      model = svm(x=train.x, y=factor(train.y), kernel = "linear", probability = TRUE, cachesize = 2048)
      t = proc.time() - t
      
    }
    else if (learner == "smo") {
      
      options(java.parameters = "-Xmx16g")
      require("RWeka")
      
      d = cbind(train.x, train.y)
      f = as.formula(paste(as.name(colnames(d)[ncol(d)]), "~", ".")) # y against all
      
      t = proc.time()
      
      model = SMO(f, data = d)
      t = proc.time() - t
      
    }
    else if (learner == "j48") {
      
      require("RWeka")
      
      d = cbind(train.x, train.y)
      f = as.formula(paste(as.name(colnames(d)[ncol(d)]), "~", ".")) # y against all
      
      t = proc.time()
      
      model = J48(f, data = d)
      t = proc.time() - t
      
    }
    else {
      stop(sprintf("unsupported base learner: %s", learner))
    }
    
    if (!is.null(test.x)) {
      pred = predict(model, test.x)
      pred = factor(pred, levels=levels(train.y)) # restore any level that could have been removed
    }
  }
  
  return(list(
    t = t,
    model = model,
    pred = pred))
}

pred.evaluation = function(true, pred) {
  
  nys = ncol(true)
  measures = c("accuracy", "recall", "precision", "f1",
               "global.accuracy")
  values = array(NA, dim=c(nys, length(measures)), dimnames=list(y.i=1:nys, measure=measures))
  
  values[, c("accuracy", "recall", "precision", "f1")] = t(apply(array(1:nys), 1, function(y.i) {
    
    ct = table(true=factor(true[, y.i], levels=1:2), pred=factor(pred[, y.i], levels=1:2))
    tp = ct[2, 2]
    tn = ct[1, 1]
    fp = ct[1, 2]
    fn = ct[2, 1]
    
    acc = (tp + tn) / (tp + tn + fp + fn)
    rec = if (tp == 0) 0 else tp / (tp + fn)
    pre = if (fp == 0) 1 else tp / (tp + fp)
    f1 = if (tp == 0) 0 else 2 * tp / (2 * tp + fn + fp)
    
    return(c(acc, rec, pre, f1))
  }))
  
  all.eq = apply(array(1:nrow(true)), 1, function(i) {
    all(true[i, ] == pred[i, ])
  })
  
  n.all.eq.per.ys.comb = aggregate(all.eq, by=as.data.frame(true), sum)[, nys+1]
  n.obs.per.ys.comb = aggregate(all.eq, by=as.data.frame(true), length)[, nys+1]
  
  values[1, "global.accuracy"] = sum(all.eq) / nrow(true)
  
  return(values)
}

labels.modalities = function(data) {
  
  # recover LP modalities (classes)
  lp.mods = unique(data)
  #   for (i in ncol(lp.mods):1) {
  #     lp.mods = lp.mods[order(lp.mods[, i]), , drop=FALSE]
  #   }
  return(lp.mods)
}

labels.aggregation = function(data, lp.mods) {
  
  v = rep(as.integer(NA), nrow(data))
  for (i in 1:nrow(lp.mods)) {
    v[apply(apply(array(1:ncol(data)), 1, function(c) {
      lp.mods[i, c] == data[, c]
    }), 1, all)] = i
  }
  return(factor(v, levels=1:nrow(lp.mods)))
}
