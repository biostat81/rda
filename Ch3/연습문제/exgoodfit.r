raw.obs <- sample.int(6, 100, replace=T)
obs.freq <- table(raw.obs)
chisq.test(obs.freq, correct=F, p=rep(1/6, 6))
