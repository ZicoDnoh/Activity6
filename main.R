source(gen_A_W.R)

#setting
niter = 1000
num.sample = 3
prior.alp.vec = c(30, rep(0.5,num.sample))
prior.beta.vec = c(1, rep(0.5,num.sample))
all.lhist = gen.AL(T = num.sample)
Wlist = gen.W(T = num.sample, AL = all.lhist)
obs.freq = f