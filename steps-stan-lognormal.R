require(rstan)
require(ggplot2)
require(coda)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

Sys.setlocale("LC_TIME","C") # for as.Date
d <- read.csv("./data/HealthData.csv", header=T)

df <- data.frame(finish=as.Date(d$Finish, format="%d-%b-%Y %H:%M"), steps=d$Steps..count.)

df.t <- data.frame(
          finish = df[!duplicated(df$finish),]$finish, # 重複削除
          steps = as.integer(tapply(df$steps, df$finish, sum)) # 同じ日でまとめる
        )

df.t <- df.t[-1 * length(df.t$steps),] # 最終日の歩数 0 の行を除く

stan_code <- '
  data {
    int<lower=0> N;
    real<lower=0> d[N];
  }

  parameters {
    real<lower=0> mu;
    real<lower=0> sigma;
  }

  model {
    for (i in 1:N){
        d[i] ~ lognormal(mu, sigma);
    }
  }

  generated quantities{
    real<lower=0>	zeta;
    real<lower=0> lower;
    real<lower=0> upper;
    zeta <- exp(mu);
    lower <- exp(mu - 0.95 * sigma);
    upper <- exp(mu + 0.95 * sigma);
  }
'

d.list <- list(
    N=length(df.t$finish),
    d=df.t$steps
  )

d.fit <- stan(model_code=stan_code, data=d.list, iter=2000, chains=2)

print(d.fit, digits=2)
# Inference for Stan model: 0f858f077ba0a94715dca66266b5e3dd.
# 2 chains, each with iter=2000; warmup=1000; thin=1;
# post-warmup draws per chain=1000, total post-warmup draws=2000.
#
#           mean se_mean     sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
# mu        9.33    0.00   0.02     9.29     9.32     9.33     9.34     9.37   960    1
# sigma     0.37    0.00   0.01     0.34     0.36     0.37     0.38     0.40   582    1
# zeta  11268.88    7.35 228.38 10810.76 11126.41 11265.94 11422.60 11733.75   965    1
# lower  7954.87    6.86 194.55  7563.57  7828.78  7958.39  8091.47  8320.57   805    1
# upper 15966.59   13.88 394.84 15223.63 15690.08 15963.07 16239.13 16762.46   809    1
# lp__    166.48    0.04   1.07   163.83   166.11   166.80   167.19   167.47   613    1

model.fit.coda <- mcmc.list(
                    lapply(
                      1:ncol(d.fit),
                      function(x) mcmc(as.array(d.fit)[,x,])
                    )
                  )
plot(model.fit.coda)

ex <- extract(d.fit)
ex.zeta <-mean(ex$zeta)
ex.lower <-mean(ex$lower)
ex.upper <-mean(ex$upper)

# line
p <- ggplot(
  df.t,
  aes(
      x = finish,
      y = steps,
  )
)

p <- p + geom_line(size=0.5) +
    geom_hline(yintercept=as.integer(ex.zeta), colour="blue", linetype="longdash") +
    geom_hline(yintercept=as.integer(ex.lower), colour="cyan", linetype="longdash") +
    geom_hline(yintercept=as.integer(ex.upper), colour="cyan", linetype="longdash") +
    labs(title="Steps", x="Date", y="Steps")
plot(p)
