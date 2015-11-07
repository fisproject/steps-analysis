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
    real<lower=0, upper=20> mu;
    real<lower=0> sigma;
    real<lower=0, upper=5> m_d;
    real<lower=0> s_d;
    real<lower=0, upper=320> c;
  }

  model {
    for (i in 1:N){
      real m;
      real s;
      m <- mu + int_step(i-c) * m_d;
      s <- sigma + int_step(i-c) * s_d;
      d[i] ~ lognormal(m, s);
    }
  }

  generated quantities{
    real<lower=0>	zeta;
    real<lower=0>	late;
    zeta <- exp(mu);
    late <- exp(mu + m_d);
  }
'

d.list <- list(
    N=length(df.t$finish),
    d=df.t$steps
  )

d.fit <- stan(model_code=stan_code, data=d.list, iter=2000, chains=3)
# Inference for Stan model: 130fd1283af58a55cafff08e0d98f03f.
# 3 chains, each with iter=2000; warmup=1000; thin=1;
# post-warmup draws per chain=1000, total post-warmup draws=3000.
#
#           mean se_mean     sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
# mu        9.25    0.00   0.02     9.21     9.23     9.25     9.26     9.29    33 1.10
# sigma     0.32    0.01   0.02     0.29     0.30     0.31     0.33     0.35     5 1.29
# m_d       0.21    0.01   0.04     0.15     0.19     0.21     0.23     0.28    49 1.03
# s_d       0.09    0.01   0.04     0.02     0.06     0.09     0.12     0.17     9 1.48
# c       196.52    2.46  15.53   172.66   187.11   192.49   206.15   229.01    40 1.08
# zeta  10374.56   40.28 230.53  9964.69 10211.61 10371.78 10526.39 10850.98    33 1.10
# late  12815.84   63.33 453.86 11996.51 12485.67 12793.14 13138.31 13703.81    51 1.02
# lp__    179.88    0.29   1.64   176.28   178.83   180.04   181.13   182.38    33 1.08

print(d.fit, digits=2)

model.fit.coda <- mcmc.list(
                    lapply(
                      1:ncol(d.fit),
                      function(x) mcmc(as.array(d.fit)[,x,])
                    )
                  )
plot(model.fit.coda[,5]) # c-param

ex <- extract(d.fit)
ex.zeta <-mean(ex$zeta)
ex.late <-mean(ex$late)
ex.c <-mean(ex$c)

changes.y <- 1:d.list$N
for(i in 1:d.list$N){
  changes.y[i] <- ifelse( i < ex.c, ex.zeta, ex.late)
}

changes <- data.frame(
  x=df.t$finish,
  y=changes.y
)

# line
p <- ggplot(
  df.t,
  aes(
      x = finish,
      y = steps,
  )
)

p <- p + geom_line(size=0.5) +
    geom_vline(xintercept=as.integer(df.t$finish[227]), colour="red", linetype="longdash") + # 引越しにより徒歩量が変化した日
    geom_vline(xintercept=as.integer(df.t$finish[ex.c]), colour="blue", linetype="longdash") +
    geom_line(data=changes, aes(x=x, y=y), colour="cyan") +
    labs(title="Steps", x="Date", y="Steps")
plot(p)
