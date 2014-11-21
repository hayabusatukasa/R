# 事前にcsvファイルのあるディレクトリに移動しておく

# 点数計算関数
detcurve <- function(x,a,m,sd){
  if(x<=m){
    return <- 1/(1+exp(-a*(x-m+sd)))
  }else{
    return <- 1/(1+exp(a*(x-m-sd)))
  }
}

getscore <- function(LNscore,SPscore){
  return <- 70*LNscore+30*SPscore
}

# フォルダ内のcsvファイル読み込み
files <- list.files()
files.csvindex <- grep("\\.csv$",files)
csvdata = c()
for (i in 1:length(files.csvindex)){
  tmp <- read.csv(files[files.csvindex[i]],header=T,row.names=1)
  csvdata <- rbind(csvdata,tmp)
  
}

# csvデータを時間順に並べ替える
# 読み込むcsvデータが時間順でない場合があるため
csvdata <- csvdata[sort.list(as.double(rownames(csvdata)),decreasing = F),]

# Loudness,Sharpnessの中央値と標準偏差を求める
LN <- csvdata[,1] 
SP <- csvdata[,2]
LN.median <- median(LN)
LN.sd <- sd(LN)
SP.median <- median(SP)
SP.sd <- sd(SP)

# フレームごとの点数の計算
a_LN = 1.0 # ラウドネスの点数決定曲線の係数
a_SP = 30  # シャープネスの点数決定曲線の係数
sec = 60   # スコア2の中央値と標準偏差をとるデータの間隔
LN.score1 <- c()
LN.score2 <- c()
SP.score1 <- c()
SP.score2 <- c()
score1 <- c()
score2 <- c()
for(i in 1:length(LN)){
  ## スコア1の計算
  LN.score1[i] <- detcurve(LN[i],a_LN,LN.median,LN.sd)
  SP.score1[i] <- detcurve(SP[i],a_SP,SP.median,SP.sd)
  # score[i] <- 100*LN.score[i]*SP.score[i]
  score1[i] <- getscore(LN.score1[i],SP.score1[i])
  
  ## スコア2の計算
  len <- length(LN)
  if(i>sec && i<=len-sec){
    tmp_LN <- LN[(i-sec):(i+sec)]
    tmp_SP <- SP[(i-sec):(i+sec)]
  }else if(i<=sec && i<=len-sec){
    tmp_LN <- c(LN[1:i],LN[(i+1):(i+sec)])
    tmp_SP <- c(SP[1:i],SP[(i+1):(i+sec)])
  }else if(i>sec && i>len-sec){
    tmp_LN <- c(LN[(i-sec):(i-1)],LN[i:len])
    tmp_SP <- c(SP[(i-sec):(i-1)],SP[i:len])
  }
  tmp_LN.me <- median(tmp_LN)
  tmp_SP.me <- median(tmp_SP)
  tmp_LN.sd <- sd(tmp_LN)
  tmp_SP.sd <- sd(tmp_SP)
  LN.score2[i] <- detcurve(LN[i],a_LN,tmp_LN.me,tmp_LN.sd)
  SP.score2[i] <- detcurve(SP[i],a_SP,tmp_SP.me,tmp_SP.sd)
  score2[i] <- getscore(LN.score2[i],SP.score2[i])
  
}

wdata <- cbind(csvdata[,1:2],score1,score2)
write.csv(wdata,file="/Users/Shunji/Documents/MATLAB/wdataver2_141111_001.csv")