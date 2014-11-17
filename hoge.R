# 事前にcsvファイルのある�?ィレクトリに移動しておく

# フォルダ�?のcsvファイル読み込み
files <- list.files()
files.csvindex <- grep("\\.csv$",files)
csvdata = c()
for (i in 1:length(files.csvindex)){
  tmp <- read.csv(files[files.csvindex[i]],header=T,row.names=1)
  csvdata <- rbind(csvdata,tmp)
  
}

# csv�?ータを時間�??に並べ替える 
csvdata <- csvdata[sort.list(as.double(rownames(csvdata)),decreasing = F),]

# Loudness,Sharpnessの平�??��標準偏差を求め�?
LN <- csvdata[,1] 
SP <- csvdata[,2]
LN.median <- median(LN)
LN.sd <- sd(LN)
SP.median <- median(SP)
SP.sd <- sd(SP)

# フレー�?の点数の計�?
a_LN = 1.0 # ラウドネスの点数決定曲線�?�係数
a_SP = 30  # シャープネスの点数決定曲線�?�係数
LN.score <- c()
SP.score <- c()
score <- c()
for(i in 1:length(LN)){
  LN.score[i] <- detcurve(LN[i],a_LN,LN.median,LN.sd)
  SP.score[i] <- detcurve(SP[i],a_SP,SP.median,SP.sd)
  score[i] <- 100*LN.score[i]*SP.score[i]
}

wdata <- cbind(csvdata[,1:2],score)
write.csv(wdata,file="/Users/Shunji/Documents/MATLAB/wdata141111_001.csv")