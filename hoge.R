# 事前にcsvファイルのあるディレクトリに移動しておく

# フォルダ内のcsvファイル読み込み
files <- list.files()
files.csvindex <- grep("\\.csv$",files)
csvdata = c()
for (i in 1:length(files.csvindex)){
  tmp <- read.csv(files[files.csvindex[i]],header=T,row.names=1)
  csvdata <- rbind(csvdata,tmp)
  
}

# csvデータを時間順に並べ替える 
csvdata <- csvdata[sort.list(as.double(rownames(csvdata)),decreasing = F),]

