#!/usr/local/env Rscript
# Usage: Rscript tsv_to_ven L15N1.tsv minus L15L1.tsv L15L2 ... L15Ln.tsv


#L15l1.tsvとL15N1.tsvの比較

library(data.table)

library(gplots)
#コマンドライン引数を取得
args <- commandArgs(trailingOnly=T)
#サンプル数を確認
samplenum <- length(args)
loopnum <- samplenum - 1

header <- fread(args[1], header=F, data.table=F, sep="\t")

#データフレームが返ってくる
for (i in 1:samplenum){
       assign(paste0("sample", i), fread(args[i], header=T, data.table=F, sep="\t"))
}
#Normalの２、３、４列目に着目する
target <- paste(sample1[, 2], sample1[, 3], sample1[, 4], sep="-")

#sampleの２、３、４列目に着目してNormalと共通しないものについてはresとしてだす
for(i in 2:samplenum){
	assign(paste0("minus", i), 
	paste(get(paste0("sample", i))[, 2],
              get(paste0("sample", i))[, 3], 
              get(paste0("sample", i))[, 4],
              sep="-"))
          assign(paste0("res", i), get(paste0("sample", i))[! target %in% get(paste0("minus", i) ), ])}

#resのうちの共通部分について取ってくる
for(i in 2:samplenum){
	assign(paste0("minus_", i),
	paste(get(paste0("res", i))[, 2],
	      get(paste0("res", i))[, 3],
	      get(paste0("res", i))[, 4],
    	      sep="-"))
} 

res_3 <- res3[minus_2 %in% minus_3, ]
for(i in 3:loopnum){
	j <- i + 1
	assign(paste0("res_", j),
	get(paste0("res", j))[get(paste0("res_", i)) %in% get(paste0("minus_", j)),])
}

data <- list(L1 = minus_2, L2 = minus_3, L3 = minus_4, L4 = minus_5)

res_data <- list()
for(i in 2:samplenum){
	assign(paste0("L", i - 1), 
	paste(get(paste0("minus_", i))))
	res_data <- c(res_data, list(get(paste0("L", i - 1))))
}
venn(res_data)
pdf("venn.pdf")
venn(res_data)
dev.off()
