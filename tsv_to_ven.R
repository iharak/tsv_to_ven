#!/usr/local/env Rscript
# Usage: Rscript tsv_to_ven normal1.tsv minus sample1.tsv sample2.tsv ... samplen.tsv

library(data.table)
library(gplots)
library(VennDiagram)
library(grDevices)

#pass command-line arguments
args <- commandArgs(trailingOnly=T)
#count the number of sample files
samplenum <- length(args)
loopnum <- samplenum - 1
#create dataframe for every sample 
for (i in 1:samplenum){
       assign(paste0("sample", i), fread(args[i], header=T, data.table=F, sep="\t"))
}
#extract 2nd, 3rd,and 4th columns of normal.tsv
target <- paste(sample1[, 2], sample1[, 3], sample1[, 4], sep="-")
dir.create("./output")
#extract 2nd, 3rd,and 4th columns of sanple files and create var files for not-common indexes 
for(i in 2:samplenum){
	j <- i - 1
	assign(paste0("minus", i), 
	paste(get(paste0("sample", i))[, 2],
              get(paste0("sample", i))[, 3], 
              get(paste0("sample", i))[, 4],
              sep="-"))
        assign(paste0("var", i), get(paste0("sample", i))[! get(paste0("minus", i) ) %in% target, ])
	sample <- get(paste0("var", i))
	former <- sub(".tsv", "", args[1])
	latter <- sub(".tsv", "", args[i])
	file_index <- paste0(former, "_vs_", latter)
	file_name <- paste0("./output/", file_index, ".tsv")
	write.table(sample, file_name, sep="\t", row.names=F, col.names=T, quote=F)
	sample_name <- args[j]
	venn_sample <- get(paste0("minus", i))
	venn_list <- list(target, venn_sample)
	names(venn_list) <- c(args[1], sample_name)
	pdf_name <- paste0("./output/venn_", file_index, ".pdf")
	temp <- venn.diagram(venn_list, fill=c(5, 6), alpha=0.2, lty=1, filename=NULL)
	pdf(file=pdf_name)
	grid.draw(temp)
	dev.off()
}
#extract mutual var-index 
for(i in 2:samplenum){
	assign(paste0("var_index", i),
	paste(get(paste0("var", i))[, 2],
	      get(paste0("var", i))[, 3],
	      get(paste0("var", i))[, 4],
    	      sep="-"))
} 
mutual_index3 <- var_index2[var_index2 %in% var_index3]
for(i in 3:loopnum){
	j <- i + 1
	assign(paste0("mutual_index", j),
	get(paste0("var_index", j))[get(paste0("var_index", j)) %in% get(paste0("mutual_index", i))])
}
total_res_index <- get(paste0("mutual_index", samplenum))
file_name <- "./output/total_res.tsv"
total_res_index <- strsplit(total_res_index, "-")
total_res_index <- data.frame(t(sapply(total_res_index, c)))
colnames(total_res_index) <- c("Variant", "Chr", "Coordinate")
write.table(total_res_index, file_name, sep="\t", row.names=F, col.names=T, quote=F)
res_data <- list()
sample_name <- sub(".tsv", "", args[-1])
for(i in 2:samplenum){
	assign(paste0("sample", i - 1), 
	paste(get(paste0("var_index", i))))
	res_data <- c(res_data, list(get(paste0("sample", i - 1))))
}
names(res_data) <- sample_name
library(grDevices)
color_num <- c(1:(length(args) - 1))
temp <- venn.diagram(res_data, fill=color_num, alpha=0.2, lty=1, filename=NULL)
pdf(file="./output/total_res_ven.pdf")
grid.draw(temp)
dev.off()
