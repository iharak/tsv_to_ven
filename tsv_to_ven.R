#!/usr/local/env Rscript
# Usage: Rscript tsv_to_ven normal1.tsv minus sample1.tsv sample2.tsv ... samplen.tsv
library(data.table)
library(gplots)
library(VennDiagram)
library(grDevices)

# pass command-line arguments
args <- commandArgs(trailingOnly=T)
former <- sub(".tsv", "", args[1])
latters <- sub(".tsv", "", args[2:length(args)])
file_index <- paste0(former, "_vs_", latters)
output <- "./output"
total_res_filename <- paste0(output, "/total_res.tsv")
union_filename <- paste0(output, "/union.tsv")
pdf_name <- paste0(output,
                   "/venn_",
                   file_index,
                   ".pdf")
# count the number of sample files
samplenum <- length(args)
loopnum <- samplenum - 1

# create dataframe for every sample
for (i in 1:samplenum){
              assign(paste0("sample", i),
                     fread(args[i],
                           header=T,
                           data.table=F,
                           sep="\t"
                           )
                     )  
}
# extract 2nd, 3rd,and 4th columns of normal.tsv
target <- paste(sample1[, 2], sample1[, 3], sample1[, 4], sep="-")
if (file.exists(output)){
    print("./output already exists")
} else {
    dir.create(output)
} 
# extract 2nd, 3rd,and 4th columns of sanple files and create var files for not-common indexes
for(i in 2:samplenum){
    j <- i -1
    vs_filename <- paste0(output, "/" , file_index[j], ".tsv")
    assign(paste0("minus", i),
    paste(get(paste0("sample", i))[, 2],
          get(paste0("sample", i))[, 3],
          get(paste0("sample", i))[, 4],
          sep="-"))
    assign(paste0("var", i),
           get(paste0("sample", i))
           [! get(paste0("minus", i) ) %in% target, ])
    sample <- get(paste0("var", i))
    write.table(sample,
                vs_filename,
                sep="\t",
                row.names=F,
                col.names=T,
                quote=F)
    venn_sample <- get(paste0("minus", i))
    venn_list <- list(target, venn_sample)
    names(venn_list) <- c(former, latters[j])
    pdf(file=pdf_name[j])
    venn(venn_list)
    dev.off()
}

# extract mutual var-index
for(i in 2:samplenum){
    assign(paste0("var_index", i),
           paste(get(paste0("var", i))[, 2],
                 get(paste0("var", i))[, 3],
                 get(paste0("var", i))[, 4],
                 sep="-"))
}
total_res_index <- var_index2[var_index2 %in% var_index3]
for(i in 4:length(args)){
    total_res_index <- total_res_index[total_res_index %in%
                                 get(paste0("var_index", i))]
}

# create a union of not-common var indexes and add 1/0 columns for each sample 
union_index <- c()
for(i in 2:length(args)){
    union_index <- union(union_index, get(paste0("var_index", i)))
}
union_index_list <- strsplit(union_index, "-")
union_index_frame <- data.frame(t(sapply(union_index_list, c)))
for(i in 2:length(args)){
    assign(paste0("union_answer", i),
           ifelse(union_index %in% get(paste0("var_index", i)), "1", "0"))
    union_index_frame <- data.frame(union_index_frame, get(paste0("union_answer", i)))
}
colnames(union_index_frame) <- c("Variant", "Chr", "Coordinate", args[2:length(args)])
write.table(union_index_frame,
            union_filename,
            sep="\t",
            row.names=F,
            col.names=T,
            quote=F)

# extract common results for each sample
total_res_index <- strsplit(total_res_index, "-")
total_res_index <- data.frame(t(sapply(total_res_index, c)))
colnames(total_res_index) <- c("Variant", "Chr", "Coordinate")
write.table(total_res_index,
            total_res_filename,
            sep="\t",
            row.names=F,
            col.names=T,
            quote=F)
res_data <- list()
for(i in 2:samplenum){
    assign(paste0("sample", i - 1),
           paste(get(paste0("var_index", i))))
    res_data <- c(res_data, list(get(paste0("sample", i - 1))))
}
names(res_data) <- latters
pdf(file=paste0(output, "/total_res_ven.pdf"))
venn(res_data)
dev.off()
