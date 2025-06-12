# qpWave Heatmap for Ovilava MODEL 3
## Author: Heather Chamberlain
## Date: May 2025


# Upload data
# Set working directory  
setwd("/lisc/scratch/anthropology/Pinhasi_group/Ovilava/qpWave/Model3")

# Batch read all qpWave output files and retrieve rank=0 p-values
rm(list = ls())
file.remove(list.files(path=getwd(),pattern="NA_QPWAVE_OUT$"))
mylist1=list.files(path=getwd(),pattern="_QPWAVE_OUT$")
testList = c()
for(i in list.files(path=getwd(),pattern="_QPWAVE_OUT$")) {
  testList = c(testList,strsplit(i,"_PLUS_")[[1]][1])
}
sampleList = unique(testList)

amodels_table = data.frame(matrix(nrow = 0,ncol = 5))
colnames(amodels_table)=c("A","B","Right","P-value_R0","SNPs")


counter = 1
for(qpwavefile in mylist1) {
  print(qpwavefile)
  
  qpwaveload = read.table(file = qpwavefile, fill = TRUE, sep = "\n", stringsAsFactors = FALSE)
  
  # Get left populations
  lineL = which(grepl(pattern = "left pops:", x = qpwaveload$V1)) + 1
  left_pops = qpwaveload[seq(lineL, lineL + 1), , drop = FALSE]
  
  # Get right populations
  lineR = which(grepl(pattern = "right pops:", x = qpwaveload$V1)) + 1
  lineRlast = (which(grepl(pattern = " 0 ", x = qpwaveload$V1)) - 1)[1]
  right_pops = qpwaveload[seq(lineR, lineRlast), , drop = FALSE]
  
  # Extract p-value for rank 0
  line0 = which(grepl(pattern = "f4rank: 0", x = qpwaveload$V1))
  pvalue_parts = strsplit(x = as.character(qpwaveload[line0, ]), split = " ")[[1]]
  pvalue = pvalue_parts[pvalue_parts != ""][8]
  
  # Extract number of SNPs
  linesnp = which(grepl(pattern = "numsnps used:", x = qpwaveload$V1))
  snp_parts = strsplit(x = as.character(qpwaveload[linesnp, ]), split = " ")[[1]]
  numspns = snp_parts[snp_parts != ""][3]
  
  # Add to table
  amodels_table[counter, ] <- c(
    strsplit(trimws(left_pops[1, 1]), "\\s+")[[1]][1],
    strsplit(trimws(left_pops[2, 1]), "\\s+")[[1]][1],
    paste0(trimws(right_pops[, 1]), collapse = ","),
    as.character(pvalue),
    as.character(numspns)
  )
  
  counter = counter + 1
}








## Sorting by column index [1] then [2]
amodels_table = amodels_table[order( amodels_table[,1], amodels_table[,2] ),]
amodels_table$`P-value_R0` = as.numeric(amodels_table$`P-value_R0`)
## Replacing values smaller than 1E-299 with 0 to avoid incompatibilities with Excel when manually reordering the heatmap further down
amodels_table[which(amodels_table$`P-value_R0` < 1e-299 & amodels_table$`P-value_R0` > 0),4] = 0
write.table(x = amodels_table,file = "table_batch_qpWave",sep = "\t",quote = F,row.names = F,col.names = T)



system("head table_batch_qpWave")
lines <- readLines("table_batch_qpWave")
sapply(strsplit(lines, "\t"), length)

# Load "table_batch_qpWave" to create a heatmap
rm(list = ls())


##### here starts the manual test
qpWave_batch_load = read.table(file = "table_batch_qpWave", fill = TRUE, sep = "\t", stringsAsFactors = FALSE, header = TRUE)
#qpWave_batch_load = read.table(file = "table_batch_qpWave", fill= T, sep="",stringsAsFactors = F,header = T)
qpWave_batch_load[,6] = paste0(qpWave_batch_load$A,"---",qpWave_batch_load$B)
qpWave_batch_load[,7] = paste0(qpWave_batch_load$B,"---",qpWave_batch_load$A)


pops1 = unique(qpWave_batch_load$A)
pops2 = unique(qpWave_batch_load$B)

uniquePops = unique(c(pops1,pops2))

rm1 = which(duplicated(qpWave_batch_load$V6) == TRUE)
rm2 = which(duplicated(qpWave_batch_load$V7) == TRUE)
if(length(c(rm1,rm2)) != 0) {
  qpWave_batch_load = qpWave_batch_load[-c(rm1,rm2),]}

## Heatmap matrix of p-values
qpWave_heat = matrix(nrow=length(uniquePops),ncol=length(uniquePops))
rownames(qpWave_heat) = uniquePops
colnames(qpWave_heat) = uniquePops

sequPops = matrix(ncol=2,nrow=length(uniquePops))
sequPops[,1] = uniquePops
sequPops[,2] = seq(1:length(uniquePops))

combis = combn(uniquePops,2)
it=1
for(colu in seq(1:length(combis[1,]))) {
  if(length(which(qpWave_batch_load$V6 %in% paste0(combis[,colu][1],"---",combis[,colu][2]))) != 0) {
    where = which(qpWave_batch_load$V6 %in% paste0(combis[,colu][1],"---",combis[,colu][2]))
  } else if(length(which(qpWave_batch_load$V7 %in% paste0(combis[,colu][1],"---",combis[,colu][2]))) != 0) {
    where = which(qpWave_batch_load$V7 %in% paste0(combis[,colu][1],"---",combis[,colu][2]))
  }
  pvalue = qpWave_batch_load[where,4]
  
  popsHere1 = qpWave_batch_load[where,1]
  popsHere2 = qpWave_batch_load[where,2]
  qpWave_heat[popsHere1,popsHere2] = pvalue
  qpWave_heat[popsHere2,popsHere1] = pvalue
  it=it+1
}


## Output the p-value matrix if needed to be ordered or edited outside of R
write.csv(x = qpWave_heat,file = "UPDATEtable_qpWave_heatmap.csv",quote = F)

## reorder ####
## Output the p-value matrix if needed to be ordered or edited outside of R
write.csv(x = qpWave_heat,file = "UPDATEtable_qpWave_heatmap.csv",quote = F)



# Define the desired order for the test samples to be put at the end
# desired_order <- c("Sarengrad_G41", "Sarengrad_G31", "SGK34","Sarengrad_G18", "Sarengrad_G80","Sarengrad_G33",  "SGK39",  "Sarengrad_G84",  "SGK68","SGK12","SGK29","SGK28",  "SGK50", "Sarengrad_G21", "Sarengrad_G17",   "Sarengrad_G71", "Sarengrad_G73", "Sarengrad_G75","Sarengrad_G55","Sarengrad_G14","Sarengrad_G37", "Sarengrad_G38")  # Add other test samples as needed
desired_order <- c(
  "I35357", "I35358", "I35359", "I35385", "I35386", "I35387", "I35388", "I35389",
  "I35391", "I35392", "I35393", "I35394", "I35395", "I35396", "I35401", "I35402",
  "I35403", "I35404", "I35405", "I35406", "I35407", "I35408", "I35409", "I35411",
  "I35412", "I35414", "I35415", "I35416", "I35417", "I35418", "I35419", "I35421",
  "I35422", "I35423", "I35425", "I35426", "I35427", "I35436", "I35437", "I35438",
  "I35440", "I35441", "I35442", "I35443", "I35444", "I35445", "I35450", "I35451",
  "I35452", "I35453", "I35454", "I35460", "I35461", "I35462", "I35465", "I35466",
  "I35467", "I35469", "I35470", "I35471", "I35561", "I35583", "I35910", "I35911",
  "I35912", "I35913", "I35929", "I35931", "I35932", "I35933", "I35934",
  "I35935", "I35936", "I35950", "I35951", "I35952", "I35953", "I36193", "I36194",
  "I35951", "I35424", "I35419", "I35401"
)


# Reorder columns and rows of qpWave_heat according to desired_order
desired_order_indices <- match(desired_order, uniquePops)
qpWave_heat_reordered <- qpWave_heat[desired_order_indices, desired_order_indices]

# Replace original qpWave_heat with the reordered matrix
qpWave_heat <- qpWave_heat_reordered


###########################
empt = c()
for(i in colnames(qpWave_heat)){
  print(i)
  print(strsplit(i,split = "_"))
  empt = c(empt, strsplit(i,split = "_")[[1]][2])
}
print(empt)

order(empt, decreasing = T)
df2=qpWave_heat
df2 <- df2[order(as.numeric(empt), decreasing = TRUE,na.last = TRUE ),]
df2 <- df2[,order(as.numeric(empt), decreasing = TRUE, na.last = TRUE)]


qpWave_heat = df2
############################




## (Un)comment next line to read edited file, allowing to easily import modified labels and order
#qpWave_heat = read.csv(file = "table_qpWave_heatmap_original", stringsAsFactors = F, row.names = 1)
uniquePops = rownames(qpWave_heat)
qpWave_heat = as.matrix(qpWave_heat)
qpWave_heat_col = qpWave_heat
qpWave_heat_col[qpWave_heat_col < 0.01000] = "grey85"
qpWave_heat_col[qpWave_heat_col >= 0.01000 & qpWave_heat_col <0.05] = "#d8d78f"
qpWave_heat_col[qpWave_heat_col >= 0.05000 & qpWave_heat_col <0.10] = "#ceb43d"
qpWave_heat_col[qpWave_heat_col >= 0.1000 & qpWave_heat_col <0.40] = "#c34818"
qpWave_heat_col[qpWave_heat_col >= 0.4000 & qpWave_heat_col <0.70] = "#7b2300"
qpWave_heat_col[qpWave_heat_col >= 0.7000 & qpWave_heat_col <=1] = "#421300"
qpWave_heat_col[is.na(qpWave_heat_col)] = "white"


###  Heatmap matrix with color values
## Change margin sizes here by replacing the 1st and 4th value in "mar"
png("TEST_Model3.png", units="cm", width=40, height=40, res=600) #can change width and height

par(mar = c(13,0.5,0.5,13), mgp = c(3,0.5,0), xpd=NA)

plot(x=0,y=0,type="n",xlab="",ylab="",xlim = c(0,length(qpWave_heat_col[,1])), ylim = c(length(qpWave_heat_col[,1]),0), bty="n", axes = F, xaxs = "i", yaxs = "i")

for(coli in seq(1:length(qpWave_heat_col[1,]))) {
  ## In coli 1, do X rows, etc
  for(i in seq(1:length(qpWave_heat_col[,1]))) {
    rect(xleft = coli-1,ybottom = i-1,xright = coli,ytop = i,col=qpWave_heat_col[i,coli], lwd=0, border = qpWave_heat_col[i,coli]) # if you want borders, change arg border to for example "white"
  }
}

axis(at = seq(1:length(qpWave_heat_col[1,]))-0.5,labels = colnames(qpWave_heat_col),side = 1, tick = F, las = 2, cex.axis=0.8)
axis(at = seq(1:length(qpWave_heat_col[1,]))-0.5,labels = rownames(qpWave_heat_col),side = 4, tick = F, las = 2, cex.axis=0.8)

## Add legend to bottom right corner
## Use spacerX and spacerY as spacers for the position of the legend, with values between 0 and 1
spacerX = 1
spacerY = 1
rect(xleft = par("usr")[2]+spacerX+1, ybottom = par("usr")[3]+spacerY+7, xright = par("usr")[2]+spacerX+1.75, ytop = par("usr")[3]+spacerY+1.5, col = "#d8d78f", lwd = 1, border = "#d8d78f")
rect(xleft = par("usr")[2]+spacerX+1.75, ybottom = par("usr")[3]+spacerY+7, xright = par("usr")[2]+spacerX+2.5, ytop = par("usr")[3]+spacerY+1.5, col = "#ceb43d", lwd = 1, border = "#ceb43d")
rect(xleft = par("usr")[2]+spacerX+2.5, ybottom = par("usr")[3]+spacerY+7, xright = par("usr")[2]+spacerX+5.17, ytop = par("usr")[3]+spacerY+1.5, col = "#c34818", lwd = 1, border = "#c34818")
rect(xleft = par("usr")[2]+spacerX+5.17, ybottom = par("usr")[3]+spacerY+7, xright = par("usr")[2]+spacerX+7.84, ytop = par("usr")[3]+spacerY+1.5, col = "#7b2300", lwd = 1, border = "#7b2300")
rect(xleft = par("usr")[2]+spacerX+7.84, ybottom = par("usr")[3]+spacerY+7, xright = par("usr")[2]+spacerX+10.5, ytop = par("usr")[3]+spacerY+1.5, col = "#421300", lwd = 1, border = "#421300")

segments(x0 = par("usr")[2]+spacerX+1, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+10.5, y1 = par("usr")[2]+spacerY+7, lwd = 1)
segments(x0 = par("usr")[2]+spacerX+1, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+1, y1 = par("usr")[2]+spacerY+7.5, lwd = 1)
segments(x0 = par("usr")[2]+spacerX+1.75, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+1.75, y1 = par("usr")[2]+spacerY+7.5, lwd = 1)
segments(x0 = par("usr")[2]+spacerX+2.5, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+2.5, y1 = par("usr")[2]+spacerY+7.5, lwd = 1)
segments(x0 = par("usr")[2]+spacerX+5.17, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+5.17, y1 = par("usr")[2]+spacerY+7.5, lwd = 1)
segments(x0 = par("usr")[2]+spacerX+7.84, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+7.84, y1 = par("usr")[2]+spacerY+7.5, lwd = 1)
segments(x0 = par("usr")[2]+spacerX+10.5, y0 = par("usr")[3]+spacerY+7, x1 = par("usr")[2]+spacerX+10.5, y1 = par("usr")[2]+spacerY+7.5, lwd = 1)

text(par("usr")[2]+spacerX+1, par("usr")[2]+spacerY+8.3, "0", cex = 0.8)
text(par("usr")[2]+spacerX+5.75, par("usr")[2]+spacerY+8.3, expression(italic("P")~"value"), cex = 0.8)
text(par("usr")[2]+spacerX+10.5, par("usr")[2]+spacerY+8.3, "1", cex = 0.8)

text(par("usr")[2]+spacerX+1.35, par("usr")[2]+spacerY+4.2, "0.01-0.05", cex = 0.7, srt = 90)
text(par("usr")[2]+spacerX+2.115, par("usr")[2]+spacerY+4.2, "0.05-0.10", cex = 0.7, srt = 90)
text(par("usr")[2]+spacerX+3.9, par("usr")[2]+spacerY+4.2, "0.10-0.40", cex = 0.7, srt = 90)
text(par("usr")[2]+spacerX+6.5, par("usr")[2]+spacerY+4.2, "0.40-0.70", cex = 0.7, srt = 90, col="white")
text(par("usr")[2]+spacerX+9.2, par("usr")[2]+spacerY+4.2, "0.70-1.00", cex = 0.7, srt = 90, col="white")

dev.off()
dev.off()
dev.off()



