library(stringr)

# Download the BRCA mutation data in maf format from TCGA, and read it into R
mutect=read.table('TCGA.BRCA.mutect.995c0111-d90b-4140-bee7-3845436c3b42.DR-10.0.somatic.maf',header = T,sep = '\t',quote = '')

# Download the BRCA clinical data of the TCGA project in the UCSC Xena database, which contains the information about the expression status of ER, PR, and Her-2.
# Read clinical data (phenotype) in txt format, and there are 1247 samples.
phenotype_file <- read.table("BRCA_clinicalMatrix.txt",header = T, sep = '\t', quote = "")

# Only retain the phenotype information of the samples that have the corresponding mutation datas, then there are 986 samples left.
mutect<-data.frame(mutect)
ID<-str_sub(mutect[,16], end=15)
mutect$sampleID<-ID
phenotype_file2 <- phenotype_file[phenotype_file[,1] %in% mutect[,121],]

# Screening of triple negative breast cancer samples from all types of BRCA
phenotype_colnames <- as.data.frame(colnames(phenotype_file2))
## View estrogen receptor (ER) expression status
table(phenotype_file2$breast_carcinoma_estrogen_receptor_status)
## View Progesterone Receptor (PR) expression status
table(phenotype_file2$breast_carcinoma_progesterone_receptor_status)
## View Her-2 oncogene status
table(phenotype_file2$lab_proc_her2_neu_immunohistochemistry_receptor_status)

## Retrieve "receptor_status" in the phenotype_file2 column, the grep return value is the column number in the list of column names containing "receptor_status", because the three indicators of triple negative breast cancer have a phenotype_file field here
colnames_num <- grep("receptor_status",colnames(phenotype_file2))
colnames_num
## [1] 56  61 105 115 127
## Use the matching return value to take the column name into phenotype_colnames, which stores the column name containing the "receptor_status" field
phenotype_colnames <- colnames(phenotype_file2)[colnames_num]
## Screen out the three indicators and create a new table. It is convenient to filter out the IDs of all negative rows
eph <- phenotype_file2[,colnames_num[1:3]]
## Perform eph according to the column sum(x=="Negative"): Count the number of Negative in the eph column in each row
## By looking at the number of negatives, the function returns a numeric vector. The content is 0 or 1, 2, 3. Note that the number sequence is the row corresponding to the patient ID.
tnbc_rownum <- apply(eph,1,function(x)sum(x=="Negative"))
tnbc_rownum
## Distinguish by tnbc_rownum==3, if it is true, a logical vector will be returned, the logical vector can be directly referenced by the matrix, and only the rows or columns of TURE will be read
## Statistics that the value in each row is 3 (that is all "Negative") and take the first column that is the patient ID
tnbc_sample <- phenotype_file2[tnbc_rownum == 3, 1]
## Save the triple negative breast cancer sample ID
save(tnbc_sample,file = 'tnbc_sample.txt')
## Mutation information for all triple negative breast cancer samples
tnbc_sample<-data.frame(tnbc_sample)
mutect2 <- a[a[,121] %in% tnbc_sample[,1],]
write.table(mutect2, file = "mutect_tnbc.txt", sep = '\t', row.names =F, col.names =TRUE)

# Race Analysis
# Download the BRCA clinical data in tsv format from TCGA, which contains the information about race, and read it into R
clinical_file <- read.table("clinical.tsv",header = T, sep = '\t', quote = "")
clinical_file<-data.frame(clinical_file)
ID3<-str_sub(mutect2[,121], end=12)
mutect2$case_submitter_id<-ID3
# Filter out clinical information for triple negative breast cancer
ID2<-str_sub(tnbc_sample[,1], end=12)
tnbc_sample$case_submitter_id<-ID2
clinical_file2 <- clinical_file[clinical_file[,2] %in% tnbc_sample[,2],]
write.table(clinical_file2, file = "clinical_tnbc.txt", sep = '\t', row.names =F, col.names =TRUE)

## Extract the column containing the 'race' information
colnames_num_race <- grep('race',colnames(clinical_file2))
rph <- clinical_file2[,colnames_num_race]
table(rph)
## asian     black or african american       not reported        white 
##  16                            48                 12          126
## Group samples based on race
col_asian <- grep('asian', rph)
col_black <- grep('black or african american', rph) 
col_white <- grep('white', rph)

# Asian Sample Outputs: 16 samples
asian_sample <- clinical_file2[col_asian, 2]
asian_sample<-data.frame(asian_sample)
## Mutation Information Output:
mutect_asian <- mutect2[mutect2[,122] %in% asian_sample[,1],]
write.table(mutect_asian, file = "mutect_asian.txt", sep = '\t', row.names =F, col.names =TRUE)
## Clinical Information Output:
clinical_asian <- clinical_file2[clinical_file2[,2] %in% asian_sample[,1],]
write.table(clinical_asian, file = "clinical_asian.txt", sep = '\t', row.names =F, col.names =TRUE)

# AA Sample Outputs: 48 samples
AA_sample <- clinical_file2[col_black, 2]
AA_sample<-data.frame(AA_sample)
## Mutation Information Output:
mutect_AA <- mutect2[mutect2[,122] %in% AA_sample[,1],]
write.table(mutect_AA, file = "mutect_AA.txt", sep = '\t', row.names =F, col.names =TRUE)
## Clinical Information Output:
clinical_AA <- clinical_file2[clinical_file2[,2] %in% AA_sample[,1],]
write.table(clinical_AA, file = "clinical_AA.txt", sep = '\t', row.names =F, col.names =TRUE)

# EA Sample Outputs: 126 samples
EA_sample <- clinical_file2[col_white, 2]
EA_sample<-data.frame(EA_sample)
## Mutation Information Output:
mutect_EA <- mutect2[mutect2[,122] %in% EA_sample[,1],]
write.table(mutect_EA, file = "mutect_EA.txt", sep = '\t', row.names =F, col.names =TRUE)
## Clinical Information Output:
clinical_EA <- clinical_file2[clinical_file2[,2] %in% EA_sample[,1],]
write.table(clinical_EA, file = "clinical_EA.txt", sep = '\t', row.names =F, col.names =TRUE)



