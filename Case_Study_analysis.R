##############################################################################################################
#
#                                                  PART 1  (Checkpoint 1)
#
##############################################################################################################
#1. Load the two files companies.txt and rounds2.csv into two data frames and name them companies and rounds2 respectively.

companies <- read.delim("Data/companies.txt",sep = "\t", header = T, stringsAsFactors = F)
rounds2 <- read.csv("Data/rounds2.csv", header = T, stringsAsFactors = F)

#Check for duplicates
rounds2[which(duplicated(rounds2[,c("company_permalink","funding_round_permalink")])),]

#Table 1.1 : Understand the data set 

#Convert both the keys to same case to prevent issues while merging
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# How many unique companies are present in rounds2?
# 66368

length(unique(rounds2$company_permalink))

# How many unique companies are present in companies ?
# 66368 

length(unique(companies$permalink))

#In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column
# permalink

str(companies)
summary(companies)

# Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
# N

setdiff(companies$permalink, rounds2$company_permalink)

# Merge the two data frames so that all  variables (columns)  in the companies frame 
# are added to the rounds2 data frame. 
# Name the merged frame master_frame. How many observations are present in master_frame ?
# 114949

master_frame <- merge(companies,rounds2,by.x="permalink",by.y="company_permalink",all=F)
nrow(master_frame)

##############################################################################################################
#
#                                                  PART 1 (Checkpoint 2) 
#
##############################################################################################################

# Question 1: How many NA values are present in the column raised_amount_usd ?
# Answer : 19990

sum(is.na(master_frame$raised_amount_usd))

# Question 2: What do you replace NA values of raised_amount_usd  with? 
# Answer 2: We first use a plot to determine outlier values

plot(density(master_frame$raised_amount_usd, na.rm=TRUE))

#Replace all the NA values from the raised_amount_usd column of the master frame.

#Have taken the average mean and assigned it to all the na values.
# May need to see how else we can achieve it:
#   1. Take sector wise mean and assign it to the null values in that sector
#   2. Identify Outliers and exclude them from mean calculation

summary(na.exclude(master_frame$raised_amount_usd))
mean_raised_amount_raised <- mean(master_frame$raised_amount_usd, na.rm = T)

master_frame_na_values <- which(is.na(master_frame$raised_amount_usd))
master_frame[master_frame_na_values,]$raised_amount_usd <- mean_raised_amount_raised
summary(master_frame$raised_amount_usd)

sum(is.na(master_frame$raised_amount_usd))

##############################################################################################################
#
#                                                  PART 1 (Checkpoint 3) 
#
##############################################################################################################


str(master_frame)
unique(master_frame$funding_round_type)

# Average funding amount of venture type
# 11623493

mean(subset(master_frame,funding_round_type=="venture")$raised_amount_usd)

# Average funding amount of angel type
# 2875946

mean(subset(master_frame,funding_round_type=="angel")$raised_amount_usd)

# Average funding amount of seed type	 
# 2920791

mean(subset(master_frame,funding_round_type=="seed")$raised_amount_usd)

# Average funding amount of private equity type
# 63704339

mean(subset(master_frame, funding_round_type=="private_equity")$raised_amount_usd)

# Question 5 : Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, 
#             which investment type is the most suitable for them?
# Answer 5 :  Venture Type

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 4) 
#
##############################################################################################################

# Question 1: Spark Funds wants to see the top 9 countries which have received the highest total 
#             funding (across ALL sectors for the chosen investment type).
# Answer 1:   Chosen investment type is venture capital.

selected_venture_subset <- subset(master_frame, funding_round_type=="venture")

country <- aggregate(raised_amount_usd ~ country_code, data = selected_venture_subset, FUN = sum)

# For the chosen investment type, make a data frame named top9 with top9 countries 
# (based on the total investment amount each country has received).
country_ordered <- country[order(country$raised_amount_usd, decreasing = TRUE),]
top9 <- head(unique(country_ordered$country_code), n = 9)
top9

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 5) 
#
##############################################################################################################

# Use mapping file "mapping.csv" to map each primary sector to one of the 8 main sectors. 
# (Note that ‘Others’ is also considered one of the main sector)

mapping_list <- read.csv("Data/mapping_file.csv", header = T, stringsAsFactors = F)
str(mapping_list)
mapping_list$category_list <- tolower(mapping_list$category_list)

# Extract the primary sector of each category list from the category_list column

master_frame$primary_sector <- tolower(gsub("\\|.*","", master_frame$category_list))
head(master_frame[,c("category_list","primary_sector")])

master_frame <- merge(master_frame,mapping_list, by.x = "primary_sector", by.y="category_list", all.x=T)
head(master_frame[master_frame$main_sector == "Others",c("category_list","primary_sector","main_sector")])

# Check for Null values after Merge
# Assigning the Nulls to the mainsector Others (Not sure about this either)
sum(is.na(master_frame$main_sector.y))
unique(master_frame[which(is.na(master_frame$main_sector.y)),"primary_sector"])
master_frame[which(is.na(master_frame$main_sector)),]$main_sector <- "Others"
sum(is.na(master_frame$main_sector))

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 6) 
#
##############################################################################################################
# Question :  Create three separate data frames D1, D2 and D3 for each of the 3 countries containing 
#             the observations of funding type FT  falling between 5 to 15 million USD range. 
#             The three data frames should contain:

#                 All the columns of the master_frame along with the primary sector and the main sector
#                 The total number (or count) of investments for each main sector in a separate column
#                 The total amount invested in each main sector in a separate column
#
# Answer : From previous analysis, the top 3 english countries are USA, IND and GBR


#Do we need to group the results based on the company as well. 
# thats what would give the unique no of investments?
# Havent done it yet


D1 <- subset(master_frame,master_frame$country_code == "USA" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D2 <- subset(master_frame,master_frame$country_code == "GBR" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D3 <- subset(master_frame,master_frame$country_code == "IND" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)

#Total Number of investments & Total amount of investments
nrow(D1)
sum(D1$raised_amount_usd)

nrow(D2)
sum(D2$raised_amount_usd)

nrow(D3)
sum(D3$raised_amount_usd)

#Sector Wise Number of investments
freq_counts_USA <- as.data.frame(table(D1$main_sector))
colnames(freq_counts_USA) <- c ("main_sector","No. of Investments")
str(freq_counts_USA)

freq_counts_GBR <- as.data.frame(table(D2$main_sector))
colnames(freq_counts_GBR) <- c ("main_sector","No. of Investments")

freq_counts_IND <- as.data.frame(table(D3$main_sector))
colnames(freq_counts_IND) <- c ("main_sector","No. of Investments")

D1 <- merge(D1,freq_counts_USA,by="main_sector", all.x=T)
D2 <- merge(D2,freq_counts_GBR,by="main_sector", all.x=T)
D3 <- merge(D3,freq_counts_IND,by="main_sector", all.x=T)

#Sector Wise Total amount of investments
sum_USA <- aggregate(raised_amount_usd ~ main_sector, D1, FUN = sum)
colnames(sum_USA) <- c ("main_sector","Total Amount Invested")

sum_GBR <- aggregate(raised_amount_usd ~ main_sector, D2, FUN = sum)
colnames(sum_GBR) <- c ("main_sector","Total Amount Invested")

sum_IND <- aggregate(raised_amount_usd ~ main_sector, D3, FUN = sum)
colnames(sum_IND) <- c ("main_sector","Total Amount Invested")

D1 <- merge(D1,sum_USA,by="main_sector", all.x=T)
D2 <- merge(D2,sum_GBR,by="main_sector", all.x=T)
D3 <- merge(D3,sum_IND,by="main_sector", all.x=T)

#Find the Top 3 sectors based on number of investments
D1 <- D1[order(D1$`No. of Investments`, decreasing = TRUE), ]
D2 <- D2[order(D2$`No. of Investments`, decreasing = TRUE), ]
D3 <- D3[order(D3$`No. of Investments`, decreasing = TRUE), ]

unique(D1$main_sector)
summary_D1 <- merge(freq_counts_USA,sum_USA)
summary_D2 <- merge(freq_counts_GBR,sum_GBR)
summary_D3 <- merge(freq_counts_IND,sum_IND)

summary_D1 <- summary_D1[order(summary_D1$`No. of Investments`, decreasing = TRUE), ]
summary_D2 <- summary_D2[order(summary_D2$`No. of Investments`, decreasing = TRUE), ]
summary_D3 <- summary_D3[order(summary_D3$`No. of Investments`, decreasing = TRUE), ]

str(D1)

summary_D1
head(D1[D1$main_sector=="Others","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D1[D1$main_sector=="Others",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

head(D1[D1$main_sector=="Social, Finance, Analytics, Advertising","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D1[D1$main_sector=="Social, Finance, Analytics, Advertising",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

summary_D2
head(D2[D2$main_sector=="Others","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D2[D2$main_sector=="Others",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)
D2[D2$main_sector=="Social, Finance, Analytics, Advertising","name"]
top_company <- aggregate(raised_amount_usd ~ name, D2[D2$main_sector=="Social, Finance, Analytics, Advertising",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)


summary_D3
head(D3[D3$main_sector=="Others","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D3[D3$main_sector=="Others",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)
head(D3[D3$main_sector=="Social, Finance, Analytics, Advertising","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D3[D3$main_sector=="Social, Finance, Analytics, Advertising",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

str(summary_D3)
barplot(summary_D3$`No. of Investments`,summary_D3$main_sector)


##############################################################################################################
#
#                                                  Clarifications and Doubts 
#
##############################################################################################################
# 1. Use of Mean to fill the na values
# 2. Setting the main_sector to Others for the ones where no match is found in mappings file
# Eg : Golf Equipment
# Google Glass
# 3. No of investments - Does it refer to unique company investments or each investment should be counted.
# 4. Company recieving highestinvestment - Do we need to toal the amount raised in all funding rounds combined
# or is it the single highest investment recieved in a single round

