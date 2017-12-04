data1 <-read.csv("getdata-data-ss06hid.csv")
str(data1)
data2 <- filter(data1, (ACR ==3 & AGS == 6))
file1 <- read.csv("getdata-data-GDP.csv")
file2 <- read.csv("getdata-data-EDSTATS_Country.csv")
zfiles <- merge(file1,file2,by.x = "X",by.y = "CountryCode",all=TRUE)
intersect(names(file1),names(file2))
arrange(join(file1,file2),X)


means <- filter(zfiles, Income.Group %in% c("High income: OECD","High income: nonOECD"))
oecds <- group_by(means, Income.Group)
summarise(oecds, Ranking = mean(Ranking,na.rm = TRUE))