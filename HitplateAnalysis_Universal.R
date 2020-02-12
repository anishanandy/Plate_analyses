##### Library Calls #####

library("tidyverse")
library("reshape")



##### Initial input section #####

#The original datafile in .csv format
df <- read.csv("data/HTSplate_3.csv", header = FALSE)

#The number of time points taken
timepoints = 9

#The number of technical replicates
replicate = 2

#The number of readings per hour
read.hour = 1

#The Treshold for significance for variations
treshold = 0.01

#The path to the csv file that contains the list of all variation > than the threshold
Non0_Ouput <- "NonZero_ODVariationttestalex.csv"


##### Data wrangling section #####

#Removes the empty lines in the .csv files
df %>% 
  select(-1) %>% 
  filter(!is.na(V2))-> df2

#Make a list of nested dataframes for each time point
chunk <- 17
n <- nrow(df2)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
list_df <- split(df2,r)
remove(n)
remove(r)
remove(chunk)
remove(df2)

#Rename the nested dataframes with the time point and replicate
names.1 <- sprintf("t%d",rep(seq(1:timepoints), each=replicate))
names.2 <- rep(sprintf(".%d", seq(1:replicate)),timepoints)
fill <- paste0(names.1, names.2)
names(list_df) <- fill
remove(names.1)
remove(names.2)

#Remove first row from each dataframes
list_df <- lapply(list_df, function(x) {
  slice(x, 2:17)
})

#Make dataframes into single column
list_df_single <- lapply(list_df, function(x) {
  x <- data.frame(unlist(x))
})

#Creating the vertical ID column
ID.V <- c("A01", "B01", "C01", "D01", "E01", "F01", "G01", "H01", "I01", "J01", "K01", "L01", "M01",
        "N01", "O01", "P01", "A02", "B02", "C02", "D02", "E02", "F02", "G02", "H02", "I02", "J02",
        "K02", "L02", "M02", "N02", "O02", "P02", "A03", "B03", "C03", "D03", "E03", "F03", "G03",
        "H03", "I03", "J03", "K03", "L03", "M03", "N03", "O03", "P03", "A04", "B04", "C04", "D04",
        "E04", "F04", "G04", "H04", "I04", "J04", "K04", "L04", "M04", "N04", "O04", "P04", "A05",
        "B05", "C05", "D05", "E05", "F05", "G05", "H05", "I05", "J05", "K05", "L05", "M05", "N05",
        "O05", "P05", "A06", "B06", "C06", "D06", "E06", "F06", "G06", "H06", "I06", "J06", "K06",
        "L06", "M06", "N06", "O06", "P06", "A07", "B07", "C07", "D07", "E07", "F07", "G07", "H07",
        "I07", "J07", "K07", "L07", "M07", "N07", "O07", "P07", "A08", "B08", "C08", "D08", "E08",
        "F08", "G08", "H08", "I08", "J08", "K08", "L08", "M08", "N08", "O08", "P08", "A09", "B09",
        "C09", "D09", "E09", "F09", "G09", "H09", "I09", "J09", "K09", "L09", "M09", "N09", "O09",
        "P09", "A10", "B10", "C10", "D10", "E10", "F10", "G10", "H10", "I10", "J10", "K10", "L10",
        "M10", "N10", "O10", "P10", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11",
        "J11", "K11", "L11", "M11", "N11", "O11", "P11", "A12", "B12", "C12", "D12", "E12", "F12",
        "G12", "H12", "I12", "J12", "K12", "L12", "M12", "N12", "O12", "P12", "A13", "B13", "C13",
        "D13", "E13", "F13", "G13", "H13", "I13", "J13", "K13", "L13", "M13", "N13", "O13", "P13",
        "A14", "B14", "C14", "D14", "E14", "F14", "G14", "H14", "I14", "J14", "K14", "L14", "M14",
        "N14", "O14", "P14", "A15", "B15", "C15", "D15", "E15", "F15", "G15", "H15", "I15", "J15",
        "K15", "L15", "M15", "N15", "O15", "P15", "A16", "B16", "C16", "D16", "E16", "F16", "G16",
        "H16", "I16", "J16", "K16", "L16", "M16", "N16", "O16", "P16", "A17", "B17", "C17", "D17",
        "E17", "F17", "G17", "H17", "I17", "J17", "K17", "L17", "M17", "N17", "O17", "P17", "A18",
        "B18", "C18", "D18", "E18", "F18", "G18", "H18", "I18", "J18", "K18", "L18", "M18", "N18",
        "O18", "P18", "A19", "B19", "C19", "D19", "E19", "F19", "G19", "H19", "I19", "J19", "K19",
        "L19", "M19", "N19", "O19", "P19", "A20", "B20", "C20", "D20", "E20", "F20", "G20", "H20",
        "I20", "J20", "K20", "L20", "M20", "N20", "O20", "P20", "A21", "B21", "C21", "D21", "E21",
        "F21", "G21", "H21", "I21", "J21", "K21", "L21", "M21", "N21", "O21", "P21", "A22", "B22",
        "C22", "D22", "E22", "F22", "G22", "H22", "I22", "J22", "K22", "L22", "M22", "N22", "O22",
        "P22", "A23", "B23", "C23", "D23", "E23", "F23", "G23", "H23", "I23", "J23", "K23", "L23",
        "M23", "N23", "O23", "P23", "A24", "B24", "C24", "D24", "E24", "F24", "G24", "H24", "I24",
        "J24", "K24", "L24", "M24", "N24", "O24", "P24")
tAll.raw <- as.data.frame(ID.V)

#Merge dataframes into 1
list_ID <- lapply(list_df_single, function(x){
  as.data.frame(append(tAll.raw, x))
})

merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by="ID.V")
}

tAll.raw <- Reduce(merge.all, list_ID)
colnames(tAll.raw)[1] <- "ID.H"
ID.H <- as.vector(tAll.raw$ID.H)
names.1 <- sprintf("t%d",rep(seq(1:timepoints), each=replicate))
names.2 <- rep(sprintf(".%d", seq(1:replicate)),timepoints)
dataset.vector <- paste0(names.1, names.2)
fill <- append("ID.H", dataset.vector)
colnames(tAll.raw) <- fill
remove(merge.all)
remove(fill)
remove(list_ID)
remove(list_df)



##### Data analysis section #####

#Obtain the dataframe of the averages
as.data.frame(within(tAll.raw, {
  pair.colmeans <- sapply(seq(2, ncol(tAll.raw), 2), function(i) {
    rowMeans(tAll.raw[, c(i, i+1)], na.rm=TRUE)
    })
  })) -> intermediate
as.data.frame(intermediate$pair.colmeans, drop = F) -> tAll
colnames(tAll) <- (sprintf("t%d",rep(seq(1:timepoints))))
row.names(tAll) <- tAll.raw$ID.H
remove(intermediate)

#Transpose and melt the dataframe for easier graphing
time.vector <- seq(from = 0, to = (timepoints/read.hour)-(1/read.hour), by = (1/read.hour))
tAll.t <- as.data.frame(t(tAll))
tAll.t %>% 
  mutate(time = time.vector) %>% 
  select(time, A01:P24) -> tAll.t
tAll.long <- melt(tAll.t, id="time")

#Compute the difference between the highest and last point
tAll.t %>%
  select(A01:P24) -> tAll_diff

colMax <- function(data) sapply(data, max, na.rm = TRUE)

tAll_max <- as.data.frame(colMax(tAll_diff))
colnames(tAll_max) <- c("max")
tAll_last <- as.data.frame(t(slice(tAll_diff, timepoints)))
colnames(tAll_last) <- c("last")

tAll_diff <- bind_cols(tAll_max, tAll_last)
tAll_diff %>% 
  mutate(diff = max-last) %>% 
  mutate(ID = ID.H) %>% 
  select(ID, everything()) -> tAll_diff
remove(tAll_last)
remove(tAll_max)

#compute relative difference
tAll_diff %>%
  mutate(rel.diff = 1-(last/max)) -> tAll_diff
  
#Extract all differences above the treshold
tAll_diff %>% 
  filter(diff>treshold) -> tAll_diff_hits

#Separates the curve data into hits and non hits
hit.ID <- as.vector(tAll_diff_hits$ID)
tAll.long %>%
  filter(variable %in% hit.ID) %>% 
  mutate(hit = "hit")-> tAll.hits

tAll.long %>%
  filter(!(variable %in% hit.ID)) %>% 
  mutate(hit = "No.hit")-> tAll.NotHits

tAll.long <- union(tAll.hits, tAll.NotHits)
remove(tAll.hits)
remove(tAll.NotHits)

##### Output section #####

#Extract csv of hits
write.csv(tAll_diff_hits, file = Non0_Ouput, row.names = FALSE)



##### Plotting Section #####

#Example of a plot with low number of variables
ggplot() +
  geom_line(data = tAll.t,
            aes(x = time, y = O22, color = "O22"),
            size = 2) +
  geom_line(data = tAll.t,
            aes(x =time, y = F22,  color = "F22"),
            size = 2) +
  geom_line(data = tAll.t,
            aes(x =time, y = N18,  color = "N18"),
            size = 2) +
  scale_color_manual(values = c(
    "O22" = "#7A003C",
    "F22" = "#FDBF57",
    "N18" = "#5E6A71")) +
  labs(x = "Time (H)", y = expression(OD[600]), color = "Well")

#Plot all variables together with hits highlited and none hits in grey
ggplot(subset(tAll.long, hit %in% c("hit"))) +
  geom_line(aes(x = time,
                y = value,
                group = variable,
                colour = hit)) +
  scale_color_manual(values = c("#7A003C")) + 
  expand_limits(x=c(0,4), y=c(0.15, 0.4))

ggplot(subset(tAll.long, hit %in% c("No.hit"))) +
  geom_line(aes(x = time,
                y = value,
                group = variable,
                colour = hit)) +
  scale_color_manual(values = c("#5E6A71")) + 
  expand_limits(x=c(0,4), y=c(0.15, 0.4))


#Plot the Histogram of difference between max and last (removes the 0s)
ggplot(data=tAll_diff_hits,
       aes(tAll_diff_hits$diff)) +
  geom_histogram(binwidth = 0.001,
                 color = "#5E6A71",
                 fill = "#7A003C") + 
  theme(legend.position="none") +
  ylim(0,5) +
  labs(x = expression(OD[600]~Variation), y = "Count")