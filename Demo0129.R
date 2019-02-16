#Question 1.
#a)
c(1:30, 50:80)
#b)
seq(from = 0, to = 1000, by = 50)
#c)
rep(c(1,3),times = 10)
#d)
rep(c(3,30), each = 10)
#e)
sort(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), decreasing = T)
#f)
rev(10:1)
#g)
unique((c('a','b','b','c')))
#h)
a = c(12, 4, 6, 73, 67, 84, 45, 74, 5, 52, 35)
a[c(3,6)]
#i)
a[-c(3,6)]
#j)
a[a < 10]
#k)
i <- 1
while (i < 11) {
  print(i)
  i = i+1
}
#l)
if(i == 3) print("Yes") else print("No")
#m)
a = letters
class(a)
#n)
as.numeric(c('1', '3', '6'))
#o)
as.character(c(6, 3, 1))
#p)
rank(c(12,4, 6, 73, 67, 84, 45, 74, 5, 52, 35))
#q)
var(c(12, 4, 6, 73, 67, 84, 45, 74, 5, 52, 35))
#r)
ls()
#s)
rm(list = ls())
#t)
m = matrix(c(1:9), nrow = 3, ncol = 3)
#u)
X <-c(1:10)
Y <-c(11:20)
Z <-c(21:30)
cbind(X,Y,Z)
#v)
cbind(X)
#w)
nchar('letter')
#x)
toupper('letter')
####################
#Question 2.
#a)
library("tidyverse")
ridmov <- read.csv(file.choose())
#b)
head(ridmov,n=10)
#c)
summary(ridmov)
#d)
ridmov %>%
  group_by(Ownership) %>%
  summarise(avg_income = mean(Income)) %>%
  ungroup()
ridmov %>%
  group_by(Ownership) %>%
  summarise(avg_lotsize = mean(Lot_Size)) %>%
  ungroup()
ridmov %>%
  group_by(Ownership) %>%
  summarise(std_income = sd(Income)) %>%
  ungroup()
ridmov %>%
  group_by(Ownership) %>%
  summarise(std_income = sd(Lot_Size)) %>%
  ungroup()
#e)
ridmov %>%
  filter(Income <= 50 & Ownership == "Owner")
#f)
ridmov %>%
  filter(Income >= 50 | Lot_Size >= 10 )
#g)
ridmov %>%
  distinct(Income, .keep_all = T)
#h)
filter(ridmov, row_number() == seq(from = 3, to = n(), by = 3))
#i)
ridmov %>%
  arrange(Income)
#j)
temp1 = ridmov %>%
  arrange(Income)
temp1 %>%
  arrange(desc(Lot_Size))
#k)
ridmov %>%
  mutate(Income_Cy = Income * 6.7,
         Lot_Size_Sy = Lot_Size * 3)
#l)
ridmov %>%
  mutate(cumsum = cumsum(Income),
         cummean = cummean(Income))
#m)
temp1 = ridmov %>%
  select(Income)

temp2 = ridmov %>%
  select(Lot_Size)

temp3 = temp1 %>%
  bind_cols(temp2)
#n)
temp1 = ridmov %>%
  filter(Ownership == "Owner")

temp2 = ridmov %>%
  filter(Ownership == "Nonowner")

temp3 = temp1 %>%
  bind_rows(temp2)
#o)
ridmov %>%
  mutate(Household_Number = 1:nrow(ridmov))
#p)
temp1 = ridmov %>%
  mutate(Household_Number = 1:nrow(ridmov)) %>%
  slice(1:16) %>%
  select(Household_Number,Income)
#q)
temp2 = ridmov %>%
  mutate(Household_Number = 1:nrow(ridmov)) %>%
  slice(9:24) %>%
  select(Household_Number,Lot_Size)
#r)
temp2 %>% 
  right_join(temp1, by = "Household_Number")
#s)
temp2 %>% 
  left_join(temp1, by = "Household_Number")
#t)
temp1 %>% 
  inner_join(temp2, by = "Household_Number")
#u)
temp1 %>% 
  full_join(temp2, by = "Household_Number")
#v)
a = mean(temp1$Income, na.rm = TRUE)
b = mean(temp1$Lot_Size, na.rm = TRUE)
temp2 <- temp1 %>%
  replace_na(list(Income = a))
temp2 %>%
  replace_na(list(Lot_Size = b))
#w)
library(ggplot2)
p = ggplot(ridmov, aes(x = Income, y = Lot_Size)) +  geom_point(size = 2, colour = "red") +
    labs(x = "Income ($000s)", y = "Lot_Size (000 ft2)")
print(p)
#x)
p = ggplot(ridmov, aes(x = Ownership, y = Income)) + geom_boxplot(fill = "grey") +
  labs(x = "Ownership of Riding Mower", y = "Income ($000s)")
print(p)