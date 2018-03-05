#set working environment folder
setwd("C:/dev/Kaggle/kickstarter_proj")

#read CSV file including header
kickstart <- read.csv("ks-projects-201801.csv", header = TRUE)

#create a backup dataset
ksproj <- kickstart

#remove state projects as "live" and "undefined"
ksproj <- subset(kickstart, 
                 state != "live" & state != "undefined",
                 select = category:usd_goal_real)

#View(ksproj)

#convert categorical variables into numerical ones
ksproj$state_int <- as.integer(ksproj$state)
ksproj$subcat_int <- as.integer(ksproj$category)
ksproj$cat_int <- as.integer(ksproj$main_category)
ksproj$ctry_int <- as.integer(ksproj$country)

ksproj <- within(ksproj, state_int != 4 <- 1)
ksproj <- within(ksproj, state_int == 4 <- 0)

test <- ksproj[1:30,]

ksproj <- within(test,{
  state_int <- ifelse( xor(state_int != 4, state_int == 4),1,0)
})

# 1-canceled, 2-failed, 3-live, 4-successful, 5-suspended, 6-undefined

to_run <- subset(ksproj, select = c(14,16,15,17,12,13,9))

#View(to_run)

#set proportion of train and test datasets
smp_size <- floor(0.8 * nrow(to_run))
set.seed(123)

#select a random sample
train_ind <- sample(seq_len(nrow(to_run)), size = smp_size)

train <- to_run[train_ind, ]
test <- to_run[-train_ind, ]


#head(train1)

model <- glm(state_int ~.,family=binomial(link='logit'),data=train)

