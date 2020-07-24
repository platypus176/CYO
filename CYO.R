#############################################################
# Choose your own project: Pokemon Combats
# Dale Chen-Song
#############################################################

# Install packages if necessary, and load them
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr")
if(!require(ggcorrplot)) install.packages("ggcorrplot")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(randomForest)) install.packages("randomForest")

##################
# Create Database
##################
# Link: https://www.kaggle.com/terminus7/pokemon-challenge

# Download  and create database
pokemon <- fread("pokemon.csv")
combat <-fread("combat.csv")
test <-fread("tests.csv")

# Tidy data
names(pokemon)[1] <- "Number"


##################
# Data Observation
##################

#----- Pokemon Dataset------
head(pokemon)
dim(pokemon)

## Groupings
# Mega
mega <- pokemon %>% filter(str_detect(Name, "Mega ")==TRUE| str_detect(Name, "Primal")==TRUE)
head(mega)
mega %>% count() %>% pull()

# Legendary
legendary <- pokemon %>% filter(Legendary == TRUE)
head(legendary)
legendary %>% count() %>% pull()

# Mega Legendary
mega_legendary <- mega %>% filter(Legendary==TRUE)
head(mega_legendary)
mega_legendary %>% count() %>% pull()

# Different Forms
pokemon[grep("Rotom", Name), ]


## Type
n_distinct(pokemon$`Type 1`)

# Number of Single Type
pokemon %>% group_by(`Type 2`) %>% filter(str_detect(`Type 2`, "") == FALSE) %>%summarize(tot=n())%>% pull()

# Type 1 total
type_1<-pokemon %>% group_by(`Type 1`) %>% summarize(Total_1=n())
names(type_1)[1]<- "Type"
# Type 2 total
type_2<-pokemon %>% group_by(`Type 2`) %>% filter(str_detect(`Type 2`, "") == TRUE) %>%summarize(Total_2=n())
names(type_2)[1]<- "Type"
# Join types together
type<-inner_join(type_1,type_2)
type$Total<- type$Total_1+type$Total_2
type <- type %>% arrange(desc(Total))

# Graph for Pokemon Type Distribution
par(las =2)
barplot(type$Total, names.arg = type$Type, main="Pokemon Type Distribution", cex.names= 0.8, horiz=TRUE)

# Legendary Type Distribution
# Type 1 total
leg_type_1<-legendary %>% group_by(`Type 1`) %>% summarize(Total_1=n())
names(leg_type_1)[1]<- "Type"

# Type 2 total
leg_type_2<-legendary %>% group_by(`Type 2`) %>% filter(str_detect(`Type 2`, "") == TRUE) %>%summarize(Total_2=n())
names(leg_type_2)[1]<- "Type"

# Join type together
leg_type<-full_join(leg_type_1, leg_type_2)
leg_type<- leg_type %>% add_row(Type = "Bug")
leg_type<- leg_type %>% add_row(Type = "Poison")
leg_type[is.na(leg_type)]<-0
leg_type$Total<- leg_type$Total_1+leg_type$Total_2
leg_type <- leg_type %>% arrange(desc(Total))

# Graph for Legendary Type Distribution
par(las =2)
barplot(leg_type$Total, names.arg = leg_type$Type, main="Legendary Type Distribution", cex.names= 0.8, horiz=TRUE)

## Pokemon Stats

# Base Stat Total (BST)
pokemon$BST<- pokemon$HP + pokemon$Attack + pokemon$Defense + 
  pokemon$`Sp. Atk` + pokemon$`Sp. Def` + pokemon$Speed
head(pokemon)

# Scyther Evolution
pokemon[133,]
pokemon[229,]

# Mega BST
mega$BST<- mega$HP + mega$Attack + mega$Defense + 
  mega$`Sp. Atk` + mega$`Sp. Def` + mega$Speed
# Getting rid of duplicate Mega Evolution (Charizard and Mewtwo have 2 Mega Evolution)
nonmega<-mega[-c(3,15),]
# Obtain base form
nonmega<-nonmega$Number-1
nonmega<-pokemon %>% filter(pokemon$Number %in% nonmega)
# Join both 
bothmega<-full_join(nonmega,mega) %>% arrange(Number) %>% select(Number, Name, BST)
head(bothmega)

# Top BST
BST<-pokemon %>% select(Number, Name, BST, Legendary) %>% arrange(desc(BST))
head(BST)
hist(pokemon$BST, main = "BST distribution", xlab = "BST")

# BST summary of all Pokemon
pokemon %>% select(BST) %>% summary(pokemon)
# BST summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(BST) %>% summary(BST)
# BST summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(BST) %>% summary(BST)
# BST summary of Mega
mega %>% select(BST) %>% summary(BST)

# HP
hp<-pokemon %>% select(Number, Name, HP, Legendary) %>% arrange(desc(HP))
head(hp)
hist(pokemon$HP, main = "HP distribution", xlab = "HP")
# HP summary of all Pokemon
pokemon %>% select(HP) %>% summary(pokemon)
# HP summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(HP) %>% summary(HP)
# HP summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(HP) %>% summary(HP)
# HP summary of Mega
mega %>% select(HP) %>% summary(HP)
# Top HP 
legendary %>% filter(HP == max(HP)) %>% select(Number, Name, HP, Legendary)
mega %>% filter(HP == max(HP)) %>% select(Number, Name, HP, Legendary)


# Attack
atk<-pokemon %>% select(Number, Name, Attack, Legendary) %>% arrange(desc(Attack))
head(atk)
hist(pokemon$Attack, main = "Attack distribution", xlab = "Attack")
# Top Attack
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  filter(Attack == max(Attack)) %>% select(Number, Name, Attack, Legendary)
# Attack summary of all Pokemon
pokemon %>% select(Attack) %>% summary(pokemon)
# Attack summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(Attack) %>% summary(Attack)
# HP summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(Attack) %>% summary(Attack)
# HP summary of Mega
mega %>% select(Attack) %>% summary(Attack)


# Defense
def<-pokemon %>% select(Number, Name, Defense, Legendary) %>% arrange(desc(Defense))
head(def)
hist(pokemon$Defense, main = "Defense distribution", xlab = "Defense")
# Defense summary of all Pokemon
pokemon %>% select(Defense) %>% summary(pokemon)
# Defense summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(Defense) %>% summary(Defense)
# HP summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(Defense) %>% summary(Defense)
# HP summary of Mega
mega %>% select(Defense) %>% summary(Defense)


# Special Attack
satk<- pokemon %>% select(Number, Name, `Sp. Atk`, Legendary) %>% arrange(desc(`Sp. Atk`))
head(satk)
hist(pokemon$`Sp. Atk`, main = "Sp. Atk distribution", xlab = "Sp. Atk")
# Sp. Atk summary of all Pokemon
pokemon %>% select(`Sp. Atk`) %>% summary(pokemon)
# Sp. Atk summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(`Sp. Atk`) %>% summary(`Sp. Atk`)
# Sp. Atk summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(`Sp. Atk`) %>% summary(`Sp. Atk`)
# Sp. Atk summary of Mega
mega %>% select(`Sp. Atk`) %>% summary(`Sp. Atk`)


# Special Defense
sdef<- pokemon %>% select(Number, Name, `Sp. Def`, Legendary) %>% arrange(desc(`Sp. Def`))
head(sdef)
hist(pokemon$`Sp. Def`, main = "Sp. Def distribution", xlab = "Sp. Def")
# Sp. Def summary of all Pokemon
pokemon %>% select(`Sp. Def`) %>% summary(pokemon)
# Sp. Def summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(`Sp. Def`) %>% summary(`Sp. Def`)
# Sp. Def summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(`Sp. Def`) %>% summary(`Sp. Def`)
# Sp. Def summary of Mega
mega %>% select(`Sp. Def`) %>% summary(`Sp. Def`)


# Speed
speed<- pokemon %>% select(Number, Name, Speed, Legendary) %>% arrange(desc(Speed))
head(speed)
hist(pokemon$Speed, main = "Speed distribution", xlab = "Speed")
# Speed summary of all Pokemon
pokemon %>% select(Speed) %>% summary(pokemon)
# Speed summary of non Legendary and Mega
pokemon %>% filter(str_detect(Name, "Mega ")==FALSE& str_detect(Name, "Primal")==FALSE & Legendary == FALSE) %>% 
  select(Speed) %>% summary(Speed)
# Speed summary of Legendary
pokemon %>% filter(Legendary== TRUE) %>% select(Speed) %>% summary(Speed)
# Speed summary of Mega
mega %>% select(Speed) %>% summary(Speed)


#Other

#minimum stats
min <- pokemon %>%  select(HP, Attack, Defense, `Sp. Atk`, `Sp. Def`, Speed)
apply(min,2,min)
#finding the 1 HP Pokemon
pokemon %>% filter(HP==1)

#----- Combat Dataset------
head(combat)
dim(combat)

# Finding number of Wins per Pokemon
numberWins <- combat %>% group_by(Winner) %>% count()
head(numberWins)
dim(numberWins)

# Figuring out which Pokemon didn't battle
anti_join(pokemon, numberWins, by = c("Number"="Winner"))

# normalize first pokemon
firstCount <- combat %>% group_by(First_pokemon) %>% count() 
head(firstCount)
dim(firstCount)

# normalize second pokemon
secondCount <- combat %>% group_by(Second_pokemon) %>% count()
head(secondCount)
dim(secondCount)

# Finding the Pokemon that couldn't win
neverWon <- firstCount$First_pokemon[!firstCount$First_pokemon %in% numberWins$Winner]
pokemon[pokemon$Number == neverWon,]

# Adding Shuckle
numberWins <- rbind(numberWins, c(Winner= 231, n =0))
numberWins <- numberWins[order(numberWins$Winner),]

# Win Percentage
# Obtain total battles for each pokemon
numberWins$Total_Battles <- firstCount$n + secondCount$n
# Win percentage
numberWins$Win_Percentage <- numberWins$n/numberWins$Total_Battles
head(numberWins)

# Top Win rate
pokemon<-full_join(pokemon, numberWins, by = c("Number" = "Winner"))
pokemon <- pokemon %>% rename(Wins = n)
top_winner<-pokemon %>% group_by(Win_Percentage) %>% select(-`Type 1`, -`Type 2`, -Generation, - Wins, -Total_Battles) %>%
  arrange(desc(Win_Percentage))
head(top_winner)

# Bottom Win rate
top_loser <-top_winner %>% arrange(Win_Percentage)
head(top_loser)

# Average Win Percentage by Type 1
type_win_rate_1 <- drop_na(pokemon)
type_win_rate_1 <- aggregate(type_win_rate_1$Win_Percentage, by=list(type_win_rate_1$`Type 1`), FUN = mean)
type_win_rate_1 <- type_win_rate_1 %>% rename(Type = Group.1, Win_Percentage_1 = x) %>% arrange(desc(Win_Percentage_1))
# Average Win Percentage by Type 2
type_win_rate_2 <- drop_na(pokemon)
type_win_rate_2 <- aggregate(type_win_rate_2$Win_Percentage, by=list(type_win_rate_2$`Type 2`), FUN = mean)
type_win_rate_2 <- type_win_rate_2[-1, ]
type_win_rate_2 <- type_win_rate_2 %>% rename(Type = Group.1, Win_Percentage_2 = x) %>% arrange(desc(Win_Percentage_2))
# Average Win Percentage by Type
type_win_rate <- full_join(type_win_rate_1, type_win_rate_2)
type_win_rate <- full_join(type, type_win_rate)
type_win_rate <- type_win_rate %>% 
  mutate(Total_Win_Percentage = ((Win_Percentage_1 * Total_1)+(Win_Percentage_2 * Total_2))/Total) %>% 
  select(Type, Total_Win_Percentage) %>% arrange(desc(Total_Win_Percentage))
type_win_rate

# Correlation by Stats
stats<-pokemon %>% select(-Number, -Name, -`Type 1`, - `Type 2`, -Generation, -Legendary, -Wins, -Total_Battles)
stats<-drop_na(stats)
stats<-signif(cor(stats),2)
ggcorrplot(stats, lab = TRUE)

# Plot stats
ggplot(pokemon, aes(x = Speed, y =  Win_Percentage)) + geom_point()+ geom_smooth()
ggplot(pokemon, aes(x = BST, y =  Win_Percentage)) + geom_point()+ geom_smooth()
ggplot(pokemon, aes(x = Attack, y =  Win_Percentage)) + geom_point()+ geom_smooth()
ggplot(pokemon, aes(x = `Sp. Atk`, y =  Win_Percentage)) + geom_point()+ geom_smooth()

# Win Percentage by Groups
# Win Percentage by Legendary
leg_win_rate <- drop_na(pokemon)
leg_win_rate <- aggregate(leg_win_rate$Win_Percentage, by=list(leg_win_rate$Legendary), FUN = mean)
leg_win_rate

# Win Percentage by Mega 
mega_win_rate<- left_join(mega, pokemon)
mega_win_rate<- drop_na(mega_win_rate)
mega_win_rate<- sum(mega_win_rate$Wins)/sum(mega_win_rate$Total_Battles)
mega_win_rate

# Win Percentage by Mega Legendary
mega_leg_win_rate<- left_join(mega_legendary, pokemon)
mega_leg_win_rate<- drop_na(mega_leg_win_rate)
mega_leg_win_rate<- sum(mega_leg_win_rate$Wins)/sum(mega_leg_win_rate$Total_Battles)
mega_leg_win_rate

####################
# Analysis/Method
####################
# Combining datasets
fight<-combat
# Join two dataset together by Number
fight<- fight %>% left_join(pokemon, by= c("First_pokemon" = "Number")) %>% 
  left_join(pokemon, by = c("Second_pokemon" = "Number")) %>% select(-Name.x, -Generation.x, 
                                                                     -Wins.x, -Total_Battles.x, -Name.y, -Generation.y, -Wins.y, -Total_Battles.y) %>% 
  mutate(Winner_First = ifelse(Winner == First_pokemon, 1, 0))
# First Pokemon Model
mean(fight$First_pokemon == fight$Winner)


# Testing what percentage to Partition
partition <- seq(from=.30, to =.90, by=.01)
# Calculate accuracy for each value
predict<- sapply(partition, function(p){
  test_index<- createDataPartition(fight$Winner_First, times =1, p=p, list= FALSE)
  test_set <- fight[test_index,]
  train_set <- fight[-test_index,]
  fit <- glm(Winner_First ~ HP.x+Attack.x+Defense.x+`Sp. Atk.x`+`Sp. Def.x`+
               Speed.x+HP.y+Attack.y+Defense.y+`Sp. Atk.y`+`Sp. Def.y`+ Speed.y,
             data = train_set)
  glm_preds<-round(predict(fit, test_set))
  mean(glm_preds==test_set$Winner_First)
})

# Plotting accuracy over percentage
plot(partition, predict)
partition[which.max(predict)]
max(predict)

# Partitioning Train and Test Set
set.seed(1, sample.kind = "Rounding")
test_index<-createDataPartition(fight$Winner, times = 1, p = 0.3, list = FALSE)
test_set<- fight[test_index,]
train_set<-fight[-test_index,]
# GLM 
fit <- glm(Winner_First ~ HP.x+Attack.x+Defense.x+`Sp. Atk.x`+`Sp. Def.x`+
             Speed.x+HP.y+Attack.y+Defense.y+`Sp. Atk.y`+`Sp. Def.y`+ Speed.y,
           data = train_set)
glm_preds<-round(predict(fit, test_set))
mean(glm_preds==test_set$Winner_First)


# GLM speed only
speed_fit <- glm(Winner_First ~ Speed.x+Speed.y,
                 data = train_set)
glm_speed_pred<-round(predict(speed_fit, test_set))
mean(glm_speed_pred==test_set$Winner_First)

# Add difference of speed
fight <-fight %>% mutate(diff_speed = Speed.x-Speed.y)
# Partitioning Train and Test Set
set.seed(1, sample.kind = "Rounding")
test_index<-createDataPartition(fight$Winner, times = 1, p = 0.3, list = FALSE)
test_set<- fight[test_index,]
train_set<-fight[-test_index,]

# Tree
binary_tree<-rpart(Winner_First ~ diff_speed+Speed.x + Speed.y, data= train_set, method = 'class')
rpart.plot(binary_tree)
predict_tree<- predict(binary_tree, test_set, type = 'class')
mean(predict_tree==test_set$Winner_First)


