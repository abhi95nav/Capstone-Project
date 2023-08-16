Capstone Project
================
2023-07-18

## **Loading all the necessary packages.**

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(caret)
```

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     margin

    ## Loading required package: lattice

``` r
library(gbm)
```

    ## Loaded gbm 2.1.8.1

``` r
library(class)
library(esquisse)
library(caretEnsemble)
```

    ## 
    ## Attaching package: 'caretEnsemble'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     autoplot

``` r
library(ggplot2)
```

### **Loading train and test datasets.**

``` r
train<- read.csv("train.csv")
test <- read.csv("test.csv")
```

### **Binding train and test datasets into one dataset.**

``` r
df <- bind_rows(train, test)
```

## **To ensure the train and test have homogeneous transformations, we will be binding the both the dataset for Exploratory Data Analysis (EDA).**

### **Exploratory Data Analysis (EDA).**:

## **1.Checking all missing values in the dataset.**

## **2.Checking all categorical attributes that have significance with the target variable with help of Data Visualization.Based on the analysis**:

\## **2.a) we convert them to factors.** \## **2.b) we completely remove
them from the dataset.** \## **3. Checking for numerical or integers
attributes. If they have additional NA’s, we will use median impute to
replace the remaining NA’s.**

### **1.Checking all missing values in the dataset.**

``` r
# Calculate the count or percentage of missing values for each variable
missing_counts <- colSums(is.na(train))
missing_percentages <- colMeans(is.na(train)) * 100

# Create a train_na frame with variable names, missing counts, and missing percentages
missing_train_na <- data.frame(variable = names(missing_counts),
                           count = missing_counts,
                           percentage = missing_percentages)

missing_filter <- missing_train_na %>% filter(percentage > 0) %>% arrange(desc(count))


ggplot(missing_filter,
       aes(x = reorder(variable, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.91), 
            color = "black", 
            size = 2.8) +
  theme_minimal() +
  labs(x = "Variables", y = "Missing Percentage", title = "Missing Values by Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \##
**From above we can observe that,19 attributes have NA’s between 0.1% -
99.5%. I have removed all the attributes from the dataset.** \##
**Creating a new dataset - train_na, subsetting and removing NA
attributes that we observed above.**

``` r
train_na <- subset(df, select = -c(PoolQC, MiscFeature,  Alley, Fence, FireplaceQu, LotFrontage, GarageCond, GarageFinish, GarageQual, GarageType, GarageYrBlt, BsmtExposure, BsmtFinType2, BsmtCond, BsmtFinType1, BsmtQual, MasVnrArea, MasVnrType, Electrical))
```

### **2.Now we, concentrate on all the categorical attributes. As this dataset has multiples categorical attributes with multiples levels. We check the variability of the these attributes with respect to SalePrice. If they show variability,we change them to factors or remove them completely.**

### **Displaying all the character attributes in the dataset.**

``` r
char_columns <- sapply(train_na, function(col) is.character(col) || is.factor(col))

# Display numerical attributes
char_data <- train_na[, char_columns]
names(char_data)
```

    ##  [1] "MSZoning"      "Street"        "LotShape"      "LandContour"  
    ##  [5] "Utilities"     "LotConfig"     "LandSlope"     "Neighborhood" 
    ##  [9] "Condition1"    "Condition2"    "BldgType"      "HouseStyle"   
    ## [13] "RoofStyle"     "RoofMatl"      "Exterior1st"   "Exterior2nd"  
    ## [17] "ExterQual"     "ExterCond"     "Foundation"    "Heating"      
    ## [21] "HeatingQC"     "CentralAir"    "KitchenQual"   "Functional"   
    ## [25] "PavedDrive"    "SaleType"      "SaleCondition"

### **2.MSZoning:Identifies the general zoning classification of the sale.**

``` r
train_na$MSZoning[is.na(train_na$MSZoning)] <- "C (all)"

ggplot(train) +
 aes(x = MSZoning, fill = MSZoning, weight = SalePrice) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
## **MSZoning**: From above we can observe that this attributes has multiple levels and as per description of the data, the values don't give a clear picture of the SalePrice. Therefore omitting the attribute from the dataset.
```

### **2.Street: Type of road access to property.**

``` r
ggplot(train) +
 aes(x = Street, fill = Street, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
train_na$Street <- as.factor(train_na$Street)

## **Street**: As per description of the data, we observed if the street is grvl then the SalePrice less and if its "paved" then the SalePrice is high. Therefore converting these into factors.
```

### **2.Attributes with respect to Lot.**

``` r
### **LotShape**: General shape of property
ggplot(train) +
 aes(x = LotShape, fill = LotShape, weight = SalePrice/1000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
train_na$LotShape <- as.factor(train_na$LotShape)

## **LotShape**: As per description of the data, Regular lotshape has high SalePrice and IR3 being the "irregular" has very less SalePrice.  Therefore converting these into factors.

### **LotConfig**:Lot configuration

ggplot(train) +
 aes(x = LotConfig, fill = LotConfig, weight = SalePrice/1000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
train_na$LotConfig <- as.factor(train_na$LotConfig)

## **LotConfig**: As per description of the data,Lot configuration of the property is directly proportional. Therefore converting these into factors.
```

### **2.Attributes with respect to Land.**

``` r
### **LandContour**:Flatness of the property

ggplot(train) +
 aes(x = LandContour, fill = LandContour, weight = SalePrice/1000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
train_na$LandContour<- as.factor(train_na$LandContour)

## **LandContour**:As per data, we observed that Flatness of the property is directly proportional to the SalePrice of the House. Therefore converting these into factors.


### **LandSlope**: Slope of property

ggplot(train) +
 aes(x = LandSlope, fill = LandSlope, weight = SalePrice/1000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
train_na$LandSlope<- as.factor(train_na$LandSlope)

## **LandSlope**: As per data, we observed that Slope of property is inversely proportional to SalePrice of House. Therefore converting these into factors.
```

### **2.Utility**:Type of utilities available.

``` r
train_na$Utilities[is.na(train_na$Utilities)] <- "NoSeWa"

ggplot(train) +
 aes(x = Utilities, fill = Utilities, weight = SalePrice) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
## **Utility**: Even though this attribute has 2 levels, "NoSeWa" has significant impact on the SalePrice. Therefore omitting this attribute from the dataset.
```

### **2.Neighborhood**: Physical locations within Ames city limits.

``` r
ggplot(train) +
 aes(x = Neighborhood, fill = Neighborhood, weight = SalePrice/1000) +
 geom_bar() +
 scale_fill_manual(values = c(Blmngtn = "#F8766D", 
Blueste = "#EB7F48", BrDale = "#DF8824", BrkSide = "#D39200", ClearCr = "#BD9A00", CollgCr = "#A8A200", 
Crawfor = "#93AA00", Edwards = "#62AF12", Gilbert = "#31B425", IDOTRR = "#00BA38", MeadowV = "#00BC5A", 
Mitchel = "#00BE7C", NAmes = "#00C19F", NoRidge = "#00BEB5", NPkVill = "#00BBCC", NridgHt = "#00B9E3", 
NWAmes = "#20AFEC", OldTown = "#40A5F5", Sawyer = "#619CFF", SawyerW = "#898EFD", Somerst = "#B280FC", 
StoneBr = "#DB72FB", SWISU = "#E66CE8", Timber = "#F366D5", Veenker = "#FF61C3")) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
## **Neighborhood**: As we can observe, this attribute has multiple levels with no particular variablity for SalePrice. Therefore removing this attribute.
```

### **2.Condition1** & **Condition2**:Proximity to various conditions.

``` r
ggplot(train) +
 aes(x = Condition1, fill = Condition1, weight = SalePrice/1000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot(train) +
 aes(x = Condition2, fill = Condition2, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
## **Condition1** & **Condition2**: As per below, we can that both condition1 & condition2 have uniformity in the data with only one level having most impact. Therefore omitting the attributes from the dataset.
```

### **2.Attributes with respect to dwelling.**

``` r
### **BldgType**: Type of dwelling
ggplot(train) +
 aes(x = BldgType, fill = BldgType, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
### **HouseStyle**: Style of dwelling
ggplot(train) +
 aes(x = HouseStyle, fill = HouseStyle, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_minimal()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
## **BldgType** & **HouseStyle** : Both the attributes have multiple levels with no uniformity in the dataset. Therefore removing both the attributes.
```

### **2.Attributes with respect to Roof.**

``` r
## **RoofStyle**: Type of Roof
ggplot(train) +
 aes(x = RoofStyle, fill = RoofStyle, weight = SalePrice) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
## **RoofMatl**: Roof material 
ggplot(train) +
 aes(x = RoofMatl, fill = RoofMatl, weight = SalePrice) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
## **RoofStyle** & **RoofMatl** : Both the attributes have multiple levels with no uniformity in the dataset. Therefore removing both the attributes.
```

### **2.Attributes with respect to Exterior.**

``` r
## **Exterior1st**: Exterior covering on house
ggplot(train) +
 aes(x = Exterior1st, fill = Exterior1st, weight = SalePrice) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
## **Exterior2nd**: Exterior covering on house (if more than one material)
ggplot(train) +
 aes(x = Exterior2nd, fill = Exterior2nd, weight = SalePrice) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
## **Exterior1st** & **Exterior2nd** : Both the attributes have multiple levels with no uniformity in the dataset. Therefore removing both the attributes.
```

## **2.Attributes with respect to Exterior quality & Condition.**

``` r
## **ExterQual**: Evaluates the quality of the material on the exterior 
ggplot(train) +
 aes(x = ExterQual, fill = ExterQual, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
## **ExterCond**: Evaluates the present condition of the material on the exterior
ggplot(train) +
 aes(x = ExterCond, fill = ExterCond, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
## **ExterQual* & ## **ExterCond** : Both the attributes don't show any significant uniformity to SalePrice. Therefore removing these attributes.
```

### **2.Foundation**: Type of foundation.

``` r
ggplot(train) +
 aes(x = Foundation, fill = Foundation, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
## **Foundation**: This attribute have multiple levels with no uniformity in the dataset. Therefore removing the attributes.
```

### **2.Attributes with respect to Heating.**

``` r
## **Heating**: Type of heating

ggplot(train) +
 aes(x = Heating, fill = Heating, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
##Heating : This attribute have multiple levels with no uniformity in the dataset. Therefore removing the attributes.

## **HeatingQC** : Heating quality and condition

ggplot(train) +
 aes(x = HeatingQC, fill = HeatingQC, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
train_na$HeatingQC<- as.factor(train_na$HeatingQC)

## **HeatingQC** : The quality of heating is directly proportional to the SalePrice of the House. Therefore converting these into factors.
```

### **2.CentralAir**: Central air conditioning.

``` r
ggplot(train) +
 aes(x = CentralAir, fill = CentralAir, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
train_na$CentralAir <- as.factor(train_na$CentralAir)

## **CentralAir** : The availability of Central air is directly proportional to the SalePrice of the House. Therefore converting these into factors.
```

### **2.KitchenQual**: Kitchen quality.

``` r
train_na$KitchenQual[is.na(train_na$KitchenQual)] <- "Fa"

ggplot(train) +
 aes(x = KitchenQual, fill = KitchenQual, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
## **KitchenQual**: The attributes don't show any significant uniformity to SalePrice. Therefore removing the attributes.
```

### **2.Functional**: Home functionality (Assume typical unless deductions are warranted).

``` r
train_na$Functional[is.na(train_na$Functional)] <- "Sev"

ggplot(train) +
 aes(x = Functional, fill = Functional, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_minimal()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
## **Functional**: The attributes don't show any significant uniformity to SalePrice. Therefore removing the attribute.
```

### **2.PavedDrive**: Paved driveway.

``` r
ggplot(train) +
 aes(x = PavedDrive, fill = PavedDrive, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 theme_minimal()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
train_na$PavedDrive <- as.factor(train_na$PavedDrive)

## **PavedDrive**: The availability of PavedDrive is directly proportional to the SalePrice of the House. Therefore converting these into factors.
```

### **2.SaleType**: Type of sale.

``` r
train_na$SaleType[is.na(train_na$SaleType)] <- "Oth"

ggplot(train) +
 aes(x = SaleType, fill = SaleType, weight = SalePrice) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
## **SaleType**: The attributes don't show any significant uniformity to SalePrice. Therefore removing the attribute.
```

### **2.SaleCondition**: Condition of sale.

``` r
ggplot(train_na) +
 aes(x = SaleCondition, fill = SaleCondition, weight = SalePrice/10000) +
 geom_bar() +
 scale_fill_brewer(palette = "YlOrRd", direction = 1) +
 theme_minimal()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
## **SaleCondition**:The attributes don't show any significant uniformity to SalePrice. Therefore removing the attribute.
```

### **3. Checking for numerical or integers attributes. If they have additional NA’s, we will use median impute to replace the remaining NA’s.**

### **3.Attributes with respect to Basement.**

``` r
train_na$BsmtFinSF1[is.na(train_na$BsmtFinSF1)] <- median(na.omit(train_na$BsmtFinSF1))
train_na$BsmtFinSF2[is.na(train_na$BsmtFinSF2)] <-median(na.omit(train_na$BsmtFinSF2))
train_na$BsmtUnfSF[is.na(train_na$BsmtUnfSF)] <- median(na.omit(train_na$BsmtUnfSF))
train_na$TotalBsmtSF[is.na(train_na$TotalBsmtSF)] <-median(na.omit(train_na$TotalBsmtSF))
```

### **3.Attributes with respect to Basement bath.**

``` r
train_na$BsmtFullBath[is.na(train_na$BsmtFullBath)] <- median(na.omit(train_na$BsmtFullBath))
train_na$BsmtHalfBath[is.na(train_na$BsmtHalfBath)] <-median(na.omit(train_na$BsmtHalfBath))
```

## **3.Attributes with respect to Garage.**

``` r
train_na$GarageCars[is.na(train_na$GarageCars)] <- median(na.omit(train_na$GarageCars))
train_na$GarageArea[is.na(train_na$GarageArea)] <-median(na.omit(train_na$GarageArea))
```

### **Creating new dataset - “train_na_1” by removing the categorical variables that have no significance.**

``` r
train_na_1<- subset(train_na, select = -c(MSZoning, Utilities, Neighborhood, Condition1, Condition2, BldgType, HouseStyle, RoofStyle, RoofMatl, Exterior1st, Exterior2nd, ExterQual, ExterCond, Foundation, Heating, KitchenQual, Functional, SaleType, SaleCondition))
```

## **Since EDA and data transformations have been completed, we split the train_na_1 into train and test datasets for model and evaluation.**

``` r
set.seed(123)
train1 <- train_na_1[1:1460,]
test1 <- train_na_1[1461:nrow(df),]

test1 <- test1[-43] ## **Removing SalePrice from test dataset as it is target variable to predict.**
```

### **Displaying all the numerical and integer attributes in the dataset.**

``` r
numerical_columns <- sapply(train1, function(col) is.integer(col) || is.numeric(col))

# Display numerical attributes
numerical_data <- train1[, numerical_columns]
names(numerical_data)
```

    ##  [1] "Id"            "MSSubClass"    "LotArea"       "OverallQual"  
    ##  [5] "OverallCond"   "YearBuilt"     "YearRemodAdd"  "BsmtFinSF1"   
    ##  [9] "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "X1stFlrSF"    
    ## [13] "X2ndFlrSF"     "LowQualFinSF"  "GrLivArea"     "BsmtFullBath" 
    ## [17] "BsmtHalfBath"  "FullBath"      "HalfBath"      "BedroomAbvGr" 
    ## [21] "KitchenAbvGr"  "TotRmsAbvGrd"  "Fireplaces"    "GarageCars"   
    ## [25] "GarageArea"    "WoodDeckSF"    "OpenPorchSF"   "EnclosedPorch"
    ## [29] "X3SsnPorch"    "ScreenPorch"   "PoolArea"      "MiscVal"      
    ## [33] "MoSold"        "YrSold"        "SalePrice"

## **Checking for outliers using boxplot in “SalePrice”.**

``` r
ggplot(train_na_1[1:1460,]) +
  aes(x = "", y = SalePrice/1000) +
  geom_boxplot(fill = "#EBB16B") +
  theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->
\## **Removing all the outliers using boxplot.stats with respect to
“SalePrice”.**

``` r
set.seed(123)
a<-boxplot.stats(train1$SalePrice)
outliers <- a$out
train1 <- train1[!train1$SalePrice %in% outliers, ]

b<-boxplot.stats(train1$SalePrice)
outliers_1 <- b$out
train1 <- train1[!train1$SalePrice %in% outliers_1, ]

c<-boxplot.stats(train1$SalePrice)
outliers_2 <- c$out
train1 <- train1[!train1$SalePrice %in% outliers_2, ]

d<-boxplot.stats(train1$SalePrice)
outliers_3 <- d$out
train1 <- train1[!train1$SalePrice %in% outliers_3, ]

e <- boxplot.stats(train1$SalePrice)
outliers_4 <- e$out
train1 <- train1[!train1$SalePrice %in% outliers_4, ]
```

## **Checking for boxplot after removing outliers from “SalePrice”.**

``` r
ggplot(train1) +
  aes(x = "", y = SalePrice/1000) +
  geom_boxplot(fill = "#EBB16B") +
  theme_classic()
```

![](Capstone-Project_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

\###**5.Modeling & Evaluation**: \## **Creating dummy variables for
categorical attributes using mlr package in train and test dataset.**

``` r
set.seed(9596)
train2 <- mlr::createDummyFeatures(train1[-1]) #Removing Id column from the train dataset.
test2 <- mlr::createDummyFeatures(test1[-1]) #Removing Id column from the test dataset.
```

## **Creating data partition by splitting train into train and validation datasets.**

``` r
set.seed(123)
train_index <- createDataPartition(train2$SalePrice, p = 0.60, list = FALSE)
train2_train <- train2[train_index, ]
train2_validate <- train2[-train_index, ]
```

### **Below I applied 6 machine learning methods to **:

**gbm: Gradient Boosting Machine** **glm: Generalized Linear Model**
**lasso: Lasso Regression** **lm: Linear Regression** **rf: Random
Forest** **xgbLinear: XGBoost Linear**

``` r
models <- c("gbm","glm","lasso","lm","rf", "xgbLinear") 

set.seed(9596)


fits <- suppressWarnings({lapply(models, function(model){train(SalePrice~., data = train2_train, method = model, preProc = c("YeoJohnson","center", "scale", "nzv"), trControl = trainControl(method = "repeatedcv",number = 5))})})
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2475537248.2529             nan     0.1000 234127690.8743
    ##      2 2267486112.5953             nan     0.1000 195454605.3232
    ##      3 2114047555.3272             nan     0.1000 150112178.2392
    ##      4 1978007973.1520             nan     0.1000 137191413.0964
    ##      5 1855961633.9906             nan     0.1000 104051053.7309
    ##      6 1751511403.0698             nan     0.1000 101897049.2098
    ##      7 1674374787.1386             nan     0.1000 75959061.4158
    ##      8 1591571287.8468             nan     0.1000 83291321.5117
    ##      9 1507700652.2458             nan     0.1000 73271949.2464
    ##     10 1440759201.8346             nan     0.1000 61713741.4867
    ##     20 1000156487.2142             nan     0.1000 27695870.4940
    ##     40 644927512.7679             nan     0.1000 9208947.7527
    ##     60 510539058.2459             nan     0.1000 -732353.4550
    ##     80 440303805.9210             nan     0.1000 1187260.3487
    ##    100 399780638.1711             nan     0.1000 -1946971.5940
    ##    120 375410963.1662             nan     0.1000 -72501.4396
    ##    140 360485726.8060             nan     0.1000 -2119063.4156
    ##    150 352955887.6389             nan     0.1000 -706635.3164
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2429833817.8880             nan     0.1000 261374527.4739
    ##      2 2199912379.2878             nan     0.1000 219159784.9812
    ##      3 2002486574.6482             nan     0.1000 190688777.9663
    ##      4 1819483522.8095             nan     0.1000 173518435.1987
    ##      5 1680773897.1743             nan     0.1000 132084507.9066
    ##      6 1570612157.3383             nan     0.1000 120122901.1236
    ##      7 1471537923.0993             nan     0.1000 84567560.0222
    ##      8 1374521554.8374             nan     0.1000 77145333.1432
    ##      9 1291899554.2798             nan     0.1000 70146604.1310
    ##     10 1219790455.3084             nan     0.1000 62919305.1619
    ##     20 778328615.5070             nan     0.1000 23689585.5640
    ##     40 474622656.8907             nan     0.1000 4395560.3448
    ##     60 378771569.4606             nan     0.1000 -372686.7343
    ##     80 332397312.4789             nan     0.1000 -1042049.6964
    ##    100 307197042.5005             nan     0.1000 -3384420.7547
    ##    120 284917252.5316             nan     0.1000 -853021.3707
    ##    140 268508989.2038             nan     0.1000 -1346157.3895
    ##    150 263293822.8148             nan     0.1000 -950247.1061
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2419671958.9587             nan     0.1000 276117896.0066
    ##      2 2169700552.0257             nan     0.1000 249161609.7732
    ##      3 1953994422.3262             nan     0.1000 205007724.1233
    ##      4 1768918581.1176             nan     0.1000 169920166.1767
    ##      5 1609290463.5323             nan     0.1000 144012831.2703
    ##      6 1469529497.4634             nan     0.1000 138326441.7096
    ##      7 1358317561.3561             nan     0.1000 101279019.9208
    ##      8 1250514406.3471             nan     0.1000 108176441.1000
    ##      9 1163865254.7269             nan     0.1000 90654186.6259
    ##     10 1074991885.9370             nan     0.1000 69068204.3028
    ##     20 643854423.9173             nan     0.1000 19192800.1444
    ##     40 383569008.0922             nan     0.1000 2133661.1673
    ##     60 310594722.5110             nan     0.1000 -3374928.3970
    ##     80 274718002.3115             nan     0.1000 -840210.5746
    ##    100 251601611.3205             nan     0.1000 -1741477.8424
    ##    120 232316532.2151             nan     0.1000 -2183395.4647
    ##    140 215060662.9587             nan     0.1000 -812016.3813
    ##    150 205941673.3654             nan     0.1000 -520337.7064
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2507505275.6705             nan     0.1000 250924348.7711
    ##      2 2295485770.6826             nan     0.1000 205538114.5873
    ##      3 2122827690.6359             nan     0.1000 166079312.7157
    ##      4 1965614922.6198             nan     0.1000 151592106.4781
    ##      5 1854447235.5014             nan     0.1000 105206896.6255
    ##      6 1729477820.1043             nan     0.1000 113343099.2791
    ##      7 1620331606.5435             nan     0.1000 100980846.4689
    ##      8 1533563001.2259             nan     0.1000 78413128.0851
    ##      9 1445088676.1326             nan     0.1000 81617550.5288
    ##     10 1377758188.1062             nan     0.1000 65386958.8465
    ##     20 912675694.6164             nan     0.1000 23618623.1983
    ##     40 583248625.3399             nan     0.1000 2985116.2940
    ##     60 452926396.3282             nan     0.1000 520181.9109
    ##     80 385815662.3980             nan     0.1000 1455644.2226
    ##    100 346924416.3550             nan     0.1000 -375210.0304
    ##    120 321542368.0355             nan     0.1000 904044.1820
    ##    140 306701356.1686             nan     0.1000 372794.1742
    ##    150 300586338.2466             nan     0.1000 -708397.1020
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2458539267.5067             nan     0.1000 299744717.0605
    ##      2 2204004787.6072             nan     0.1000 262058319.6530
    ##      3 1988965948.3083             nan     0.1000 203593457.1761
    ##      4 1809840995.0729             nan     0.1000 159450577.3266
    ##      5 1653476006.7435             nan     0.1000 139476874.6738
    ##      6 1521054704.8636             nan     0.1000 118894281.0558
    ##      7 1425900039.5512             nan     0.1000 86178713.0785
    ##      8 1322998288.3438             nan     0.1000 98028168.1237
    ##      9 1237861809.3327             nan     0.1000 74763808.0766
    ##     10 1167160989.1727             nan     0.1000 64672313.5637
    ##     20 708125956.9340             nan     0.1000 24642369.9227
    ##     40 412364068.3244             nan     0.1000 5941572.3812
    ##     60 320843731.4844             nan     0.1000 2466886.2921
    ##     80 280630167.5602             nan     0.1000 598534.0354
    ##    100 260419192.5869             nan     0.1000 -2128286.6678
    ##    120 242984250.3187             nan     0.1000 -140807.8899
    ##    140 232046572.8767             nan     0.1000 -1875218.1693
    ##    150 224199122.2692             nan     0.1000 -743871.3775
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2442406175.5148             nan     0.1000 313721339.9904
    ##      2 2153550484.8509             nan     0.1000 250828641.3089
    ##      3 1925029529.7381             nan     0.1000 210641227.1664
    ##      4 1728397921.0594             nan     0.1000 194458972.8775
    ##      5 1546998178.9845             nan     0.1000 182242742.6382
    ##      6 1417812547.4414             nan     0.1000 140255017.8144
    ##      7 1289651170.1891             nan     0.1000 106072081.3790
    ##      8 1185211779.7617             nan     0.1000 92428711.8280
    ##      9 1085985061.7428             nan     0.1000 82920858.1745
    ##     10 1014030024.1389             nan     0.1000 75912762.3045
    ##     20 577560267.5043             nan     0.1000 19204123.7070
    ##     40 329267503.8453             nan     0.1000 2908366.8864
    ##     60 258574739.5179             nan     0.1000 496666.8540
    ##     80 227196241.7779             nan     0.1000 -1025229.3470
    ##    100 207591378.1981             nan     0.1000 -538571.9369
    ##    120 193026134.1929             nan     0.1000 -2128117.6633
    ##    140 178524934.1253             nan     0.1000 -1262967.4562
    ##    150 172967684.7024             nan     0.1000 -944445.9763
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2423062433.7621             nan     0.1000 212042396.6375
    ##      2 2258605546.2707             nan     0.1000 162917318.2896
    ##      3 2100957242.9473             nan     0.1000 160417848.4658
    ##      4 1954604359.0192             nan     0.1000 127868894.5252
    ##      5 1831649022.4151             nan     0.1000 115228609.6236
    ##      6 1726730838.8714             nan     0.1000 101341220.7206
    ##      7 1642384338.7542             nan     0.1000 72085989.3104
    ##      8 1555067214.3009             nan     0.1000 90186163.5004
    ##      9 1480845754.0638             nan     0.1000 69747397.4699
    ##     10 1419601874.7028             nan     0.1000 52801546.7159
    ##     20 960353554.2142             nan     0.1000 26958517.9374
    ##     40 627379052.9039             nan     0.1000 6053080.6982
    ##     60 485842102.2563             nan     0.1000 1593331.3138
    ##     80 425329944.9659             nan     0.1000 2065138.2194
    ##    100 390360776.2450             nan     0.1000 -2419653.5758
    ##    120 372338730.5451             nan     0.1000 -1744369.2109
    ##    140 355988700.6608             nan     0.1000 -364054.1832
    ##    150 350819635.7220             nan     0.1000 -1983096.5673
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2371474883.5635             nan     0.1000 264250052.2144
    ##      2 2150830036.3279             nan     0.1000 201947366.0296
    ##      3 1976957245.7560             nan     0.1000 143160094.2714
    ##      4 1813668516.2277             nan     0.1000 154992367.3686
    ##      5 1675382626.5827             nan     0.1000 132313727.6934
    ##      6 1551796625.3039             nan     0.1000 127068907.9489
    ##      7 1446591980.5856             nan     0.1000 88159837.4826
    ##      8 1339698141.5131             nan     0.1000 95379791.0301
    ##      9 1260476922.0694             nan     0.1000 72077628.6718
    ##     10 1191969492.7391             nan     0.1000 71449677.8861
    ##     20 750700948.8232             nan     0.1000 27093836.6474
    ##     40 449668848.2865             nan     0.1000 4908328.3669
    ##     60 360533672.9382             nan     0.1000 479349.2451
    ##     80 320052126.5834             nan     0.1000 -2354019.2701
    ##    100 298543237.0386             nan     0.1000 -1337447.1744
    ##    120 278702339.2854             nan     0.1000 -1404665.4218
    ##    140 260191009.3691             nan     0.1000 -1323118.2407
    ##    150 252621002.1914             nan     0.1000 -582588.0327
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2368765928.0141             nan     0.1000 305850288.8872
    ##      2 2125881605.3046             nan     0.1000 243946346.0007
    ##      3 1902244907.2244             nan     0.1000 203872419.6527
    ##      4 1716695287.6742             nan     0.1000 168239029.8414
    ##      5 1575975925.4726             nan     0.1000 138864595.9858
    ##      6 1444153606.6496             nan     0.1000 131020657.1464
    ##      7 1334200697.4549             nan     0.1000 101689823.8653
    ##      8 1238799501.1821             nan     0.1000 88541856.7630
    ##      9 1146338555.7654             nan     0.1000 90127131.9327
    ##     10 1068239266.4416             nan     0.1000 76381302.9007
    ##     20 618201683.3248             nan     0.1000 15218650.5267
    ##     40 370690918.4011             nan     0.1000 2681980.4181
    ##     60 301803927.8875             nan     0.1000 -1049805.2584
    ##     80 265445230.5963             nan     0.1000 -2252462.3226
    ##    100 242010282.2166             nan     0.1000 -2277572.8609
    ##    120 218494371.8101             nan     0.1000 -1058783.9218
    ##    140 205867479.0349             nan     0.1000 -1851113.7239
    ##    150 197706483.9585             nan     0.1000 -1560724.0495
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2452712534.2205             nan     0.1000 215219952.3617
    ##      2 2282482032.8294             nan     0.1000 165756699.0219
    ##      3 2115092233.3837             nan     0.1000 153284355.8697
    ##      4 1978976046.4144             nan     0.1000 127332153.6484
    ##      5 1883249087.8826             nan     0.1000 95368172.1226
    ##      6 1767995839.4439             nan     0.1000 106014014.1371
    ##      7 1668504918.5448             nan     0.1000 107611029.2585
    ##      8 1590367087.8583             nan     0.1000 75090600.8644
    ##      9 1508458085.8957             nan     0.1000 78077506.1475
    ##     10 1443898148.0836             nan     0.1000 63480801.6894
    ##     20 979250200.9118             nan     0.1000 26330444.7353
    ##     40 640996622.2801             nan     0.1000 6616388.3421
    ##     60 505250419.7391             nan     0.1000 3732520.3781
    ##     80 436877423.0874             nan     0.1000 1076104.1642
    ##    100 392024998.4183             nan     0.1000 -613059.4561
    ##    120 368552892.4088             nan     0.1000 -2626840.5640
    ##    140 352104657.3770             nan     0.1000 -3407596.0533
    ##    150 345564917.7922             nan     0.1000 -995160.8271
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2401725163.2644             nan     0.1000 263074053.2746
    ##      2 2192816603.5075             nan     0.1000 210704830.0904
    ##      3 1993324900.8992             nan     0.1000 203077383.6833
    ##      4 1839394116.1450             nan     0.1000 152966755.4586
    ##      5 1694633967.1935             nan     0.1000 118749934.2340
    ##      6 1578227570.3609             nan     0.1000 115706949.8848
    ##      7 1473463554.1694             nan     0.1000 96771537.0647
    ##      8 1379102652.9698             nan     0.1000 79384872.0531
    ##      9 1281617064.0732             nan     0.1000 79607087.4069
    ##     10 1198486410.4284             nan     0.1000 68637784.7665
    ##     20 748586794.3273             nan     0.1000 30225947.1249
    ##     40 454615339.4674             nan     0.1000 4553068.7550
    ##     60 361784115.6768             nan     0.1000 -100325.4489
    ##     80 317332069.6722             nan     0.1000 -548967.6236
    ##    100 289638317.1632             nan     0.1000 -1466078.3434
    ##    120 270235538.6445             nan     0.1000 -2558675.7036
    ##    140 256129986.2472             nan     0.1000 -1657430.0796
    ##    150 248931031.8035             nan     0.1000 -1239035.2427
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2370536178.2739             nan     0.1000 274683188.2750
    ##      2 2117620582.3631             nan     0.1000 252996625.8292
    ##      3 1900249280.3953             nan     0.1000 190419642.2218
    ##      4 1707780948.8112             nan     0.1000 179988745.7351
    ##      5 1563528258.2712             nan     0.1000 144370328.8265
    ##      6 1434601175.1807             nan     0.1000 119162790.3821
    ##      7 1310396520.1699             nan     0.1000 102338009.2639
    ##      8 1212517850.6456             nan     0.1000 89351189.9405
    ##      9 1124238374.3268             nan     0.1000 67999086.2958
    ##     10 1054334236.9703             nan     0.1000 63418520.2658
    ##     20 621513361.1266             nan     0.1000 20696706.3099
    ##     40 367978155.2939             nan     0.1000 2809329.1866
    ##     60 296559391.8175             nan     0.1000 -128825.9440
    ##     80 256005434.0860             nan     0.1000 -1090136.9622
    ##    100 233250190.9383             nan     0.1000 -261722.7988
    ##    120 215490530.1192             nan     0.1000 -1504569.9997
    ##    140 198575740.0282             nan     0.1000 -508285.7828
    ##    150 191885121.5805             nan     0.1000 -907910.4096
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2531624772.9498             nan     0.1000 222701439.0314
    ##      2 2338712516.5162             nan     0.1000 191948574.1563
    ##      3 2169482993.6277             nan     0.1000 163642088.9070
    ##      4 2025088080.4915             nan     0.1000 136205022.6768
    ##      5 1903792902.8219             nan     0.1000 118809248.3856
    ##      6 1798461014.7080             nan     0.1000 94952624.8840
    ##      7 1702522486.1223             nan     0.1000 92247606.5379
    ##      8 1610066305.7900             nan     0.1000 87252607.8523
    ##      9 1532136436.5577             nan     0.1000 73311383.9715
    ##     10 1463821380.2660             nan     0.1000 48379263.7700
    ##     20 1013310684.5212             nan     0.1000 22575590.2492
    ##     40 658387847.6518             nan     0.1000 5219508.2650
    ##     60 512866550.6896             nan     0.1000 4253458.8361
    ##     80 441511173.2235             nan     0.1000 -1985451.4906
    ##    100 402868144.5583             nan     0.1000 660657.6032
    ##    120 384179612.7764             nan     0.1000 -1857817.4199
    ##    140 368836918.8684             nan     0.1000 -957568.3560
    ##    150 363560145.3809             nan     0.1000 -2467822.4425
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2459148971.2233             nan     0.1000 269582023.1548
    ##      2 2228117145.7321             nan     0.1000 202718286.9379
    ##      3 2025178971.4029             nan     0.1000 195307900.9162
    ##      4 1867675647.8736             nan     0.1000 153675695.0862
    ##      5 1723145679.7994             nan     0.1000 139266303.0709
    ##      6 1602655889.4614             nan     0.1000 107247948.2685
    ##      7 1497774239.4708             nan     0.1000 95461767.7106
    ##      8 1402033211.8059             nan     0.1000 88899250.9221
    ##      9 1313044437.2280             nan     0.1000 70759876.3724
    ##     10 1229342869.4553             nan     0.1000 85098845.3309
    ##     20 774919892.2591             nan     0.1000 20024965.0591
    ##     40 467035852.4624             nan     0.1000 3587426.5262
    ##     60 379439141.4480             nan     0.1000 108198.4071
    ##     80 336224648.1449             nan     0.1000 -1041238.6288
    ##    100 307935424.0375             nan     0.1000 -1510851.0500
    ##    120 286905751.1896             nan     0.1000 -261278.4099
    ##    140 268397480.4057             nan     0.1000 -602304.7311
    ##    150 261186915.1337             nan     0.1000 -2075184.6594
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2435352221.4777             nan     0.1000 299139892.4085
    ##      2 2178166841.5337             nan     0.1000 217102509.7568
    ##      3 1962314211.1632             nan     0.1000 179853752.7161
    ##      4 1759491377.0719             nan     0.1000 198771954.8981
    ##      5 1604608377.1765             nan     0.1000 143565615.4178
    ##      6 1477060732.7010             nan     0.1000 107980926.7610
    ##      7 1354826645.4569             nan     0.1000 106288844.5641
    ##      8 1255330267.2491             nan     0.1000 86577895.0711
    ##      9 1154276444.0563             nan     0.1000 90220576.5342
    ##     10 1074309880.2751             nan     0.1000 67893406.3397
    ##     20 619497101.5925             nan     0.1000 18588781.2464
    ##     40 389613359.6515             nan     0.1000 957193.9456
    ##     60 311979354.2957             nan     0.1000 -1372337.7287
    ##     80 272528420.2929             nan     0.1000 -992737.5583
    ##    100 244386946.9203             nan     0.1000 -533215.3924
    ##    120 222943639.5231             nan     0.1000 -1960370.6371
    ##    140 204752421.3000             nan     0.1000 -1229894.2979
    ##    150 197460645.7553             nan     0.1000 -1854669.2405
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 2410733299.9306             nan     0.1000 283775016.9052
    ##      2 2171439467.3493             nan     0.1000 238579796.6586
    ##      3 1969447263.0534             nan     0.1000 199475397.3487
    ##      4 1787216771.7633             nan     0.1000 174836784.6954
    ##      5 1627662561.4064             nan     0.1000 161466536.6233
    ##      6 1487332668.4187             nan     0.1000 122398969.2510
    ##      7 1366244815.3716             nan     0.1000 111707354.4187
    ##      8 1276816949.4618             nan     0.1000 80147698.3784
    ##      9 1175769038.2726             nan     0.1000 88926914.9479
    ##     10 1093143083.4154             nan     0.1000 75181672.2316
    ##     20 631159966.1167             nan     0.1000 18106746.2877
    ##     40 380914942.4947             nan     0.1000 2283761.0606
    ##     60 303881945.2473             nan     0.1000 1897536.0145
    ##     80 271623209.9411             nan     0.1000 -2241624.9120
    ##    100 246035477.6752             nan     0.1000 -1014100.8840
    ##    120 226610259.7928             nan     0.1000 -566573.7426
    ##    140 211381756.4412             nan     0.1000 -1541356.0018
    ##    150 204279571.4185             nan     0.1000 -1100317.6847

``` r
names(fits) <- models

results <- data.frame(gbm = max(fits[["gbm"]]$results$Rsquared), glm = max(fits[["glm"]]$results$Rsquared),lasso = max(fits[["lasso"]]$results$Rsquared) , lm = max(fits[["lm"]]$results$Rsquared), rf = max(fits[["rf"]]$results$Rsquared), xgbLinear = max(fits[["xgbLinear"]]$results$Rsquared))
results
```

    ##         gbm       glm     lasso        lm        rf xgbLinear
    ## 1 0.8411357 0.7496593 0.7615173 0.7464322 0.8236849 0.7885838

### **Selecting the best model based on R-squared value for final predictions**:

## **Created multiple hyper-parameters for the gbm model to find the best hyper-parameters to achieve best performance.**

``` r
set.seed(9596)

# Define train control with repeated cross-validation
train_control <- trainControl(method="repeatedcv", 
                              number=5)
                             
# Define parameter grid for tuning
param_grid <- expand.grid(n.trees=c(100,500,1000),  
                        shrinkage=c(0.1,0.05,0.01),
                        interaction.depth=1:5,
                        n.minobsinnode=c(10,15,20))
                          # Minimum number of observations in terminal nodes

gbm_model <- train(SalePrice~.,
                   data=train2_train,
                   prePro=c("YeoJohnson","center","scale","nzv"), 
                   method="gbm",
                   tuneGrid = param_grid,
                   trControl=train_control, verbose = FALSE)


gbm_model$bestTune
```

    ##    n.trees interaction.depth shrinkage n.minobsinnode
    ## 39    1000                 5      0.01             10

### **Combining both attributes and hyper-parameters returned from gbm_model into final_gbm_model.**

``` r
set.seed(9596)
final_train_control <- trainControl(method="repeatedcv", 
                              number=5)

final_param <- expand.grid(n.trees=1000,
                           interaction.depth=5,
                          shrinkage=0.01,
                          n.minobsinnode=10)


final_gbm_model<-suppressWarnings({train(SalePrice~.,
                   data=train2_validate,
                   prePro=c("YeoJohnson","center","scale"), 
                   method="gbm",
                   tuneGrid = final_param,
                   trControl=final_train_control,verbose = FALSE)})
  
final_gbm_model
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 535 samples
    ##  61 predictor
    ## 
    ## Pre-processing: Yeo-Johnson transformation (61), centered (61), scaled (61) 
    ## Resampling: Cross-Validated (5 fold, repeated 1 times) 
    ## Summary of sample sizes: 428, 428, 428, 428, 428 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   17990.46  0.8849166  12778.11
    ## 
    ## Tuning parameter 'n.trees' was held constant at a value of 1000
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.01
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10

## **Predicting the “SalePrice” on the test dataset and saving as CSV file.**

``` r
set.seed(123)
preds <- data.frame(Saleprice = predict(final_gbm_model, newdata = test2))

submission_file<-data.frame(Id = test1$Id, preds)

write.csv(submission_file, "submission.csv")
```

### **Selecting important attributes returned from gbm_model to improve final model performance.**

``` r
var_importance <- varImp(final_gbm_model)
var_importance
```

    ## gbm variable importance
    ## 
    ##   only 20 most important variables shown (out of 61)
    ## 
    ##               Overall
    ## OverallQual   100.000
    ## GrLivArea      89.338
    ## TotalBsmtSF    34.127
    ## YearBuilt      30.086
    ## GarageArea     23.739
    ## LotArea        15.944
    ## FullBath       13.201
    ## YearRemodAdd   12.991
    ## BsmtFinSF1     10.947
    ## Fireplaces     10.037
    ## GarageCars      7.350
    ## OverallCond     7.264
    ## X1stFlrSF       6.660
    ## X2ndFlrSF       6.328
    ## OpenPorchSF     3.759
    ## HalfBath        2.797
    ## BsmtUnfSF       2.014
    ## LandSlope.Gtl   1.965
    ## MoSold          1.636
    ## MSSubClass      1.594
