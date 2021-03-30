require(dplyr)

source("../Multivariate-FlexCode/MultDimensionFlexCode.R")
source("../Multivariate-FlexCode/ExemplosFlexCodeMulti.R")
source("../Multivariate-FlexCode/CopulaFunctions.R")
require(FlexCoDE)

source("preProcessFunctions.R")


# Load Data

df = PreProcess_ecommerce(
  df_products = read.csv("ecommerce_dados/olist_products_dataset.csv")
  ,df_orders_items = read.csv("ecommerce_dados/olist_order_items_dataset.csv")
  ,df_orders = read.csv("ecommerce_dados/olist_orders_dataset.csv")
  ,df_sellers = read.csv("ecommerce_dados/olist_sellers_dataset.csv")
  ,df_custumer =  read.csv("ecommerce_dados/olist_customers_dataset.csv")
  ,df_geo = read.csv("ecommerce_dados/olist_geolocation_dataset.csv")
)

# Pre Process

df = df[,c((ncol(df)-1):ncol(df) , 1:(ncol(df)-2) )]
df0 = df
#df0 = df[df$seller_state == 'SP', names(df) != "seller_state"]

df0$seller_city = as.character(df0$seller_city)
df0$product_category_name = as.character(df0$product_category_name)

df0 = fastDummies::dummy_cols(df0, remove_first_dummy = TRUE)
df0 = df0[,!names(df0) %in% c("seller_city", "seller_state", "product_category_name")]

data.split <- splitData(df0, n.Test = 2)

# Ajustar Modelo



time = system.time({
  temp_model = MultDimensionFlexCoDE(xTrain = data.split$xTrain,                     
                                     zTrain = data.split$zTrain,                     
                                     xValidation = data.split$xValidation,               
                                     zValidation = data.split$zValidation,                
                                     regressionFunction = regressionFunction.XGBoost,
                                     copulaFunction = 'gumbel')
})

temp_pred <- predict(model = temp_model, newX = data.split$xTest)


