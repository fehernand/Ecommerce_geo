
PreProcess_ecommerce <- function(df_products,
                                 df_orders_items,
                                 df_orders,
                                 df_sellers,
                                 df_custumer,
                                 df_geo){
  
  df_order_products = merge(df_products, df_orders_items, by = "product_id")
  
  df_order_products_seller = merge(df_order_products, df_sellers[,names(df_sellers) != "seller_zip_code_prefix"], by = "seller_id")
  
  
  
  df_geo = df_geo %>%
    group_by(geolocation_zip_code_prefix) %>%
    dplyr::summarize(geolocation_lat = mean(geolocation_lat, na.rm=TRUE), geolocation_lng = mean(geolocation_lng, na.rm=TRUE))
  
  df_custumer_geo = merge(df_custumer[,c("customer_id", "customer_zip_code_prefix")],
                          df_geo[,c("geolocation_zip_code_prefix", "geolocation_lat", "geolocation_lng")],
                          by.x = "customer_zip_code_prefix",
                          by.y = "geolocation_zip_code_prefix")
  
  df_custumer_geo_order = base::merge(df_custumer_geo[,names(df_custumer_geo) != "customer_zip_code_prefix"], df_orders[,c("order_id", "customer_id")], by = "customer_id" )
  
  
  df = merge(df_order_products_seller, df_custumer_geo_order, by = "order_id")
  
  df = df[, !names(df) %in% c("order_id", "seller_id","product_id", "order_item_id", "shipping_limit_date", "customer_id")]
  
  return(df)
}