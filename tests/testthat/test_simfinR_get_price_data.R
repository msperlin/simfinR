test_that( "https://github.com/msperlin/simfinR/issues/4", {
    
    p1 = simfinR_get_price_data(
      #Apple:
      111052, 
      api_key = my_api_key
    ) %>% 
      filter(!is.na(.$close_adj))
    
    p2 = simfinR_get_price_data(
      #Amazon:
      62747, 
      api_key = my_api_key
    ) %>% 
      filter(!is.na(.$close_adj))
    
    expect_equal(p1$ref_date, p2$ref_date)

})
