if(exists('my_api_key')) test_that( "get_price_data_share_classes", {
    
    # this will also run/test simfinR_get_single_share_classes
    expect_equal(
        names(simfinR_get_price_by_class(c(670420, 62747), my_api_key)),
        c('share_class_id', 'share_class_name', 'share_class_type', 'company_name', 'id_sim', 'ref_date', 'close_adj', 'split_coef', 'currency')
    )

})
