if(exists('my_api_key')) test_that( "get_shares_outstanding", {
    
    # this will also run/test simfinR_get_single_share_classes
    expect_equal(
        names(simfinR_get_shares_outstanding(c(670420, 62747), my_api_key)),
        c('share_class_id', 'share_class_name', 'share_class_type', 'company_name', 'id_sim', 'type', 'ref_date', 'value')
    )

})
