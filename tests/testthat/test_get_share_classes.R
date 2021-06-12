test_that( "get_share_classes", {
    
    # this will also run/test simfinR_get_single_share_classes
    expect_equal(
        names(simfinR_get_share_classes(c(111052, 62747), my_api_key)),
        c('share_class_id', 'share_class_name', 'share_class_type', 'company_name')
    )

})
