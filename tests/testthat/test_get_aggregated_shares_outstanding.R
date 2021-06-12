if(exists('my_api_key')) test_that( "get_aggregated_shares_outstanding", {
    
    # this will also run/test "single" version.
    expect_equal(
        names(simfinR_get_aggregated_shares_outstanding(c(111052, 62747), my_api_key)),
        c('figure', 'type', 'measure', 'date', 'period', 'fyear', 'value')
    )

    # filter.
    expect_equal(
        unique(simfinR_get_aggregated_shares_outstanding(c(111052, 62747), my_api_key, filter = 'common-outstanding')$figure),
        'common-outstanding'
    )

})
