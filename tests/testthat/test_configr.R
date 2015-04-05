get.config <-  configrr()
expect_that(get.config,is_a('function'))

context('basic functions')

test_that('Reading a file works okay',{
    config <- get.config('../files/test.config.json')

    expect_that(config,is_a('list'))

    expect_that(config$couchdb,is_a('list'))
    expect_that(config$couchdb$host,equals('192.168.0.1'))
    expect_that(config$couchdb$port,equals(5984))
    expect_that(config$couchdb$trackingdb,equals('a_test_state_db'))
    expect_that(config$couchdb$auth,is_a('list'))

    expect_that(config$couchdb$auth$username,equals('blabbity'))
    expect_that(config$couchdb$auth$password,equals('correct horse battery staple'))

    expect_that(config$postgresql,is_a('list'))
    expect_that(config$postgresql$host,equals('192.168.0.1'))
    expect_that(config$postgresql$port,equals(5432))
    expect_that(config$postgresql$grid_merge_sqlquery_db,equals('spatialspaces'))
    expect_that(config$postgresql$auth,is_a('list'))

    expect_that(config$postgresql$auth$username,equals('sqlrulez'))
    expect_that(config$postgresql$auth$password,is_null())

})

test_that('Calling without a filename works okay',{
    config <- get.config('../files/test.config.json')
    ## second time don't need the filename
    config2 <- get.config()
    expect_that(config2,equals(config))
})

context('read more than one file')

test_that('calling a second file works okay',{

    config_zero <- get.config()
    config <- get.config('../files/test.config.json')
    expect_that(config_zero,equals(config))

    config3 <- get.config('../files/another.config.json')

    expect_that(config3,is_a('list'))

    expect_that(config3$couchdb,is_a('list'))
    expect_that(config3$couchdb$host,equals('127.0.0.1'))
    expect_that(config3$couchdb$port,equals(5984))
    expect_that(config3$couchdb$clonedb,equals('a_test_state_db'))
    expect_that(config3$couchdb$auth,is_a('list'))
    expect_that(config3$couchdb$dbroot,equals('backups'))

    expect_that(config3$couchdb$auth$username,equals('blabbity'))
    expect_that(config3$couchdb$auth$password,equals('correct horse battery staple'))

    expect_that(config3$postgresql,is_a('list'))
    expect_that(config3$postgresql$host,equals('192.168.0.1'))
    expect_that(config3$postgresql$port,equals(5433))
    expect_that(config3$postgresql$OSM_db,equals('spatialspaces'))
    expect_that(config3$postgresql$traffic_db,equals('spatialspaces'))

    expect_that(config3$postgresql$auth$username,equals('sqlrulez'))
    expect_that(config3$postgresql$auth$password,is_null())

})
