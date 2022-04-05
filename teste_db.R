library(DBI)

# Connect to a specific postgres database i.e. Heroku
con <- dbConnect(RPostgres::Postgres(),dbname = 'aop', 
                 host = '134.122.58.210', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'kauebraga',
                 password = 'pikesPPP1')



dbListTables(con)
dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)

res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)

hex <- aopdata::read_landuse(city = "all")
dbListTables(con)
dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)