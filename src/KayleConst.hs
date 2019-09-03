-- file: KayleConst.hs

module KayleConst where

-- Unit is microseconds
seconds_micro :: Integer
seconds_micro = 1000000

-- Error messages
serverAddr_err_msg :: String
serverAddr_err_msg = "Server's informations is not found in configuration file"

db_err_msg :: String
db_err_msg = "Database's configurations is not found in configuration file"

test_proj_err_msg :: String
test_proj_err_msg = "TestProject's informations is not found in configuration file"
