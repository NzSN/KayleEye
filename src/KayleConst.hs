-- file: KayleConst.hs

module KayleConst where

-- Unit is microseconds
seconds_micro :: Integer
seconds_micro = 1000000

-- configPath is a url point to gitlab where to store
-- configuration files.
configPath :: String
configPath = "git@gpon.git.com:root/CI_Config.git"

-- Error messages
serverAddr_err_msg :: String
serverAddr_err_msg = "Server's informations is not found in configuration file"

db_err_msg :: String
db_err_msg = "Database's configurations is not found in configuration file"

test_proj_err_msg :: String
test_proj_err_msg = "TestProject's informations is not found in configuration file"
