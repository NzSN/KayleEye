-- file: KayleConst.hs

module KayleConst where

k_ok :: Integer
k_ok = 0

k_error :: Integer
k_error = 1

boxKeyRetryInterval = 10 * second_micro_int

minute_micro :: Int
minute_micro = 60 * 1000000

second_micro_int :: Int
second_micro_int = 1000000

-- Letter events
mr_event = "merge_request"
push_event = "push"
daily_event = "daily"
control_event = "control"
disconn_event = "disconn"
ack_event = "ack"

-- Letter control commands
cmd_noMerged = "noMerged"
cmd_terminated = "terminated"

-- Unit is microseconds
seconds_micro :: Integer
seconds_micro = 1000000

-- Error messages
accept_err_msg :: String
accept_err_msg = "Accept url is not found in configuration file"

rebase_err_msg :: String
rebase_err_msg = "Rebase url is not found in configuration file"

serverAddr_err_msg :: String
serverAddr_err_msg = "Server's informations is not found in configuration file"

db_err_msg :: String
db_err_msg = "Database's configurations is not found in configuration file"

test_proj_err_msg :: String
test_proj_err_msg = "TestProject's informations is not found in configuration file"

test_cmd_err_msg :: String
test_cmd_err_msg = "Test commands is not found in configuration file"
