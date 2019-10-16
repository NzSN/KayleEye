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

-- Letter Events Values
-- Message -- Information purposes
mr_event = "merge_request"
push_event = "push"
daily_event = "daily"
-- Control -- Control purposes
disconn_event = "disconn"
ack_event = "ack"
answer_event = "answer"
req_event = "req"
register_event = "register"
unRegister_event = "unRegister"
terminated_event = "terminated"

-- Letter Content Keys
content_who = "who"
content_answer = "answer"
content_term_type = "termType"

-- Letter Content Values
answer_accept = "Accepted"
answer_reject = "Rejected"

isMessageEvent "merge_request" = True
isMessageEvent "push" = True
isMessageEvent "daily" = True
isMessageEvent _ = False

isControlEvent "register" = True
isControlEvent "unRegister" = True
isControlEvent "req" = True
isControlEvent "ack" = True
isControlEvent "answer" = True
isControlEvent "disconn" = True
isControlEvent "terminated" = True
isControlEvent _ = False

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
