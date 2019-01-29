-- start by entering the "check" state
check1 "start"       '§'    = ("check",       '§', 1 )
-- if you read a 1, check that the next cell is blank, and accept
check1 "check"       '1'    = ("check_blank", '1', 1 )
check1 "check_blank" '_'    = ("accept",      '_', -1)
-- if either of these conditions is not the case, reject
check1 "check"       b      = ("reject",       b,  1 )
check1 "check_blank" b      = ("reject",       b,  1 )
-- accepting and rejecting actions
check1 "accept"      1      = ("halt",         1,  -1)
check1 "reject"      b      = ("error",        b,  1 )
check1 _state        _char  = ("error",      '_',  0 )
