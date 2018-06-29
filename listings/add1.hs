-- start by entering the "overflow" state ...
add1 "start"    '§'    = ("overflow", '§', 1 )
-- ... and stay in this state, as long as you read only '1'-s
add1 "overflow" '1'    = ("overflow", '0', 1 )
-- if you read the first '0' or an empty cell replace it by '1'
-- and enter the "return" state to move the head to the first cell
add1 "overflow" '0'    = ("return",   '1', -1)
add1 "overflow" '_'    = ("return",   '1', -1)
-- we finish if we read '§' again or ...
add1 "return"   '§'    = ("halt",     '§', 0 )
-- ... continue to move to the left and don't change the cell
-- content. Here `b` matches '0' or '1'
add1 "return"   b      = ("return",   b  , -1)
add1 _state     _char  = ("error",    '_', 0 )
