-- start by entering the "check" state and ...
tally "start"    '§'    = ("check",    '§', 1   )
-- ... stay in this state while reading only '1'-s
tally "check"    '1'    = ("check",    '1', 1   )
-- on reading '_' accept the input and clear the tape ...
tally "check"    '_'    = ("accept",   '_', (-1))
tally "accept"   '1'    = ("accept",   '_', (-1))
-- ...except for cell c(1) where you write a '1'
tally "accept"   '§'    = ("accept",   '§', 1   )
tally "accept"   '_'    = ("halt",     '1', (-1))
-- however, if you read a '0' first reject the input
-- by moving to the end of the input string ...
tally "check"    '0'    = ("rejectMR", '0', 1   )
tally "rejectMR" '_'    = ("reject",   '_', (-1))
-- `b` matches '0' or '1'
tally "rejectMR" b      = ("rejectMR", b,   1   )
-- ... and clear the tape except for cell c(1) where you
-- write a '0'
tally "reject"   '§'    = ("reject",   '§', 1   )
tally "reject"   '_'    = ("halt",     '0', (-1))
-- `b` matches '0' or '1'
tally "reject"   b      = ("reject",   '_', (-1))
