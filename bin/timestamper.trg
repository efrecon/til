# This is the configation file for the triggers of the timestamper. In this
# file, all blank lines or commented lines (as this one!) will be ignored.
# Otherwise, lines should contain exactly 5 keywords separated by whitespaces.
# These keywords are as below.
#
# Note that in the procedure "name", the ! sign can be used to separate the real
# name (the first element) from a number of arguments (the remaining arguments).
# When called the procedure will be called with the type of the pattern, the
# pattern, the number of occurrences and the time between (in seconds) between
# the matching occurrences, followed by all arguments (spearated by !)
#
# Type  pattern number latest proc@location.tcl
GLOB    *       0       0       noop@%progdir%/%progname%-doesnotexist.tcl