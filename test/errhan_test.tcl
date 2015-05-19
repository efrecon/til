lappend auto_path ../errhan
package require errhan

proc catcherr { err } {
    if { [string match "*bad option*" $err] } {
	puts "Error catched, continuing"
	return 1
    } else {
	return 0
    }
}


::errhan::add catcherr

after 100 clock sjdfbsdkjf
after 200 clock format


vwait forever
