lappend auto_path ..
package require preprocessor

::preprocessor::loglevel debug

proc output { id line } {
    puts $line
}

set rules {
    { RX ::preprocessor:: preprocess pp }
}
set id [::preprocessor::new $rules output ../preprocessor/preprocessor.tcl]

vwait forever
