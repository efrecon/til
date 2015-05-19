source ../bin/argutil.tcl
::argutil::accesslib til
::argutil::loadmodules tax debug

foreach fname $argv {
    puts "============================================"
    puts "Parsing $fname"
    puts ""

    set f [open $fname]
    set p [::tax::new ::tax::output]
    ::tax::parse $p [read $f]
    close $f
}
