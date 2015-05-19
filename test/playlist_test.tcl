lappend auto_path ..
package require playlist


::playlist::loglevel debug

if { $tcl_platform(platform) == "windows" } {
    set mdir "C:/tmp/music"
} else {
    set mdir "/local/users/emmanuel/music"
}

set files [list \
	       [file join $mdir "Lost and Delirious Soundtrack-Indian song played during end credits.mp3"] \
	       [file join $mdir "Mike Oldfield - Sally Oldfield- Moonlight Shadow (Extended Version).mp3"] \
	       [file join $mdir "Jeff Beckley -Lost and Delirious - forget her.mp3"]]


proc pls_monitor { txt id op fid args } {
    puts "$txt: $op $fid $args"
}

set pls [::playlist::new]
::playlist::flow_watch $pls * [list pls_monitor "Operation on playlist"]

set i 0
foreach fname $files {
    set id [::playlist::add $pls $fname]
    if { $i == 1 } {
	::playlist::setbounds $pls $id 30000
    }
    incr i
}

::playlist::play $pls
after 5000 ::playlist::next $pls
after 10000 ::playlist::pause $pls
after 12000 ::playlist::pause $pls
after 20000 ::playlist::play $pls \[::playlist::current $pls\]
after 30000 ::playlist::clear $pls
after 35000 ::playlist::next $pls
vwait forever
