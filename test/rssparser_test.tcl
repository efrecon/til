lappend auto_path ../rss
package require rssparser

proc traverse { e_id } {
    global id

    set varname [::rssparser::get_element $id $e_id]
    upvar \#0 $varname Element

    puts "================="
    parray Element
    if { $Element(name) == "channel" } {
	foreach i_id $Element(items) {
	    traverse $i_id
	}
    }
}

set id [::rssparser::new]
set fname "maccast.xml"
#set fname "/home/emmanuel/projects/daphne/pondcast/cache/urlcache_1805828796_445.tmp"
set f [open $fname]
set data [read $f]
close $f

set cid [::rssparser::parse $id $data]
traverse $cid


::rssparser::destroy $id $cid
