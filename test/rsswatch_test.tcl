
array set RP {
    finished 0
    rssid    ""
}

source ../bin/argutil.tcl
argutil_accesslib tcllib

package require cmdline

set options {
    { verbose.integer "1" "Verbosity Level" }
    { rss.arg "http://doping.sics.se/prof/index.rdf" "blog RSS url" }
}

if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set RP $optlist

argutil_accesslib til
#argutil_accesslib tclxml
#argutil_accesslib tcldom
#argutil_accesslib tclsoap
argutil_loadmodules rsswatch

proc rss_item { id op token } {
    puts "==== $op"
    upvar \#0 $token item
    puts "$item(title) $item(link)"
    puts "$item(description)"
    puts "$item(enclosure)"
    puts ""
}


set RP(rssid) [::rsswatch::new $RP(rss) rss_item -async off -dftperiod 0.25]

puts "Channel: [::rsswatch::feedinfo $RP(rssid) title] at\
      [::rsswatch::feedinfo $RP(rssid) link]"
puts "[::rsswatch::feedinfo $RP(rssid) description]"

vwait RP(finished)
