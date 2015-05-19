lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require massgeturl

::massgeturl::loglevel debug

proc fetch_progress { cid requrl url current total } {
    puts "FETCHING $url: $current bytes of $total"
}


proc fetch_result { cid url status token } {
    upvar \#0 $token state
    if { $status == "OK" } {
	puts "$state(url) Fetched"
    } else {
	puts "$url fetch terminated: $status, $token"
    }
}

puts [::massgeturl::defaults]
#::massgeturl::defaults -maxconns 2

set token [::massgeturl::syncget "http://ice.sics.se/" \
	      -progress fetch_progress]
if { $token != "" } {
    upvar \#0 $token state
    parray state
    ::http::cleanup $token
}

set cid [::massgeturl::get "http://www.sics.se/" fetch_result -timeout 1000]
::massgeturl::config $cid -retries 2

set cid [::massgeturl::get "http://www.microsoft.com/" fetch_result \
	    -progress fetch_progress]
::massgeturl::cancel $cid

::massgeturl::get "http://doping.sics.se/" fetch_result
foreach url [list "http://doping.sics.se/prof/archives/002870.html" \
		 "http://doping.sics.se/prof/archives/002869.html" \
		 "http://doping.sics.se/prof/archives/002868.html" \
		 "http://doping.sics.se/prof/archives/002866.html" \
		 "http://doping.sics.se/prof/archives/002864.html" \
		 "http://doping.sics.se/prof/archives/002863.html" \
		 "http://doping.sics.se/prof/archives/002862.html" \
		 "http://doping.sics.se/prof/archives/002860.html" \
		 "http://doping.sics.se/prof/archives/002856.html" \
		 "http://doping.sics.se/prof/archives/002854.html" \
		 "http://doping.sics.se/prof/archives/002853.html" \
		 "http://doping.sics.se/prof/archives/002852.html" \
		 "http://doping.sics.se/prof/archives/002849.html"] {
    ::massgeturl::get $url fetch_result
}

# Start downloading a URL that is far away and specifying a very short
# inactivity period to ensure that we trigger the mechanism.
set faraway_url "http://mymirror.asiaosc.org/openoffice/stable/1.1.4/OOo_1.1.4_Win32Intel_install.zip"
::massgeturl::get $faraway_url fetch_result -maxinactivity 50

vwait forever
