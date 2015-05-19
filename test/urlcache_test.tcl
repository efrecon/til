lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require urlcache
package require diskutil

::urlcache::loglevel debug
#::massgeturl::loglevel debug
#::urlhead::loglevel debug

set cachedir [::diskutil::temporary_directory urlcache_test]
set id [::urlcache::new $cachedir]
::urlcache::flow_monitor $id fetch_monitor

proc fetch_monitor { id url op args } {
    puts "-- $op $url: $args"
}

proc fetch_result { iteration id url status { fname "" }} {
    global g_iteration

    if { $status == "OK" } {
	puts "Successfully fetched url $url into $fname, iteration $iteration"
    } else {
	puts "Failed fetching url $url, iteration $iteration. Error: $fname"
    }
    incr g_iteration
}


proc async_fetch { url } {
    global id g_iteration

    set g_iteration 0
    puts "-------- FETCHING: $url"
    for { set i 0 } { $i < 10 } { incr i } {
	set fname [::urlcache::open $id $url "fetch_result $i"]
    }

    while { $g_iteration < 10 } {
	update
    }
}

proc async_fetch2 { urls } {
    global id g_iteration

    set g_iteration 0
    set i 0
    foreach url $urls {
	puts "-------- FETCHING: $url"
	set fname [::urlcache::open $id $url "fetch_result $i"]
	incr i
    }

    while { $g_iteration < [llength $urls] } {
	update
    }
}

proc fetch { url } {
    global id

    puts "-------- FETCHING: $url"
    set first_fname [::urlcache::open $id $url]
    puts "Successfully fetched url into $first_fname!"
    set second_fname [::urlcache::open $id $url]
    puts "Second name is $second_fname"
    if { $first_fname != $second_fname } {
	puts "Second fetch is different from first"
    }
}

# Fetch a dynamic URL, that should fetch the URL twice
#fetch "http://www.sics.se/"
fetch "http://yp.shoutcast.com/"
# Fetch a static URL, that should fetch the URL once only
fetch "http://doping.sics.se/"
# Fetch another static URL, check that both callbacks are called.
async_fetch "http://doping.sics.se/prof/"

async_fetch2 [list "http://doping.sics.se/prof/archives/002870.html" \
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
		  "http://doping.sics.se/prof/archives/002849.html"]

# Now fetch oursleves to test the fetching of local files.
fetch $argv0

::urlcache::delete $id
set id [::urlcache::new $cachedir]
::urlcache::delete $id 1

::diskutil::clean_directory $cachedir
file delete $cachedir
