lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require urlhead
package require diskutil

::urlhead::loglevel debug

proc head_result { url id status } {
    if { $status == "OK" } {
	puts "HEAD information for $url: [::urlhead::urlinfo $id]"
    } else {
	puts "Could not get information for $url!"
    }
}

::urlhead::get "http://doping.sics.se/" head_result
::urlhead::get "http://doping.sics.se/" head_result
after 5000 ::urlhead::get "http://doping.sics.se/" head_result 1

vwait forever
