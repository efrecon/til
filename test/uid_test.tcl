lappend auto_path ..
package require uid

::uid::loglevel debug

set uids [::uid::new]
puts "onestring: [::uid::id $uids onestring]"
puts "onestring: [::uid::id $uids onestring]"
puts "twostring: [::uid::id $uids twostring]"
