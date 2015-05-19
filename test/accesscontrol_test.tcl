lappend auto_path ..
package require accesscontrol

set ctl [::accesscontrol::new %progdir%/test]
puts "Access of this program from kalle: [::accesscontrol::allow $ctl kalle]"
puts "Access of another program from kalle: [::accesscontrol::allow $ctl kalle anotherprog.tcl]"
puts "Access of another program from emmanuel: [::accesscontrol::allow $ctl emmanuel anotherprog.tcl]"
