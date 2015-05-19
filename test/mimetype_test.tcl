lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require mimetype

::mimetype::loglevel debug

foreach fname [list \
		   "/mnt/windows/home/emmanuel/tmp/DSCF0039.JPG" \
		   "/mnt/windows/home/emmanuel/tmp/chapter4.doc" \
		   "/mnt/windows/home/emmanuel/tmp/paulweller_back.gif" \
		   "/mnt/windows/home/emmanuel/tmp/marten.png"] {
    set mtype1 [::mimetype::guess $fname]
    set mtype2 [::mimetype::guess $fname 1]
    puts "$fname: $mtype1 $mtype2"
}
		   
	       
