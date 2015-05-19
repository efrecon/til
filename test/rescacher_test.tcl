lappend auto_path ..
package require rescacher

::rescacher::loglevel debug

proc slow_proc { onearg } {
    after 500
    return "Executed $onearg"
}

puts "[time [list ::rescacher::call slow_proc 10]]"
puts "[time [list ::rescacher::call slow_proc 10]]"
puts "[time [list ::rescacher::call slow_proc 20]]"
puts "[time [list ::rescacher::call slow_proc 20]]"
puts "[time [list ::rescacher::call slow_proc 20]]"
puts "[time [list ::rescacher::call slow_proc 20]]"
