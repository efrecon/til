proc dummy { val1 val2 } {
    puts "[clock format [clock seconds]]: $val1 $val2"
}

proc dumbo { } {
    puts "[clock format [clock seconds]] from dumbo"
}
