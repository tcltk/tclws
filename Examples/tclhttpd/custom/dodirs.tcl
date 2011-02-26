# code to start tclhttpd modules contained in subdirs of custom/

set here [file dirname [info script]]
foreach dir [glob -nocomplain [file join [file normalize $here] *]] {
    if {[file isdirectory $dir] && [file exists [file join $dir startup.tcl]]} {
	if {$Config(debug)} {
	    Stderr "Loading code from module $dir"
	}
	if {[catch {source [file join $dir startup.tcl]} err]} {
	    Stderr "$dir: $err"
	} elseif {$Config(debug)} {
	    Stderr "Loaded [file tail $dir]: $err"
	}
    }
}
