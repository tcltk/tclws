package require Tcl 8.4

namespace eval ::WS::AOLserver {

    if {![info exists logVersion]} {
	variable logVersion [package require log]
    }
    if {![info exists wsVersion]} {
	variable wsVersion [package require WS::Server]
    }

    namespace import -force ::log::log
}

proc ::WS::AOLserver::ReturnData {sock type data code} {

    log debug "AOLserver::ReturnData returning $code $type $data"
    ns_return $code $type $data

}

proc ::WS::AOLserver::AddHandler {args} {
    log debug "AOLserver::AddHandler added '$args'"
}

proc ::WS::AOLserver::Init { } {

    uplevel 1 {
	set server    [ns_info server]
	set nsset     [ns_configsection "ns/server/$server/module/nssock"]
	set headerSet [ns_conn headers]
	set host      [string tolower [ns_set iget $headerSet host]]
	set hostList  [split $host :]
	set peeraddr  [ns_conn peeraddr]

	if {[llength $hostList] == 1} {
	    set port 80
	} else {
	    set port [lindex $hostList 1]
	}

	set url  [lindex [split [lindex [ns_conn request] 1] ?] 0]
	set urlv [split $url /]

	switch -exact -- [lindex $urlv end] {
	    "" {
		# get service description
		set requestType doc
	    }
	    "wsdl" {
		# return wsdl
		set requestType wsdl
	    }
	    "op" {
		set requestType op
	    }
	    default {
		set requestType [lindex $urlv end]
	    }
	}

	set prefix  [join [lrange $urlv 0 end-1] /]
	set service [lindex $urlv end-1]

	::log::log debug "prefix = $prefix service = $service requestType = $requestType"
    }
}

proc ::WS::AOLserver::Return {} {

    uplevel 1 {

	set sock nosock

	switch -exact -- $requestType {

	    doc {
		::WS::Server::generateInfo $service $sock
	    }
	    wsdl {
		::WS::Server::generateWsdl $service $sock
	    }
	    op {
		upvar #0 Httpd$sock data

		# Copy Headers/ip
		set headerLength [ns_set size $headerSet]
		for {set i 0} {$i < $headerLength} {incr i} {
		    lappend headers [ns_set key $headerSet $i] [ns_set value $headerSet $i]
		}
		set data(ipaddr) $peeraddr
		set data(headerlist) $headers

		# Get POSTed data
		set length [ns_set iget $headerSet "Content-length"]
		set tmpFile [ns_tmpnam]
		::log::log debug "Using tmpFile = $tmpFile"
		set fp [ns_openexcl $tmpFile]
		fconfigure $fp -translation binary
		ns_conn copy 0 $length $fp
		seek $fp 0
		set data(query) [read $fp]
		close $fp

		# Run request
		::WS::Server::callOperation $service $sock
	    }
	    default {
		ns_return 200 text/plain "prefix = $prefix service = $service requestType = $requestType"
	    }
	}
    }
}

package provide WS::AOLserver 2.4.0
