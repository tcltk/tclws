# WSWub - Wub interface to WebServices
package require Tcl 8.4
# WS::Utils usable here for dict?
if {![llength [info command dict]]} {
    package require dict
}
package require WS::Server

package require OO
package require Direct
package require Debug
Debug off wsdl 10

package provide WS::Wub 2.4.0
package provide Wsdl 2.4.0

class create Wsdl {
    method / {r args} {
	return [Http Ok $r [::WS::Server::generateInfo $service 0] text/html]
    }

    method /op {r args} {
	if {[catch {::WS::Server::callOp $service 0 [dict get $r -entity]} result]} {
	    return [Http Ok $r $result]
	} else {
	    dict set r -code 500
	    dict set r content-type text/xml
	    dict set r -content $result
	    return [NoCache $r]
	}
    }

    method /wsdl {r args} {
	return [Http Ok $r [::WS::Server::GetWsdl $service] text/xml]
    }

    mixin Direct	;# Direct mixin maps the URL to invocations of the above methods
    variable service
    constructor {args} {
	set service [dict get $args -service]	;# we need to remember the service name
    }
}
