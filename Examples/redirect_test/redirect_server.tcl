# Test tclws redirection
# 2015-11-09 by Harald Oehlmann
#
# If (set loop 1), infinite redirect is tested, otherwise one redirect.
# Start the embedded test server and use redirect_call to call.
#
set auto_path [linsert $auto_path 0 [file join [file dirname [info script]] ../..]]
catch {console show}
package require uri

proc ::Listen {port} {
	return [socket -server ::Accept $port]
}


proc ::Accept {sock ip clientport} {
    if {1 == [catch {
        gets $sock line
        set request {}
        while {[gets $sock temp] > 0 && ![eof $sock]} {
            if {[regexp {^([^:]*):(.*)$} $temp -> key data]} {
                dict set request header [string tolower $key] [string trim $data]
            }
        }
        if {[eof $sock]} {
            puts "Connection closed from $ip"
            return
        }
        if {![regexp {^([^ ]+) +([^ ]+) ([^ ]+)$} $line -> method url version]} {
            puts  "Wrong request: $line"
            return
        }
        array set uri [::uri::split $url]
        if {[info exists ::loop]} {
            set uri(host) "localhost:8014"
        } else {
            set uri(host) "localhost:8015"
        }
        set url [eval ::uri::join [array get uri]]
        puts "Redirecting to $url"
        puts $sock "HTTP/1.1 301 Moved Permanently"
        puts $sock "Location: $url"
        puts $sock "Content-Type: text/html"
        puts $sock "Content-Length: 0\n\n"
        close $sock
    } Err]} {
        puts "Socket Error: $Err"
        return
    }
}

Listen 8014



