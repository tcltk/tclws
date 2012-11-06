###############################################################################
##                                                                           ##
##  Copyright (c) 2008, Gerald W. Lester                                     ##
##  All rights reserved.                                                     ##
##                                                                           ##
##  Redistribution and use in source and binary forms, with or without       ##
##  modification, are permitted provided that the following conditions       ##
##  are met:                                                                 ##
##                                                                           ##
##    * Redistributions of source code must retain the above copyright       ##
##      notice, this list of conditions and the following disclaimer.        ##
##    * Redistributions in binary form must reproduce the above              ##
##      copyright notice, this list of conditions and the following          ##
##      disclaimer in the documentation and/or other materials provided      ##
##      with the distribution.                                               ##
##    * Neither the name of the Visiprise Software, Inc nor the names        ##
##      of its contributors may be used to endorse or promote products       ##
##      derived from this software without specific prior written            ##
##      permission.                                                          ##
##                                                                           ##
##  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      ##
##  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        ##
##  LIMITED  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       ##
##  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE           ##
##  COPYRIGHT OWNER OR  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     ##
##  INCIDENTAL, SPECIAL,  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    ##
##  BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        ##
##  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER         ##
##  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT       ##
##  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING IN       ##
##  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  ADVISED OF THE         ##
##  POSSIBILITY OF SUCH DAMAGE.                                              ##
##                                                                           ##
###############################################################################

package require uri
package require base64
package require html
package require log

package provide WS::Embeded 2.3.0

namespace eval ::WS::Embeded {

    array set portInfo {}

    set portList [list]
    set forever {}
    
    variable returnCodeText [dict create 200 OK 404 "Not Found"\
	    500 "Internal Server Error" 501 "Not Implemented"]
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::AddHandler
#
# Description : Register a handler for a url on a port.
#
# Arguments :
#       port     -- The port to register the callback on
#       url      -- The URL to register the callback for
#       callback -- The callback prefix, two additionally arguments are lappended
#                   the callback: (1) the socket (2) the null string
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Embeded::AddHandler {port url callback} {
    variable portInfo

    dict set portInfo($port,handlers) $url $callback
    return;
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::AddHandlerAllPorts
#
# Description : Register a handler for a url on all "defined" ports.
#
# Arguments :
#       url      -- List of three elements:
#       callback -- The callback prefix, two additionally argumens are lappended
#                   the callback: (1) the socket (2) the null string
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Embeded::AddHandlerAllPorts {url callback} {
    variable portList

    foreach port $portList {
        AddHandler $port $url $callback
    }

    return;
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::Listen
#
# Description : Instruct the module to listen on a Port, security information.
#
# Arguments :
#       port     -- Port number to listen on
#       certfile -- Name of the certificate file
#       keyfile  -- Name of the key file
#       userpwds -- A list of username:password
#       realm    -- The security realm
#
# Returns :     socket handle
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Embeded::Listen {port {certfile {}} {keyfile {}} {userpwds {}} {realm {}}} {
    variable portInfo
    variable portList

    lappend portList $port
    foreach key {port certfile keyfile userpwds realm} {
        set portInfo($port,$key) [set $key]
    }
    if {![info exists portInfo($port,handlers)]} {
        set portInfo($port,handlers) {}
    }
    foreach up $userpwds {
        lappend portInfo($port,auths) [base64::encode $up]
    }

    if {$certfile ne ""} {
        package require tls

        ::tls::init \
            -certfile $certfile \
            -keyfile  $keyfile \
            -ssl2 1 \
            -ssl3 1 \
            -tls1 0 \
            -require 0 \
            -request 0
        set handle [::tls::socket -server [list ::WS::Embeded::accept $port] $port]
    } else {
        ::log::log debug [list socket -server [list ::WS::Embeded::accept $port] $port]
        set handle [socket -server [list ::WS::Embeded::accept $port] $port]
    }

    return $handle
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::ReturnData
#
# Description : Store the information to be returned.
#
# Arguments :
#       socket  -- Socket data is for
#       type    -- Mime type of data
#       data    -- Data
#       code    -- Status code
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : A callback on the socket should be pending
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Embeded::ReturnData {sock type data code} {
    upvar #0 ::WS::Embeded::Httpd$sock dataArray

    foreach var {type data code} {
        dict set dataArray(reply) $var [set $var]
    }
    return;
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::respond
#
# Description : Send response back to user.
#
# Arguments :
#       sock -- Socket to send reply on
#       code -- Code to send
#       body -- HTML body to send
#       head -- Additional HTML headers to send
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#   2.3.0  11/06/2012  H.Oehlmann   Separate head and body,
#                                   correct Content-length
#
#
###########################################################################
proc ::WS::Embeded::respond {sock code body {head ""}} {
    set body [encoding convertto iso8859-1 $body\r\n]
    chan configure $sock -translation crlf
    puts $sock "[httpreturncode $code]\nContent-Type: text/html; charset=ISO-8859-1\nConnection: close\nContent-length: [string length $body]"
    if {"" ne $head} {
	puts -nonewline $sock $head
    }
    # Separator head and body
    puts $sock ""
    chan configure $sock -translation binary
    puts -nonewline $sock $body
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::httpreturncode
#
# Description : Format the first line of a http return including the status code
#
# Arguments :
#       code -- numerical http return code
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  10/05/2012  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::httpreturncode {code} {
    variable returnCodeText
    if {[dict exist $returnCodeText $code]} {
	set textCode [dict get $returnCodeText $code]
    } else {
	set textCode "???"
    }
    return "HTTP/1.0 $code $textCode"
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::checkauth
#
# Description : Check to see if the user is allowed.
#
# Arguments :
#       port -- Port number
#       sock -- Incoming socket
#       ip   -- Requester's IP address
#       auth -- Authentication information
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Embeded::checkauth {port sock ip auth} {
    variable portInfo

    if {[info exists portInfo($port,auths)] && [llength $portInfo($port,auths)] && [lsearch -exact $portInfo($port,auths) $auth]==-1} {
        set realm $portInfo($port,realm)
        respond $sock 401 "" "WWW-Authenticate: Basic realm=\"$realm\"\n"
        ::log::log warning "Unauthorized from $ip"
        return -code error
    }
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::handler
#
# Description : Handle a request.
#
# Arguments :
#       port        -- Port number
#       sock        -- Incoming socket
#       ip          -- Requester's IP address
#       reqstring   -- Requester's message
#       auth        -- Authentication information
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#   2.3.0  10/31/2012  G.Lester     bug fix for [68310fe3bd] -- correct encoding and data length
#
#
###########################################################################
proc ::WS::Embeded::handler {port sock ip reqstring auth} {
    variable portInfo
    upvar #0 ::WS::Embeded::Httpd$sock req

    if {[catch {checkauth $port $sock $ip $auth}]} {
        ::log::log warning {Auth Failed}
        return
    }

    array set req $reqstring
    #foreach var {type data code} {
    #    dict set req(reply) $var [set $var]
    #}
    set path "/[string trim $req(path) /]"
    if {[dict exists $portInfo($port,handlers) $path]} {
        set cmd [dict get $portInfo($port,handlers) $path]
        lappend cmd $sock {}
        #puts "Calling {$cmd}"
        if {[catch {eval $cmd} msg]} {
            ::log::log error "Return 404 due to eval error: $msg"
            respond $sock 404 "Error: $msg"
        } else {
            set type [dict get $req(reply) type]
            set encoding [string tolower [lindex [split [lindex [split $type {;}] 1] {=}] 1]]
            if {$encoding ni [encoding names]} {
                set encoding utf-8
                set type "[lindex [split $type ";"] 0]; charset=UTF-8"
            }
            set data [encoding convertto $encoding [dict get $req(reply) data]]
            set reply "[httpreturncode [dict get $req(reply) code]]\n"
            append reply "Content-Type: $type\n"
            append reply "Connection: close\n"
            append reply "Content-length: [string length $data]\n"
            chan configure $sock -translation crlf
            puts $sock $reply
            chan configure $sock -translation binary
            puts -nonewline $sock $data
            ::log::log debug ok
        }
    } else {
        ::log::log warning "404 Error: URL not found"
        respond $sock 404 "URL not found"
    }

    return;
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::accept
#
# Description : Accept an incoming connection.
#
# Arguments :
#       port        -- Port number
#       sock        -- Incoming socket
#       ip          -- Requester's IP address
#       clientport  -- Requester's port number
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#   2.3.0  10/31/2012  G.Lester     Bug fix [66fb3aeef5] -- correct header parsing
#
#
###########################################################################
proc ::WS::Embeded::accept {port sock ip clientport} {
    variable portInfo

    upvar #0 ::WS::Embeded::Httpd$sock query
    ::log::log info "Receviced request on $port for $ip:$clientport"

    array unset query reply
    chan configure $sock -translation crlf
    if {1 == [catch {
        gets $sock line
        ::log::log debug "Request is: $line"
        set auth {}
        set request {}
        while {[gets $sock temp] > 0 && ![eof $sock]} {
            if {[regexp {^([^:]*):(.*)$} $temp -> key data]} {
                dict set request header [string tolower $key] [string trim $data]
            }
        }
        if {[eof $sock]} {
            ::log::log warning  "Connection closed from $ip"
            return
        }
        if {[dict exists $request header authorization]} {
            regexp -nocase {^basic +([^ ]+)$}\
                [dict get $request header authorization] -> auth
        }
        if {![regexp {^([^ ]+) +([^ ]+) ([^ ]+)$} $line -> method url version]} {
            ::log::log warning  "Wrong request: $line"
            return
        }
        switch -exact -- $method {
            POST {
                ##
                ## This is all broken and needs to be fixed
                ##
                set data ""
                if {[dict exists $request header transfer-encoding]
                    && [dict get $request header transfer-encoding] eq "chunked"} {
                    # Receive chunked request body.
                    while {[scan [gets $sock line] %x length] == 1 && $length > 0} {
                        chan configure $sock -translation binary
                        append data [read $sock $length]
                        chan configure $sock -translation crlf
                    }
                } else {
                    # Receive non-chunked request body.
                    chan configure $sock -translation binary
                    set data [read $sock [dict get $request header content-length]]
                    chan configure $sock -translation crlf
                }
                array set query [uri::split $url]
                set query(query) $data
                set query(headers) $request
                set query(ipaddr) $ip
                #parray query
                handler $port $sock $ip [array get query] $auth
            }
            GET {
                handler $port $sock $ip [uri::split $url] $auth
            }
            default {
                ::log::log warning "Unsupported method '$method' from $ip"
                respond $sock 501 "Method not implemented"
            }
        }
    } msg]} {
        ::log::log error "Error: $msg"
        # catch this against an eventual closed socket
        catch {respond $sock 500 "Server Error"}
    }

    catch {flush $sock}
    catch {close $sock}
    return
}
