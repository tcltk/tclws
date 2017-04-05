##***************************************************************************##
##                                                                           ##
##    This is a stub and needs to be filled in!!!!                           ##
##                                                                           ##
##***************************************************************************##

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

package require Tcl 8.4
# WS::Utils usable here for dict?
if {![llength [info command dict]]} {
    package require dict
}
package require uri
package require base64
package require html

package provide WS::Wub 2.4.0

namespace eval ::WS::Wub {

    array set portInfo {}

    set portList [list]
    set forever {}
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Wub::AddHandler
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
# Pre-requisite Conditions : ::WS::Wub::Listen must have been called for the port
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
proc ::WS::Wub::AddHandler {port url callback} {
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
# Procedure Name : ::WS::Wub::AddHandlerAllPorts
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
# Pre-requisite Conditions : ::WS::Wub::Listen must have been called for the port
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
proc ::WS::Wub::AddHandlerAllPorts {url callback} {
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
# Procedure Name : ::WS::Wub::Listen
#
# Description : Instruct the module to listen on a Port, security information.
#
# Arguments :
#       port     -- Port number to listen on
#       certfile -- Name of the certificate file
#       keyfile  -- Name of the key file
#       userpwds -- A list of username and passwords
#       realm    -- The security realm
#       logger   -- A logging routines for errors
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Wub::Listen must have been called for the port
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
proc ::WS::Wub::Listen {port {certfile {}} {keyfile {}} {userpwds {}} {realm {}} {logger {::WS::Wub::logger}}} {
    variable portInfo
    variable portList

    lappend portList $port
    foreach key {port certfile keyfile userpwds realm logger} {
        set portInfo($port,$key) [set $key]
    }
    set portInfo($port,$handlers) {}
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
        ::tls::socket -server [list ::WS::Wub::accept $port] $port
    } else {
        socket -server [list ::WS::Wub::accept $port] $port
    }
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Wub::ReturnData
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
proc ::WS::Wub::ReturnData {socket type data code} {
    upvar #0 ::WS::Wub::Httpd$sock data

    foreach var {type data code} {
        dict set $data(reply) $var [set $var]
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
# Procedure Name : ::WS::Wub::Start
#
# Description : Start listening on all ports (i.e. enter the event loop).
#
# Arguments : None
#
# Returns :   Value that event loop was exited with.
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions :
#        ::WS::Wub::Listen should have been called for one or more port.
#
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
proc ::WS::Wub::Start {} {
    vairable forever

    set forever 0
    vwait ::WS::Wub::forever
    return $forever
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Wub::Stop
#
# Description : Exit dispatching request.
#
# Arguments :
#       value -- Value that ::WS::Embedded::Start should return,
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
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
proc ::WS::Wub::Stop {{value 1}} {
    vairable forever

    set forever $value
    vwait ::WS::Wub::forever
    return $forever
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Wub::logger
#
# Description : Stub for a logger.
#
# Arguments :
#       args            - not used
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
proc ::WS::Wub::logger {args} {
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Wub::respond
#
# Description : Send response back to user.
#
# Arguments :
#       sock -- Socket to send reply on
#       code -- Code to send
#       body -- HTML body to send
#       head -- HTML header to send
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
proc ::WS::Wub::respond {sock code body {head ""}} {
    puts -nonewline $sock "HTTP/1.0 $code ???\nContent-Type: text/html; charset=ISO-8859-1\nConnection: close\nContent-length: [string length $body]\n$head\n$body"
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Wub::checkauth
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
proc ::WS::Wub::checkauth {port sock ip auth} {
    variable portInfo

    if {[llength portInfo($port,auths)] && [lsearch -exact $portInfo($port,auths) $auth]==-1} {
        set realm $portInfo($port,realm)
        respond $sock 401 Unauthorized "WWW-Authenticate: Basic realm=\"$realm\"\n"
        $portInfo($port,logger) "Unauthorized from $ip"
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
# Procedure Name : ::WS::Wub::handler
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
#
#
###########################################################################
proc ::WS::Wub::handler {port sock ip reqstring auth} {
    variable portInfo
    upvar #0 ::WS::Wub::Httpd$sock req

    if {[catch {checkauth $port $sock $ip $auth}]} {
        return;
    }

    array set req $reqstring
    foreach var {type data code} {
        dict set $req(reply) $var [set $var]
    }
    set path $req(path)
    if {[dict exists $portInfo($port,handlers) $path]} {
        set cmd [dict get $portInfo($port,handlers) $path]
        lappend $cmd sock {}
    } else {
        respond $port $sock 404 "Error"
    }
    if {[catch {eval $cmd} msg]} {
        respond $port $sock 404 $msg
    } else {
        set data [dict get $req(reply) data]
        set reply "HTTP/1.0 [dict get $req(reply) code] ???\n"
        append reply "Content-Type: [dict get $req(reply) type]; charset=UTF-8\n"
        append reply "Connection: close\n"
        append reply "Content-length: [string length $data]\n"
        append reply "\n"
        append reply $data
        puts -nonewline $sock $reply
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
# Procedure Name : ::WS::Wub::accept
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
#
#
###########################################################################
proc ::WS::Wub::accept {port sock ip clientport} {
    variable portInfo

    if {[catch {
        gets $sock line
        set auth ""
        for {set c 0} {[gets $sock temp]>=0 && $temp ne "\r" && $temp ne ""} {incr c} {
            regexp {Authorization: Basic ([^\r\n]+)} $temp -- auth
            if {$c == 30} {
                $portInfo($port,logger)  "Too many lines from $ip"
            }
        }
        if {[eof $sock]} {
            $portInfo($port,logger)  "Connection closed from $ip"
        }
    foreach {method url version} $line { break }
    switch -exact $method {
        GET {
            handler $port $sock $ip [uri::split $url] $auth
        }
        default {
            $portInfo($port,logger)  "Unsupported method '$method' from $ip"
        }
    } } msg]} {
        $portInfo($port,logger)  "Error: $msg"
    }

    catch {flush $sock}
    catch {close $sock}
    return;
}
