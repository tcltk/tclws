###############################################################################
##                                                                           ##
##  Copyright (c) 2016-2020, Harald Oehlmann                                 ##
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

package require Tcl 8.6

package require uri
package require base64
package require html
package require log

# Emulate the log::logsubst command introduced in log 1.4
if {![llength [info command ::log::logsubst]]} {
    proc ::log::logsubst {level text} {
        if {[::log::lvIsSuppressed $level]} {
            return
        }
        ::log::log $level [uplevel 1 [list subst $text]]
    }
}

package provide WS::Embeded 3.3.1

namespace eval ::WS::Embeded {

    variable portInfo {}
    
    variable handlerInfoDict {}

    variable returnCodeText [dict create 200 OK 404 "Not Found" \
            500 "Internal Server Error" 501 "Not Implemented"]

    variable socketStateArray

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
#       urlPath  -- The URL path to register the callback for
#       method   -- HTTP method: GET or POST
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
# 3.2.0    2021-03-17  H.Oehlmann   Also pass method.
# 3.3.0    2021-03-19  H.Oehlmann   Put handler info to own dict, so order of
#                                   Listen and AddHandler call is not important.
#
#
###########################################################################
proc ::WS::Embeded::AddHandler {port urlPath method callback} {
    variable handlerInfoDict

    dict set handlerInfoDict $port $urlPath $method $callback
    return
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::GetValue
#
# Description : Get a value found in this module
#
# Arguments :
#       index    -- type of value to get. Possible values:
#                    -- isHTTPS : true, if https protocol is used.
#       port     -- concerned port. May be ommitted, if not relevant for value.
#
# Returns :     the distinct value
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
#
# Original Author : Harald Oehlmann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#   2.7.0  2020-10-26  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::GetValue {index {port ""}} {
    variable portInfo

    switch -exact -- $index {
        isHTTPS { return [dict get $portInfo $port $index] }
        default {return -code error "Unknown index '$index'"}
    }
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
#       certfile -- Name of the certificate file or a pfx archive for twapi.
#                   Defaults to {}.
#       keyfile  -- Name of the key file. Defaults to {}.
#                   To use twapi TLS, specify a list with the following elements:
#                    -- "-twapi": Flag, that TWAPI TLS should be used
#                    -- password: password of PFX file passed by
#                       [::twapi::conceal]. The concealing makes sure that the
#                       password is not readable in the error stack trace
#                    -- ?subject?: optional search string in pfx file, if
#                       multiple certificates are included.
#       userpwds -- A list of username:password. Defaults to {}.
#       realm    -- The security realm. Defaults to {}.
#       timeout  -- A time in ms the sender may use to send the request.
#                   If a sender sends wrong data (Example: TLS if no TLS is
#                   used), the process will just stand and a timeout is required
#                   to clear the connection. Set to 0 to not use a timeout.
#                   Default: 60000 (1 Minuit).
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
# 3.0.0    2020-10-30  H.Oehlmann   Add twapi tls support
# 3.3.0    2021-03-18  H.Oehlmann   Add timeout option. Remove unused portList.
#                                   Call Close, if we use the port already.
#                                   Do not leave portInfo data, if open fails.
# 3.3.1    2021-03-23  H.Oehlmann   Fix bug: pfx subject had added ")".
#
#
###########################################################################
proc ::WS::Embeded::Listen {port {certfile {}} {keyfile {}} {userpwds {}} {realm {}} {timeout 600000}} {
    variable portInfo
    
    ##
    ## Check if port already used by us. If yes, close it.
    ##
    if {[dict exists $portInfo $port]} {
        Close $port
    }
    
    ##
    ## Check if HTTPS protocol is used
    ##
    set isHTTPS [expr {$certfile ne ""}]
    
    if {$isHTTPS } {
        if { [string is list $keyfile] && [lindex $keyfile 0] eq "-twapi"} {

            ##
            ## Use TWAPI TLS
            ##

            package require twapi_crypto

            # Decode parameters
            #
            # certfile is the pfx file name
            # keyfile is a list of:
            #   -twapi: fix element
            #   password of the pfx file, passed by twapi::conceal
            #   Optional Subject of the certificate, if there are multiple
            #       certificates contained.
            #       If not given, the first certificate is used.
            set pfxpassword [lindex $keyfile 1]
            set pfxsubject ""
            if {[llength $keyfile] > 2} {
                set pfxsubject [lindex $keyfile 2]
            }
            # Create certificate selection tring
            if {$pfxsubject eq ""} {
                set pfxselection any
            } else {
                set pfxselection [list subject_substring $pfxsubject]
            }

            set hFile [open $certfile rb]
            set PFXCur [read $hFile]
            close $hFile
            # Set up the store containing the certificates
            # Import the PFX file and search the certificate.
            set certstore [twapi::cert_temporary_store -pfx $PFXCur \
                    -password $pfxpassword]
            set servercert [twapi::cert_store_find_certificate $certstore \
                    {*}$pfxselection]
            if {"" eq $servercert} {
                # There was no certificate included in the pfx file
                catch {twapi::cert_store_release $certstore}
                return -code error "no certificate found in file '$certfile'"
            }
            # The following is catched to clean-up in case of any error
            if {![catch {
                # Start the TLS socket with the credentials
                set creds [twapi::sspi_schannel_credentials \
                        -certificates [list $servercert] \
                        -protocols [list ssl3 tls1.1 tls1.2]]
                set creds [twapi::sspi_acquire_credentials \
                        -credentials $creds -package unisp -role server]
                set handle [::twapi::tls_socket \
                        -server [list ::WS::Embeded::accept $port] \
                        -credentials $creds $port]
            } errormsg errordict]} {
                # All ok, clear error flag
                unset errormsg
            }
            # Clean up certificate and certificate store
            if {[info exists servercert]} {
                catch {twapi::cert_release $servercert}
            }
            catch {twapi::cert_store_release $certstore}
            # Return error if happened above
            if {[info exists errormsg]} {
                dict unset errordict -level
                return -options  $errordict $errormsg
            }
        } else {

            ##
            ## Use TLS Package
            ##

            package require tls

            ::tls::init \
                -certfile $certfile \
                -keyfile  $keyfile \
                -require 0 \
                -request 0
            set handle [::tls::socket -server [list ::WS::Embeded::accept $port] $port]
        }
    } else {

        ##
        ## Use http protocol without encryption
        ##

        ::log::logsubst debug {socket -server [list ::WS::Embeded::accept $port] $port}
        set handle [socket -server [list ::WS::Embeded::accept $port] $port]
    }
    
    ##
    ## Prepare basic authentication
    ##
    set authlist {}
    foreach up $userpwds {
        lappend authlist [base64::encode $up]
    }

    ##
    ## Save the port information dict entry
    ##
    dict set portInfo $port [dict create\
            port $port\
            realm $realm\
            timeout $timeout\
            auths $authlist\
            isHTTPS $isHTTPS\
            handle $handle]

    return $handle
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::Close
#
# Description : End listening, close the port.
#
# Arguments :
#       port     -- Port number to listen on
#
# Returns :     none
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Harald Oehlmann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 3.3.0    2021-03-18  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::Close {port} {
    variable socketStateArray
    variable portInfo
    
    # Check, if port exists
    if {![dict exists $portInfo $port handle]} {return}
    
    ::log::log info "closing server socket for port $port"
    # close server port
    if {[catch {close [dict get $portInfo $port handle]} msg]} {
        ::log::log error "error closing server socket for port $port: $msg"
    }
    
    # close existing connections
    foreach sock [array names socketStateArray] {
        if {[dict get $socketStateArray($sock) port] eq $port} {
                cleanup $sock
        }
    }
    
    # remove registered data
    dict unset portInfo $port
    return
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::CloseAll
#
# Description : End listening, close all ports.
#
# Arguments :
#       port     -- Port number to listen on
#
# Returns :     none
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Harald Oehlmann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 3.3.0    2021-03-18  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::CloseAll {} {
    variable portInfo
    foreach port [dict keys $portInfo] {
        Close $port
    }
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
# 2.3.0    11/06/2012  H.Oehlmann   Separate head and body,
#                                   correct Content-length
#
#
###########################################################################
proc ::WS::Embeded::respond {sock code body {head ""}} {
    set body [encoding convertto iso8859-1 $body\r\n]
    if {[catch {
        chan configure $sock -translation crlf
        puts $sock "[httpreturncode $code]\nContent-Type: text/html; charset=ISO-8859-1\nConnection: close\nContent-length: [string length $body]"
        if {"" ne $head} {
            puts -nonewline $sock $head
        }
        # Separator head and body
        puts $sock ""
        chan configure $sock -translation binary
        puts -nonewline $sock $body
        close $sock
    } msg]} {
        log::log error "Error sending response: $msg"
        cleanup $sock
    } else {
        cleanup $sock 1
    }
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
# Procedure Name : ::WS::Embeded::handler
#
# Description : Handle a request.
#
# Arguments :
#       sock        -- Incoming socket
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
# 2.3.0    10/31/2012  G.Lester     bug fix for [68310fe3bd] -- correct encoding and data length
# 2.6.1    2020-10-22  H.Oehlmann   Do not pass parameter reqstring.
#                                   The corresponding value is found in global
#                                   array anyway.
#                                   Use charset handler of request decoding.
# 2.7.0    2020-10-26  H.Oehlmann   Pass additional port parameter to handle
#                                   functions. This helps to get isHTTPS
#                                   status for WSDL.
# 3.1.0    2020-11-05  H.Oehlmann   Pass additional port parameter with leading
#                                   -port specifier to avoid clash with
#                                   other parameters.
# 3.2.0    2021-03-17  H.Oehlmann   Return the result directly by the call.
#                                   Replace global parameter dict by parameter
#                                   url and dataDict (for POST method).
# 3.3.0    2021-03-18  H.Oehlmann   Use state array, move checks to Receive,
#                                   do query recode here.
#
###########################################################################
proc ::WS::Embeded::handler {sock} {
    variable socketStateArray

    set cmd [dict get $socketStateArray($sock) cmd]
    if {[dict get $socketStateArray($sock) method] eq "POST"} {
        # Recode the query data
        dict set socketStateArray($sock) query [encoding convertfrom\
                [dict get $socketStateArray($sock) requestEncoding]\
                [dict get $socketStateArray($sock) query]]

        # The following dict keys are attended: query, ipaddr, headers
        if {[catch {
            lassign [$cmd $sock -data $socketStateArray($sock)] type data code
        } msg]} {
            ::log::log error "Return 404 due to post eval error: $msg"
            tailcall respond $sock 404 "Error: $msg"
        }
    } else {
        if {[catch {
            lassign [$cmd $sock -port [dict get $socketStateArray($sock) port]] type data code
        } msg]} {
            ::log::log error "Return 404 due to get eval error: $msg"
            tailcall respond $sock 404 "Error: $msg"
        }
    }
    # This may modify the type variable, if encoding is not found
    set encoding [contentTypeParse 0 type]
    set data [encoding convertto $encoding $data]
    set reply "[httpreturncode $code]\n"
    append reply "Content-Type: $type\n"
    append reply "Connection: close\n"
    append reply "Content-length: [string length $data]\n"

    # Note: to avoid delay, full buffering is used on the channel.
    # In consequence, the data is sent in the background after the close.
    # Socket errors may not be detected, but the event queue is free.
    # This is specially important with the Edge browser, which sometimes delays
    # data reception.
    if {[catch {
        chan configure $sock -translation crlf
        puts $sock $reply
        chan configure $sock -translation binary
        puts -nonewline $sock $data
        close $sock
    } msg]} {
        ::log::log error "Error sending reply: $msg"
        tailcall cleanup $sock
    }
    ::log::log debug ok
    tailcall cleanup $sock 1
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
# Description : Accept an incoming connection and register callback.
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
# Original Author : Harald Oehlmann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 3.3.0    2021-03-18  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::accept {port sock ip clientport} {
    variable portInfo
    variable socketStateArray
    
    ::log::logsubst info {Received request on $port for $ip:$clientport}

    # Setup events
    if {[catch {
        chan configure $sock -blocking 0 -translation crlf
        chan event $sock readable [list ::WS::Embeded::receive $sock]
    } msg]} {
        catch {chan close $sock}
        ::log::log error "Error installing accepted socket on ip '$ip': $msg"
        return
    }
    # Prepare socket state dict
    set stateDict [dict create port $port ip $ip phase request]
    # Install timeout
    if {0 < [dict get $portInfo $port timeout]} {
        dict set stateDict timeoutHandle [after\
                [dict get $portInfo $port timeout]\
                [list ::WS::Embeded::timeout $sock]]
    }
    # Save state dict
    set socketStateArray($sock) $stateDict
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::receive
#
# Description : handle a readable socket
#
# Arguments :
#       sock        -- Incoming socket
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
# Original Author : Harald Oehlmann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 3.3.0    2021-03-18  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::receive {sock} {
    variable socketStateArray
    variable portInfo
    variable handlerInfoDict

    ##
    ## Make data read attempts in this read loop.
    ##
    while 1 {

        ::log::logsubst debug {Top of loop with dict: $socketStateArray($sock)}
        
        ##
        ## Read data
        ##
        if {[catch {
            if {[dict get $socketStateArray($sock) phase] eq "body"} {
                # Read binary data
                set line [chan read $sock [dict get $socketStateArray($sock) readMax]]
            } else {
                # read line data
                set line [chan gets $sock]
            }
        } msg]} {
            ::log::log error "Data read error: $msg"
            tailcall cleanup $sock
        }
        ::log::logsubst debug {Read: len [string length $line] eof [eof $sock] block [chan blocked $sock] data '$line'}

        ##
        ## Check for early EOF
        ##
        if { [eof $sock] } {
            ::log::log warning  {Connection closed from client}
            tailcall cleanup $sock
        }

        ##
        ## Check for no new data, so wait for next file event.
        ##
        ## For gets:
        ## This makes also the difference between empty data read (crlf
        ## terminated line) and no read (true).
        ## For read with limit:
        ## If not all characters could be read, block is flagged with data.
        ## So check the data length to 0 for this case.
        ##
        if {[string length $line] == 0 && [chan blocked $sock]} {
            return
        }
        
        ##
        ## Handle the received data
        ##
        switch -exact -- [dict get $socketStateArray($sock) phase] {
            request {
                ##
                ## Handle Request line
                ##
                if {![regexp {^([^ ]+) +([^ ]+) ([^ ]+)$} $line -> method url version]} {
                    ::log::logsubst warning  {Wrong request: $line}
                    tailcall cleanup $sock
                }
                if {$method ni {"GET" "POST"}} {
                    ::log::logsubst warning {Unsupported method '$method'}
                    tailcall respond $sock 501 "Method not implemented"
                }

                # Check if we have a handler for this method and URL path
                set urlPath "/[string trim [dict get [uri::split $url] path] /]"
                set port [dict get $socketStateArray($sock) port]
                if {![dict exists $handlerInfoDict $port $urlPath $method]} {
                    ::log::log warning "404 Error: URL path '$urlPath' not found"
                    tailcall respond $sock 404 "URL not found"
                }
                # Save data and pass to header phase
                dict set socketStateArray($sock) cmd [dict get $handlerInfoDict $port $urlPath $method]
                dict set socketStateArray($sock) phase header
                dict set socketStateArray($sock) method $method
                dict set socketStateArray($sock) header ""
            }
            header {
                ##
                ## Handle Header lines
                ##
                if {[string length $line] > 0} {
                    if {[regexp {^([^:]*):(.*)$} $line -> key data]} {
                        dict set socketStateArray($sock) header [string tolower $key] [string trim $data]
                    }
                } else {
                    # End of header by empty line

                    ##
                    ## Get authorization failure condition
                    ##
                    # Authorization is ok, if no authrization required
                    # Authorization fails, if:
                    # - no authentication in current request
                    # - or current credentials incorrect
                    set port [dict get $socketStateArray($sock) port]
                    if {    0 != [llength [dict get $portInfo $port auths]] &&
                            ! ( [dict exists $socketStateArray($sock) header authorization] &&
                                [regexp -nocase {^basic +([^ ]+)$} \
                                    [dict get $socketStateArray($sock) header authorization] -> auth] &&
                                $auth in [dict get $portInfo $port auths] )
                    } {
                        set realm [dict get $portInfo $port realm]
                        ::log::log warning {Unauthorized}
                        tailcall respond $sock 401 "" "WWW-Authenticate: Basic realm=\"$realm\"\n"
                    }
                    
                    # Within the GET method, we have all we need
                    if {[dict get $socketStateArray($sock) method] eq "GET"} {
                        tailcall handler $sock
                    }
                    
                    # Post method requires content-encoding header
                    if {![dict exists $socketStateArray($sock) header content-type]} {
                        ::log::logsubst warning  {Header missing: 'Content-Type'}
                        tailcall cleanup $sock
                    }
                    set contentType [dict get $socketStateArray($sock) header content-type]
                    dict set socketStateArray($sock) requestEncoding [contentTypeParse 1 contentType]

                    # Post method requires query data
                    dict set socketStateArray($sock) query ""
                    set fChunked [expr {
                            [dict exists $socketStateArray($sock) header transfer-encoding] &&
                            [dict get $socketStateArray($sock) header transfer-encoding] eq "chunked"}]
                    dict set socketStateArray($sock) fChunked $fChunked
                    
                    if {$fChunked} {
                        dict set socketStateArray($sock) phase chunk
                    } else {
                        
                        # Check for content length
                        if { ! [dict exists $socketStateArray($sock) header content-length] ||
                                0 == [scan [dict get $socketStateArray($sock) header content-length] %d contentLength]
                        } {
                            ::log::log warning "Header content-length missing"
                            tailcall cleanup $sock
                        }
                        dict set socketStateArray($sock) readMax $contentLength
                        dict set socketStateArray($sock) phase body

                        # Switch to binary data
                        if {[catch { chan configure $sock -translation binary } msg]} {
                            ::log::log error "Channel config error: $msg"
                            tailcall cleanup $sock
                        }
                    }
                }
            }
            body {
                ##
                ## Read body data
                ##
                set query [dict get $socketStateArray($sock) query]
                append query $line
                dict set socketStateArray($sock) query $query

                set readMax [expr {
                        [dict get $socketStateArray($sock) readMax] - [string length $line] } ]

                if {$readMax > 0} {
                    # Data missing, so loop
                    dict set socketStateArray($sock) readMax $readMax
                } else {
                    # We have all data
                    
                    if {[dict get $socketStateArray($sock) fChunked]} {
                        # Chunk read
                        # Switch to line mode
                        if {[catch { chan configure $sock -translation crlf } msg]} {
                            ::log::log error "Channel config error: $msg"
                            tailcall cleanup $sock
                        }
                        dict set socketStateArray($sock) phase chunk
                    } else {
                        # no chunk -> all data -> call handler
                        tailcall handler $sock
                    }
                }
            }
            chunk {
                ##
                ## Handle chunk header
                ##
                if {[scan $line %x length] != 1} {
                    ::log::log warning "No chunk length in '$line'"
                    tailcall cleanup $sock
                }
                if {$length > 0} {
                    # Receive chunk data
                    # Switch to binary data
                    if {[catch { chan configure $sock -translation binary } msg]} {
                        ::log::log error "Channel config error: $msg"
                        tailcall cleanup $sock
                    }
                    dict set socketStateArray($sock) readMax $length
                    dict set socketStateArray($sock) phase body
                } else {
                    # We have all data
                    tailcall handler $sock
                }
            }
        }
    }
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::timeout
#
# Description : socket timeout fired
#
# Arguments :
#       sock        -- Incoming socket
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
# Original Author : Harald Oehlmann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 3.3.0    2021-03-18  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::timeout {sock} {
    variable socketStateArray

    # The timeout fired, so the cancel handle is not required any more
    dict unset socketStateArray($sock) timeoutHandle
    
    ::log::log warning "Cancelling request due to timeout"
    tailcall cleanup $sock
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::cleanup
#
# Description : cleanup a socket
#
# Arguments :
#       sock        -- Incoming socket
#       fClosed     -- Socket already closed
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
# Original Author : Harald Oehlmann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 3.3.0    2021-03-18  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::cleanup {sock {fClosed 0}} {
    variable socketStateArray
    if {!$fClosed} {
        catch { chan close $sock }
    }
    if {[dict exists $socketStateArray($sock) timeoutHandle]} {
        after cancel [dict get $socketStateArray($sock) timeoutHandle]
    }
    unset socketStateArray($sock)
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::contentTypeParse
#
# Description : Parse a content-type value and get the encoding.
#               When receiving, only the encoding is required.
#               When sending, we have to correct the encoding, if not known
#               by TCL. Thus, the content-type string is changed.
#
# Arguments :
#       fReceiving  -- When receiving, we only need the extracted codepage.
#                       If sending, the content-type string must be modified,
#                       if the codepage is not found in tcl
#       contentTypeName --  The variable containing the content type string.
#
# Returns :
#       tcl encoding to apply
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Harald Oehlmann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
# 2.6.1    2020-10-22  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::contentTypeParse {fReceiving contentTypeName} {

    upvar 1 $contentTypeName contentType

    ##
    ## Extract charset parameter from content-type header
    ##

    # content-type example content: text/xml;charset=utf-8
    set paramList [lassign [split $contentType ";"] typeOnly]
    foreach parameterCur $paramList {
        set parameterCur [string trim $parameterCur]
        # Check for 'charset="<data>', where data may contain '\"'
        if {[regexp -nocase {^charset\s*=\s*\"((?:[^""]|\\\")*)\"$} \
                $parameterCur -> requestEncoding]
        } {
            set requestEncoding [string map {{\"} \"} $requestEncoding]
            break
        } else {
            # check for 'charset=<data>'
            regexp -nocase {^charset\s*=\s*(\S+?)$} \
                    $parameterCur -> requestEncoding
            break
        }
    }

    ##
    ## Find the corresponding TCL encoding name
    ##

    if {[info exists requestEncoding]} {
        if {[llength [info commands ::http::CharsetToEncoding]]} {
            # Use private http package routine
            set requestEncoding [::http::CharsetToEncoding $requestEncoding]
            # Output is "binary" if not found
            if {$requestEncoding eq "binary"} {
                unset requestEncoding
            }
        } else {
            # Reduced version of the http package version only honoring ISO8859-x
            # and encoding names identical to tcl encoding names
            set requestEncoding [string tolower $requestEncoding]
            if {[regexp {iso-?8859-([0-9]+)} $requestEncoding -> num]} {
                set requestEncoding "iso8859-$num"
            }
            if {$requestEncoding ni [encoding names]} {
                unset requestEncoding
            }
        }
    }

    ##
    ## Output found encoding and eventually content type
    ##

    # If encoding was found, just return it
    if {[info exists requestEncoding]} {
        return $requestEncoding
    }

    # encoding was not found
    if {$fReceiving} {
        # This is the http default so use that
        ::log::logsubst info {Use default encoding as content type header has missing/unknown charset in '$contentType'}
        return iso8859-1
    }

    # When sending, be sure to cover all characters, so use utf-8
    # correct content-type string (upvar)
    ::log::logsubst info {Set send charset to utf-8 due missing/unknown charset in '$contentType'}
    if {[info exists typeOnly]} {
        set contentType "${typeOnly};charset=utf-8"
    } else {
        set contentType "text/xml;charset=utf-8"
    }
    return utf-8
}

