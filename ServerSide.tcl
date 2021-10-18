###############################################################################
##                                                                           ##
##  Copyright (c) 2016-2021, Harald Oehlmann                                 ##
##  Copyright (c) 2006-2013, Gerald W. Lester                                ##
##  Copyright (c) 2008, Georgios Petasis                                     ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
##  Copyright (c) 2006, Colin McCormack                                      ##
##  Copyright (c) 2006, Rolf Ade                                             ##
##  Copyright (c) 2001-2006, Pat Thoyts                                      ##
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
package require WS::Utils
package require html
package require log
package require tdom

package provide WS::Server 3.4.0

namespace eval ::WS::Server {
    array set ::WS::Server::serviceArr {}
    set ::WS::Server::procInfo {}
    set ::WS::Server::mode {}
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Server::Service
#
# Description : Declare a Web Service, the following URLs will exist
#                 /service/<ServiceName>
#                       Displays an HTML page describing the service
#                 /service/<ServiceName>/wsdl
#                       Returns a WSDL describing the service
#                 /service/<ServiceName>/op
#                       Invoke an operation
#
# Arguments : this procedure uses position independent arguments, they are:
#               -hostcompatibility32 bool - Activate version 3.2.0 compatibility
#                                 mode for -host parameter.
#                                 Defaults to true.
#               -host           - The host specification within XML namespaces
#                                 of the transmitted XML files.
#                                 This should be unique.
#                                 Defaults to localhost.
#                                 If 3.2 compatibility is activated, the default
#                                 value is changed to ip:port in embedded mode.
#               -hostlocation   - The host name which is promoted within the
#                                 generated WSDL file. Defaults to localhost.
#                                 If 3.2 compatibility is activated, the
#                                 default value is equal to the -host parameter.
#               -hostlocationserver bool - If true, the host location is set by
#                                 the current server settings.
#                                 In case of httpd server, this value is imported.
#                                 For other servers or if this fails, the value
#                                 is the current ip:port.
#                                 The default value is true.
#                                 In case of 3.2 compatibility, the default
#                                 value is true for tclhttpd, false otherwise.
#               -hostprotocol   - Define the host protocol (http, https) for the
#                                 WSDL location URL. The special value "server"
#                                 (default) follows the TCP/IP server specification.
#                                 This is implemented for Embedded server and tclhttpd.
#                                 Remark that the protocol for XML namespaces
#                                 is always "http".
#               -description    - The HTML description for this service
#               -service        - The service name (this will also be used for
#                                 the Tcl namespace of the procedures that implement
#                                 the operations.
#               -premonitor     - This is a command prefix to be called before
#                                 an operation is called.  The following arguments are
#                                 added to the command prefix:
#                                     PRE serviceName operationName operArgList
#               -postmonitor    - This is a command prefix to be called after
#                                 an operation is called.  The following arguments are
#                                 added to the command prefix:
#                                     POST serviceName operationName OK|ERROR results
#               -inheaders      - List of input header types.
#               -outheaders     - List of output header types.
#               -intransform    - Inbound (request) transform procedure.
#               -outtransform   - Outbound (reply) transform procedure.
#               -checkheader    - Command prefix to check headers.
#                                     If the call is not to be allowed, this command
#                                     should raise an error.
#                                     The signature of the command must be:
#                                       cmd \
#                                           service \
#                                           operation \
#                                           caller_ipaddr \
#                                           http_header_list \
#                                           soap_header_list
#               -mode           - Mode that service is running in.  Must be one of:
#                                   tclhttpd  -- running inside of tclhttpd or an
#                                                environment that supplies a
#                                                compatible Url_PrefixInstall
#                                                and Httpd_ReturnData commands
#                                   embedded  -- using the ::WS::Embedded package
#                                   aolserver -- using the ::WS::AolServer package
#                                   wub       -- using the ::WS::Wub package
#                                   wibble    -- running inside of wibble
#                                   rivet     -- running inside Apache Rivet (mod_rivet)
#                                   channel   -- use a channel pair, WSDL is return if no XML
#                                                otherwise an operation is called
#               -ports          - List of ports.  Only valid for embedded and channel mode
#                                   For chanel mode. Defaults to {stdin stdout}
#                                               NOTE -- a call should be to
#                                                      ::WS::Channel::Start to process data
#                                   For embedded mode. Default: 80
#                                               NOTE -- a call should be to
#                                                      ::WS::Embedded::Listen
#                                                      for each port in this
#                                                      list prior to this call
#               -prefix         - Path prefix used for the namespace and endpoint
#                                 Defaults to "/service/" plus the service name
#               -traceEnabled   - Boolean to enable/disable trace being passed back in exception
#                                 Defaults to "Y"
#               -docFormat      - Format of the documentation for operations ("text" or "html").
#                                 Defaults to "text"
#               -stylesheet     - The CSS stylesheet URL used in the HTML documentation
#               -errorCallback  - Callback to be invoked in the event of an error being produced
#               -verifyUserArgs - Boolean to enable/disable validating user supplied arguments
#                                 Defaults to "N"
#               -enforceRequired - Throw an error if a required field is not included in the
#                                  response.
#                                  Defaults to "N"
#
#
# Returns :     Nothing
#
# Side-Effects :        None
#
# Exception Conditions :
#       MISSREQARG -- Missing required arguments
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
#       1  07/06/2006  G.Lester     Initial version
# 2.7.0    2020-10-26  H.Oehlmann   Embedded server: Do not add port 443 to default url
# 3.0.0    2020-10-30  H.Oehlmann   New option -hostProtocol
# 3.2.0    2021-03-17  H.Oehlmann   Add HTTP method to embedded registration.
#                                   Change default of -checkheader from
#                                   ::WS::Server::ok to the empty string.
# 3.3.0    2021-10-15  H.Oehlmann   Rename option -hostProtocol to -hostprotocol.
#                                   Still accept -hostProtocol.
#                                   The logic on .hostprotocol=server is changed
#                                   to default to http and set an update flag.
#                                   The default value of -host in embedded mode
#                                   is <current IP>:port. This is critial, if
#                                   the IP changes and thus the XML namespace.
#                                   If the old WSDL with old IP is used, there
#                                   are no input parameters recognized, as the
#                                   XML namespace has changed.
#                                   In consequence, a new parameter set including
#                                   options -host, -hostlocation, -hostlocationserver
#                                   and -hostcompatibility32 is defined as
#                                   described above.
#
###########################################################################
proc ::WS::Server::Service {args} {
    variable serviceArr
    variable procInfo
    variable mode

    ::log::logsubst debug {Defining Service as $args}

    set defaults [dict create\
        -checkheader    {}\
        -inheaders      {}\
        -outheaders     {}\
        -intransform    {}\
        -outtransform   {}\
        -htmlhead       {TclHttpd Based Web Services}\
        -author         {}\
        -description    {}\
        -mode           {tclhttpd}\
        -ports          {80}\
        -traceEnabled   {Y}\
        -docFormat      {text}\
        -stylesheet     {}\
        -beautifyJson   {N}\
        -errorCallback  {}\
        -verifyUserArgs {N}\
        -enforceRequired {N}\
        -hostcompatibility32 1]

    set defaults [dict merge $defaults $args]
    
    # Set default -ports value if mode equal channel
    if {    [dict get $defaults -mode] eq "channel" &&
            ! [dict exists $args -ports] } {
        dict set defaults -ports {stdin stdout}
    }
    set requiredList {-service}
    set missingList {}
    foreach opt $requiredList {
        if {![dict exists $defaults $opt]} {
            lappend missingList $opt
        }
    }
    if {[llength $missingList]} {
        return \
            -code error \
            -errorcode [list WSSERVER MISSREQARG $missingList] \
            "Missing required arguments '[join $missingList {,}]'"
    }
    set service [dict get $defaults -service]
    if {![dict exists $defaults -prefix]} {
        dict set defaults -prefix /service/$service
    }
    # find default host/host location
    if {[dict get $defaults -hostcompatibility32]} {

        ##
        ## Version 3.2.x compatibility mode:
        ## -host: default value is changed to ip:port in embedded mode.
        ## -hostlocation : default value is equal to the -host parameter.
        ## -hostlocationserver: default true for tclhttpd, false otherwise.
        ##

        if { ! [dict exists $defaults -hostlocationserver]} {
            dict set defaults -hostlocationserver [expr {
                    [dict get $defaults -mode] eq "tclhttpd" }]
        }
        if {![dict exists $defaults -host]} {
            if { [dict get $defaults -mode] eq "embedded" } {
                set myHostLocation [MyHostLocationGet [dict get $defaults -ports]]
                dict set defaults -host $myHostLocation
                # Note: myHostLocation may be reused below
            } else {
                dict set defaults -host localhost
            }
        }
        if { ! [dict exists $defaults -hostlocation]} {
            dict set defaults -hostlocation [dict get $defaults -host]
        }
    } else {

        ##
        ## No Version 3.2.x compatibility mode:
        ## -host: defaults to localhost.
        ## -hostlocation : defaults to localhost.
        ## -hostlocationserver bool: defaults to true.
        ##
        
        set defaults [dict merge \
                [dict create \
                    -hostlocation "localhost" \
                    -hostlocationserver 1 \
                    -host "localhost"] \
                $defaults]
    }
    # Compatibility has done its work, remove it:
    dict unset defaults -hostcompatibility32

    ##
    ## Update host location server to current value if specified
    ##
    
    if { [dict get $defaults -hostlocationserver] } {
        if {![info exists myHostLocation]} {
            set myHostLocation [MyHostLocationGet [dict get $defaults -ports]]
        }
        dict set defaults -hostlocation $myHostLocation
    }

    ##
    ## Resolve -hostprotocol setting.
    ## Also accept -hostProtocol
    ##
    
    if {![dict exists $defaults -hostprotocol]} {
        if {[dict exists $defaults -hostProtocol]} {
            dict set defaults -hostprotocol [dict get $defaults -hostProtocol]
        } else {
            dict set defaults -hostprotocol server
        }
    }
    dict unset defaults -hostProtocol

    ##
    ## Resolve server protocol to http and set flag for server protocol.
    ## If the flag is set, the protocol is corrected when required
    ##
    
    dict set defaults hostProtocolServer [expr {
            [dict get $defaults -hostprotocol] eq "server"}]
    if {[dict get $defaults hostProtocolServer]} {
        dict set  defaults -hostprotocol http
    }

    dict set defaults -uri $service
    namespace eval ::$service {}
    set serviceArr($service) $defaults
    if {![dict exists $procInfo $service operationList]} {
        dict set procInfo $service operationList {}
    }
    set mode [dict get $defaults -mode]

    ##
    ## Install wsdl doc
    ##
    interp alias {} ::WS::Server::generateInfo_${service} \
                 {} ::WS::Server::generateInfo ${service}
    ::log::logsubst debug {Installing Generate info for $service at [dict get $defaults -prefix]}
    switch -exact -- $mode {
        embedded {
            package require WS::Embeded
            foreach port [dict get $defaults -ports] {
                ::WS::Embeded::AddHandler $port [dict get $defaults -prefix] GET ::WS::Server::generateInfo_${service}
            }
        }
        tclhttpd {
            ::Url_PrefixInstall [dict get $defaults -prefix] ::WS::Server::generateInfo_${service}  \
                -thread 0
        }
        wub {
            package require WS::Wub
        }
        aolserver {
            package require WS::AOLserver
        }
        rivet {
            package require Rivet
        }
        wibble {
            ##
            ## Define zone handler - get code from andy
            ##
            proc ::wibble::webservice {state} {
                dict with state options {}
                switch -exact -- $suffix {
                    "" - / {
                        ::WS::Server::generateInfo $name 0 response
                        sendresponse $response
                    } /op {
                        ::WS::Server::callOperation $name 0 [dict get $state request] response
                        sendresponse $response
                    } /wsdl {
                        ::WS::Server::generateWsdl $name 0 -responsename response
                        sendresponse $response
                    }
                    default {
                        ## Do nothing
                    }
                }
            }
            if {[package present Wibble] eq "0.1"} {
                proc ::WS::Wibble::ReturnData {responseDictVar type text status} {
                    upvar 1 $responseDictVar responseDict
                    dict set responseDict header content-type $type
                    dict set responseDict content $text
                    dict set responseDict status $status
                }
            } else {
                proc ::WS::Wibble::ReturnData {responseDictVar type text status} {
                    upvar 1 $responseDictVar responseDict
                    dict set responseDict header content-type "" $type
                    dict set responseDict content $text
                    dict set responseDict status $status
                }
            }

            ::wibble::handle [dict get $defaults -prefix] webservice name $service
        }
        default {
            return \
                -code error \
                -errorcode [list WSSERVER UNSUPMODE $mode] \
                "-mode '$mode' not supported"
        }
    }


    ##
    ## Install wsdl
    ##
    interp alias {} ::WS::Server::generateWsdl_${service} \
                 {} ::WS::Server::generateWsdl ${service}
    ::log::logsubst debug {Installing GenerateWsdl info for $service at [dict get $defaults -prefix]/wsdl}
    switch -exact -- $mode {
        embedded {
            foreach port [dict get $defaults -ports] {
                ::WS::Embeded::AddHandler $port [dict get $defaults -prefix]/wsdl GET ::WS::Server::generateWsdl_${service}
            }
        }
        channel {
            package require WS::Channel
            ::WS::Channel::AddHandler [dict get $defaults -ports] {} ::WS::Server::generateWsdl_${service}
        }
        tclhttpd {
            ::Url_PrefixInstall [dict get $defaults -prefix]/wsdl ::WS::Server::generateWsdl_${service} \
                -thread 0
        }
        default {
            ## Do nothing
        }
    }

    ##
    ## Install operations
    ##
    interp alias {} ::WS::Server::callOperation_${service} \
                 {} ::WS::Server::callOperation ${service}
    ::log::logsubst debug {Installing callOperation info for $service at [dict get $defaults -prefix]/op}
    switch -exact -- $mode {
        embedded {
            foreach port [dict get $defaults -ports] {
                ::WS::Embeded::AddHandler $port [dict get $defaults -prefix]/op POST ::WS::Server::callOperation_${service}
            }
        }
        channel {
            package require WS::Channel
            ::WS::Channel::AddHandler [dict get $defaults -ports] {op} ::WS::Server::callOperation_${service}
        }
        tclhttpd {
            ::Url_PrefixInstall [dict get $defaults -prefix]/op ::WS::Server::callOperation_${service} \
                -thread 1
        }
        default {
            ## Do nothing
        }
    }

    return
}

# Get my computers IP address
proc ::WS::Server::MyHostLocationGet {ports} {
    if {[catch {
        set me [socket -server garbage_word -myaddr [info hostname] 0]
        set myHostLocation [lindex [fconfigure $me -sockname] 0]
        close $me
    } errorMessage]} {
        ::log::logsubst error {Error '$errorMessage' when querying my own IP\
                address. Use 'localhost' instead.}
        set myHostLocation "localhost"
    }
    if { 0 !=[llength $ports] && [lindex $ports 0] ni {80 433} } {
        append myHostLocation ":[lindex $ports 0]"
    }
    return $myHostLocation
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Server::ServiceProc
#
# Description : Register an operation for a service and declare the procedure to handle
#               the operations.
#
# Arguments :
#       ServiceName     -- Name of the service this operation is for
#       NameInfo        -- List of four elements:
#                               1) OperationName -- the name of the operation
#                               2) ReturnType    -- the type of the procedure return,
#                                                   this can be a simple or complex type
#                               3) Description   -- description of the return method
#                               4) Version       -- optional version for the service
#                                                   if ommitted then available in all versions
#       Arglist         -- List of argument definitions,
#                           each list element must be of the form:
#                               1) ArgumentName -- the name of the argument
#                               2) ArgumentTypeInfo -- A list of:
#                                     {type typeName comment commentString version versionCode}
#                                          typeName can be any simple or defined type.
#                                          commentString is a quoted string describing the field.
#                                          versionCode is the version which this argument is available for
#       Documentation   -- HTML describing what this operation does
#       Body            -- The tcl code to be called when the operation is invoked. . This
#                             code should return a dictionary with <OperationName>Result as a
#                             key and the operation's result as the value.
#
# Returns :     Nothing
#
# Side-Effects :
#       A procedure named "<ServiceName>::<OperationName>" defined
#       A type name with the name <OperationName>Result is defined.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Server::Server must have been called for the ServiceName
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::ServiceProc {service nameInfo arglist documentation body} {
    variable procInfo

    set name [lindex $nameInfo 0]
    set version [expr {[llength $nameInfo] > 3 ? [lindex $nameInfo 3] : {}}]
    ::log::logsubst debug {Defining operation $name for $service}
    set argOrder {}
    ::log::logsubst debug {\targs are {$arglist}}
    foreach {arg data} $arglist {
        lappend argOrder $arg
    }
    if {![dict exists $procInfo $service op$name argList]} {
        set tmpList [dict get $procInfo $service operationList]
        lappend tmpList $name
        dict set procInfo $service operationList $tmpList
    }
    dict set procInfo $service op$name argList $arglist
    dict set procInfo $service op$name argOrder $argOrder
    dict set procInfo $service op$name docs $documentation
    dict set procInfo $service op$name returnInfo [lindex $nameInfo 1]
    set typeInfo [dict get $procInfo $service op$name returnInfo]
    dict set procInfo $service op$name version $version
    ::WS::Utils::ServiceTypeDef Server $service ${name}Results [list ${name}Result $typeInfo] {} {} $version
    ::WS::Utils::ServiceTypeDef Server $service ${name}Request $arglist {} {} $version
    set tclArgList [list]
    foreach {arg typeinfo} $arglist {
        lappend tclArgList [list [lindex $arg 0] ""]
    }
    lappend tclArgList [list tclws_op_version ""]

    proc ::${service}::${name} $tclArgList $body

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::GetWsdl
#
# Description : Generates a WSDL for a registered service.
#
# Arguments :
#       serviceName     - The name of the service
#       urlPrefix       - (optional) Prefix to use for location; defaults to http://<host>
#       version         - (optional) specific version of service to generate.  If ommitted
#                             then returns the lastest
#
# Returns :
#       XML for the WSDL
#
# Side-Effects : None
#
# Exception Conditions :
#       WS SERVER UNKSERV - Unknown service name
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
#       1  07/06/2006  G.Lester     Initial version
#       2  12/12/2009  W.Kocjan     Support for services over SSL in Tclhttpd
#       3  11/13/2018  J.Cone       Version support
#   2.7.0  2020-10-26  H.Oehlmann   urlPrefix is a mandatory parameter.
#                                   The caller has to think about it.
#   3.3.0  2021-10-15  H.Oehlmann   Build default location from options
#                                   -hostprotocol and -hostlocation replacing
#                                   http and option -host.
#
#
###########################################################################
proc ::WS::Server::GetWsdl {serviceName {urlPrefix ""} {version {}}} {
    variable serviceArr
    variable procInfo

    set serviceData $serviceArr($serviceName)

    set operList {}
    foreach oper [lsort -dictionary [dict get $procInfo $serviceName operationList]] {
        if {$version ne {} && [dict exists $procInfo $serviceName op$oper version]
                && ![::WS::Utils::check_version [dict get $procInfo $serviceName op$oper version] $version]} {
            continue
        }
        lappend operList $oper
    }
    ::log::logsubst debug {Generating WSDL for $serviceName}
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        ::return \
            -code error \
            -errorCode [list WS SERVER UNKSERV $serviceName] \
            $msg
    }

    set msg {}
    set reply [::dom createDocument wsdl:definitions]
    $reply documentElement definition
    $definition setAttribute \
        xmlns:wsdl      "http://schemas.xmlsoap.org/wsdl/" \
        xmlns:http      "http://schemas.xmlsoap.org/wsdl/http/" \
        xmlns:mime      "http://schemas.xmlsoap.org/wsdl/mime/" \
        xmlns:xs         "http://www.w3.org/2001/XMLSchema" \
        xmlns:soap      "http://schemas.xmlsoap.org/wsdl/soap/" \
        xmlns:soapenc   "http://schemas.xmlsoap.org/soap/encoding/" \
        xmlns:${serviceName} "http://[dict get $serviceData -host][dict get $serviceData -prefix]" \
        targetNamespace "http://[dict get $serviceData -host][dict get $serviceData -prefix]"

    foreach topLevel {types} {
        $definition appendChild [$reply createElement wsdl:$topLevel $topLevel]
    }

    ##
    ## Messages
    ##

    ## Operations
    foreach oper $operList {
        $definition appendChild [$reply createElement wsdl:message input]
        $input setAttribute name ${oper}In
        $input appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${oper}Request
        $definition appendChild [$reply createElement wsdl:message output]
        $output setAttribute name ${oper}Out
        $output appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${oper}Results
    }

    ## Input headers
    foreach headerType [dict get $serviceData -inheaders] {
        $definition appendChild [$reply createElement wsdl:message header]
        $header setAttribute name $headerType
        $header appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${headerType}
    }

    ## Output headers
    foreach headerType [dict get $serviceData -outheaders] {
        $definition appendChild [$reply createElement wsdl:message header]
        $header setAttribute name $headerType
        $header appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${headerType}
    }

    ##
    ## Add the rest of the toplevels in
    ##
    foreach topLevel {portType binding service} {
        $definition appendChild [$reply createElement wsdl:$topLevel $topLevel]
    }

    ##
    ## Service
    ##
    $service setAttribute name $serviceName

    $service appendChild [$reply createElement wsdl:documentation documentation]
    $documentation appendChild [$reply createTextNode [dict get $serviceData -description]]

    $service appendChild [$reply createElement wsdl:port port]
    $port setAttribute \
        name ${serviceName}Soap \
        binding ${serviceName}:${serviceName}Soap

    $port appendChild [$reply createElement soap:address address]

    if {$urlPrefix == ""} {
        set urlPrefix "[dict get $serviceData -hostprotocol]://[dict get $serviceData -hostlocation]"
    }

    $address setAttribute \
        location "$urlPrefix[dict get $serviceData -prefix]/op"


    ##
    ## Bindings
    ##
    $binding setAttribute \
        name ${serviceName}Soap \
        type ${serviceName}:${serviceName}Soap
    $binding appendChild [$reply createElement soap:binding b2]
    $b2 setAttribute \
        style document \
        transport "http://schemas.xmlsoap.org/soap/http"

    foreach oper $operList {
        $binding appendChild [$reply createElement wsdl:operation operNode]
        $operNode setAttribute name $oper
        $operNode appendChild [$reply createElement soap:operation o2]
        $o2 setAttribute \
            soapAction $serviceName:$oper \
            style document
        ## Input message
        $operNode appendChild [$reply createElement wsdl:input input]
        $input appendChild [$reply createElement soap:body tmp]
        $tmp setAttribute use literal
        foreach headerType [dict get $serviceData -inheaders] {
            $operNode appendChild [$reply createElement wsdl:header header]
            $header appendChild [$reply createElement soap:header tmp]
            $tmp setAttribute \
                use literal \
                message ${serviceName}:${headerType} \
                part $headerType
        }
        ## Output message
        $operNode appendChild [$reply createElement wsdl:output output]
        $output appendChild [$reply createElement soap:body tmp]
        $tmp setAttribute use literal
        foreach headerType [dict get $serviceData -outheaders] {
            $operNode appendChild [$reply createElement wsdl:header header]
            $header appendChild [$reply createElement soap:header tmp]
            $tmp setAttribute \
                use literal \
                message ${serviceName}:${headerType} \
                part $headerType
        }
    }

    ##
    ## Ports
    ##
    $portType setAttribute name ${serviceName}Soap
    foreach oper $operList {
        $portType appendChild [$reply createElement wsdl:operation operNode]
        $operNode setAttribute name $oper
        $operNode appendChild [$reply createElement wsdl:input input]
        $input setAttribute message ${serviceName}:${oper}In
        $operNode appendChild [$reply createElement wsdl:output output]
        $output setAttribute message ${serviceName}:${oper}Out
    }

    ##
    ## Types
    ##
    GenerateScheme Server $serviceName $reply $types $version

    append msg \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$reply asXML  \
            -indent 4 \
            -escapeNonASCII \
            -doctypeDeclaration 0 \
        ]
    $reply delete
    return $msg
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateWsdl
#
# Description : Generates a WSDL for a registered service.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - Multi-function depending on server:
#                         embedded: -port number: embedded server port number
#                         unknown server: -version number: version
#                         wibble: -responsename name: response variable name
#
# Returns :
#       Embedded mode: return list of contentType, message, httpcode
#       Other modes: 1 - On error, 0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  12/12/2009  W.Kocjan     Support for services over SSL in Tclhttpd
#       3  11/13/2018  J.Cone       Version support
#   2.7.0  2020-10-26  H.Oehlmann   Support https for embedded server.
#   3.1.0  2020-11-06  H.Oehlmann   Pass port parameter from embedded server
#                                   with prefix "-port" to be compatible with
#                                   the -version parameter.
#                                   Pass wibble responsename also with prefix
#                                   "-responsename".
# 3.2.0    2021-03-17  H.Oehlmann   In embedded mode, directly return the data
# 3.3.0    2021-10-15  H.Oehlmann   Change the setting/update of the host location
#                                   by using the new parameter -hostlocationserver
#                                   and -hostlocation.
#
###########################################################################
proc ::WS::Server::generateWsdl {serviceName sock args} {
    variable serviceArr
    variable procInfo
    variable mode

    set operList [lsort -dictionary [dict get $procInfo $serviceName operationList]]
    ::log::logsubst debug {Generating WSDL for $serviceName on $sock with {$args}}
    # ToDo HaO 2020-11-06: if confirmed, args may be interpreted as a dict
    #   and the code may be simplified. It is not clear, if any server does not
    #   pass any unused data not being a dict.
    set version ""
    set argsIdx [lsearch -exact $args "-version"]
    if {$argsIdx != -1} {
        set version [lindex $args [expr {$argsIdx + 1}]]
    }
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData \
                    $sock \
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            embedded {
                return [list\
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404]
            }
            channel {
                ::WS::Channel::ReturnData \
                    $sock \
                    "text/xml; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            rivet {
                headers type "text/html; charset=UTF-8"
                headers numeric 404
                puts "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>"
            }
            aolserver {
                ::WS::AOLserver::ReturnData \
                    $sock \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            wibble  {
                upvar 1 [lindex $args 0] responseDict
                ::WS::Wibble::ReturnData responseDict text/html "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" 404
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }

    set serviceData $serviceArr($serviceName)

    ##
    ## Check if the server protocol (and host) should be used
    ##
    
    ##
    ## Prepare default URL prefix
    ##
    
    set urlPrefix "[dict get $serviceData -hostprotocol]://[dict get $serviceData -hostlocation]"

    ##
    ## Return the WSDL
    ##

    switch -exact -- $mode {
        tclhttpd {
            upvar #0 ::Httpd$sock s
            # Get protocol and host location from server if configured
            if { [dict get $serviceData hostProtocolServer] } {
                catch {
                    dict set serviceData -hostprotocol [lindex $s(self) 0]
                }
            }
            if { [dict get $serviceData -hostlocationserver] } {
                catch {
                    dict set serviceData -hostprotocol $s(mime,host)
                }
            }
            set urlPrefix [dict get $serviceData -hostprotocol]://[dict get $serviceData -hostlocation]
            set xml [GetWsdl $serviceName $urlPrefix $version]
            ::Httpd_ReturnData $sock "text/xml; charset=UTF-8" $xml 200
        }
        channel {
            set xml [GetWsdl $serviceName $urlPrefix $version]
            ::WS::Channel::ReturnData $sock "text/xml; charset=UTF-8" $xml 200
        }
        embedded {
            if {[dict get $serviceData hostProtocolServer]} {
                
                ##
                ## For https, change the URL prefix
                ##
                
                set argsIdx [lsearch -exact $args "-port"]
                if {$argsIdx == -1} {
                    ::log::logsubst error {Parameter '-port' missing from\
                            embedded server in arguments '$args'}
                } elseif {  [::WS::Embeded::GetValue isHTTPS\
                            [lindex $args [expr {$argsIdx + 1}]]]
                } {
                    dict set serviceData -hostprotocol https
                } else {
                    dict set serviceData -hostprotocol http
                }
                set urlPrefix "[dict get $serviceData -hostprotocol]://[dict get $serviceData -hostlocation]"
            }
            set xml [GetWsdl $serviceName $urlPrefix $version]
            return [list "text/xml; charset=UTF-8" $xml 200]
        }
        rivet {
            set xml [GetWsdl $serviceName $urlPrefix $version]
            headers type "text/xml; charset=UTF-8"
            headers numeric 200
            puts $xml
        }
        aolserver {
            set xml [GetWsdl $serviceName $urlPrefix $version]
            ::WS::AOLserver::ReturnData $sock text/xml $xml 200
        }
        wibble  {
            set xml [GetWsdl $serviceName $urlPrefix $version]
            set argsIdx [lsearch -exact $args "-responsename"]
            if {$argsIdx == -1} {
                ::log::logsubst error {Parameter '-responsename' missing from\
                        wibble server in arguments '$args'}
            } else {
                upvar 1 [lindex $args [expr {$argsIdx + 1}]] responseDict
                ::WS::Wibble::ReturnData responseDict text/xml $xml 200
            }
        }
        default {
            ## Do nothing
        }
    }
    return 0
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Server::GenerateScheme
#
# Description : Generate a scheme
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       doc         - The document to add the scheme to
#       parent      - The parent node of the scheme
#       version     - Service version
#
# Returns :     nothing
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
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
#       1  02/15/2008  G.Lester     Made Scheme generation a utility
#       2  02/03/2008  G.Lester     Moved scheme generation into WS::Utils namespace
#       3  11/13/2018  J.Cone       Version support
#
###########################################################################
proc ::WS::Server::GenerateScheme {mode serviceName doc parent {version {}}} {
    variable serviceArr

    set serviceData $serviceArr($serviceName)
    set targetNamespace "http://[dict get $serviceData -host][dict get $serviceData -prefix]"
    return [::WS::Utils::GenerateScheme $mode $serviceName $doc $parent $targetNamespace $version]

}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateJsonInfo
#
# Description : Generate an json description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - supports passing service version info.
#                             Eg: -version 3
#
# Returns :
#       1 - On error
#       0 - On success
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : James Sulak
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  05/16/2012  J.Sulak      Initial version
#       2  09/04/2018  A.Brooks     Send an 'example' field
#       3  11/13/2018  J.Cone       Version support
#
#
###########################################################################
# NOTE: This proc only works with Rivet
# TODO: Update to handle jsonp?
proc ::WS::Server::generateJsonInfo { serviceName sock args } {
    variable serviceArr
    variable procInfo

    ::log::logsubst debug {Generating JSON Documentation for $serviceName on $sock with {$args}}
    set version {}
    set versionIdx [lsearch -exact $args "-version"]
    if {$versionIdx != -1} {
        set version [lindex $args [expr {$versionIdx + 1}]]
    }
    set serviceData $serviceArr($serviceName)
    set doc [yajl create #auto -beautify [dict get $serviceData -beautifyJson]]

    $doc map_open

    $doc string operations array_open
    ::log::log debug "\tDisplay Operations (json)"

    foreach oper [lsort -dictionary [dict get $procInfo $serviceName operationList]] {
        # version check
        if {$version ne {} && [dict exists $procInfo $serviceName op$oper version]
                && ![::WS::Utils::check_version [dict get $procInfo $serviceName op$oper version] $version]} {
            continue
        }

        $doc map_open

        # operation name
        $doc string name string $oper

        # description
        set description [dict get $procInfo $serviceName op$oper docs]
        $doc string description string $description

        # parameters
        if {[llength [dict get $procInfo $serviceName op$oper argOrder]]} {
            $doc string inputs array_open

            foreach arg [dict get $procInfo $serviceName op$oper argOrder] {
                # version check
                if {$version ne {} && [dict exists $procInfo $serviceName op$oper argList $arg version]
                        && ![::WS::Utils::check_version [dict get $procInfo $serviceName op$oper argList $arg version] $version]} {
                    continue
                }
                ::log::logsubst debug {\t\t\tDisplaying '$arg'}
                set comment {}
                if {[dict exists $procInfo $serviceName op$oper argList $arg comment]} {
                    set comment [dict get $procInfo $serviceName op$oper argList $arg comment]
                }
                set example {}
                if {[dict exists $procInfo $serviceName op$oper argList $arg example]} {
                    set example [dict get $procInfo $serviceName op$oper argList $arg example]
                }
                set type [dict get $procInfo $serviceName op$oper argList $arg type]

                $doc map_open \
                    string name string $arg \
                    string type string $type \
                    string comment string $comment \
                    string example string $example \
                    map_close
            }

            $doc array_close
        } else {
            $doc string inputs array_open array_close
        }

        $doc string returns map_open
        set comment {}
        if {[dict exists $procInfo $serviceName op$oper returnInfo comment]} {
            set comment [dict get $procInfo $serviceName op$oper returnInfo comment]
        }
        set example {}
        if {[dict exists $procInfo $serviceName op$oper returnInfo example]} {
            set example [dict get $procInfo $serviceName op$oper returnInfo example]
        }

        set type [dict get $procInfo $serviceName op$oper returnInfo type]

        $doc string comment string $comment string type string $type string example string $example
        $doc map_close

        $doc map_close
    }

    $doc array_close

    ::log::log debug "\tDisplay custom types"
    $doc string types array_open
    set localTypeInfo [::WS::Utils::GetServiceTypeDef Server $serviceName]
    foreach type [lsort -dictionary [dict keys $localTypeInfo]] {
        # version check
        if {$version ne {} && [dict exists $localTypeInfo $type version]
                && ![::WS::Utils::check_version [dict get $localTypeInfo $type version] $version]} {
            continue
        }
        ::log::logsubst debug {\t\tDisplaying '$type'}

        $doc map_open
        $doc string name string $type
        $doc string fields array_open

        set typeDetails [dict get $localTypeInfo $type definition]
        foreach part [lsort -dictionary [dict keys $typeDetails]] {
            # version check
            if {$version ne {} && [dict exists $typeDetails $part version]
                    && ![::WS::Utils::check_version [dict get $typeDetails $part version] $version]} {
                continue
            }
            ::log::logsubst debug {\t\t\tDisplaying '$part'}
            set subType [dict get $typeDetails $part type]
            set comment {}
            if {[dict exists $typeDetails $part comment]} {
                set comment [dict get $typeDetails $part comment]
            }
            set example {}
            if {[dict exists $typeDetails $part example]} {
                set example [dict get $typeDetails $part example]
            }
            $doc map_open \
                string field string $part \
                string type  string $subType \
                string comment string $comment \
                string example string $example \
                map_close
        }

        $doc array_close
        $doc map_close
    }

    $doc array_close

    $doc map_close

    set contentType "application/json; charset=UTF-8"
    headers type $contentType
    headers numeric 200
    puts [$doc get]
    $doc delete
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - Supports passing a wibble dictionary ref or a
#                             service version.  Eg: -version 3
#
# Returns :
#       Embedded mode: return list of contentType, message, httpcode
#       Other modes: 1 - On error, 0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
# 3.2.0    2021-03-17  H.Oehlmann   In embedded mode, directly return the data
#
#
###########################################################################
proc ::WS::Server::generateInfo {serviceName sock args} {
    variable serviceArr
    variable procInfo
    variable mode

    ::log::logsubst debug {Generating HTML Documentation for $serviceName on $sock with {$args}}
    set version {}
    if {$args ne {}} {
        if {[lindex $args 0] eq {-version}} {
            set version [lindex $args 1]
        }
    }
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData \
                    $sock \
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            embedded {
                return [list\
                        "text/html; charset=UTF-8" \
                        "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                        404]
            }
            channel {
                ::WS::Channel::ReturnData \
                    $sock \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            rivet {
                headers type "text/html; charset=UTF-8"
                headers numeric 404
                puts "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>"
            }
            aolserver {
                ::WS::AOLserver::ReturnData \
                    $sock \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            wibble  {
                upvar 1 [lindex $args 0] responseDict
                ::WS::Wibble::ReturnData responseDict \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }

    set menuList {
        {List of Operations}    {#TOC}
        {Operation Details}     {#OperDetails}
        {Simple Types}          {#SimpleTypeDetails}
        {Custom Types}          {#CustomTypeDetails}
    }

    ##
    ## Display Service General Information
    ##
    append msg [generateGeneralInfo $serviceArr($serviceName) $menuList]

    ##
    ## Display TOC
    ##
    append msg [generateTocInfo $serviceArr($serviceName) $menuList $version]

    ##
    ## Display Operations
    ##
    ::log::log debug "\tDisplay Operations"
    append msg [generateOperationInfo $serviceArr($serviceName) $menuList $version]

    ##
    ## Display custom types
    ##
    ::log::log debug "\tDisplay custom types"
    append msg [generateCustomTypeInfo $serviceArr($serviceName) $menuList $version]

    ##
    ## Display list of simple types
    ##
    ::log::log debug "\tDisplay list of simply types"
    append msg [generateSimpleTypeInfo $serviceArr($serviceName) $menuList]

    ##
    ## All Done
    ##
    append msg [::html::end]
    switch -exact -- $mode {
        tclhttpd {
            ::Httpd_ReturnData $sock "text/html; charset=UTF-8" $msg 200
        }
        embedded {
            return [list "text/html; charset=UTF-8" $msg 200]
        }
        channel {
            ::WS::Channel::ReturnData $sock "text/html; charset=UTF-8" $msg 200
        }
        rivet {
            headers numeric 200
            headers type "text/html; charset=UTF-8"
            puts $msg
        }
        aolserver {
            ::WS::AOLserver::ReturnData $sock text/html $msg 200
        }
        wibble  {
                upvar 1 [lindex $args 0] responseDict
                ::WS::Wibble::ReturnData responseDict text/html $msg 200
        }
        default {
            ## Do nothing
        }
    }
    return 0
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::displayType
#
# Description : Formats the type, including a link to a complex type.
#
# Arguments :
#       serviceName     - The name of the service
#       type            - The type
#
# Returns : Formatted type information.
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::displayType {serviceName type} {
    set testType [string trimright $type {()?}]
    if {([lindex [::WS::Utils::TypeInfo Server $serviceName $testType] 0] == 0) &&
        ([info exists ::WS::Utils::simpleTypes($testType)])} {
        set result $type
    } else {
        set result [format {<a href="#type_%1$s">%2$s</a>} $testType $type]
    }
    return $result
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::callOperation
#
# Description : Process the call to an operation.  If an error occurs, a standard
#               error packet is sent, otherwise the appropriate message type
#               is sent.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       -rest           - Use Rest flavor call instead of SOAP
#       -version        - specify service version
#       args            - additional arguments and flags:
#                           -rest: choose rest flavor instead soap
#                           -version num: choose a version number
#                           -data dict: data dict in embedded mode.
#                               Used keys are: query, ipaddr, headers.
#                           first element: response dict name in wibble mode
#
# Returns :
#       Embedded mode: return list of contentType, message, httpcode
#       Other modes: 1 - On error, 0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
# 3.1.0    2020-11-06  H.Oehlmann   Get global validHttpStatusCodes in this
#                                   module, was deleted in Utilities.tcl.
# 3.2.0    2021-03-17  H.Oehlmann   Embedded mode: directly return data.
#                                   Embedded mode: directly receive data via
#                                   argument -data dict.
#                                   The result return is not protected any more
#                                   by a catch to allow direct return.
#
#
###########################################################################
proc ::WS::Server::callOperation {serviceName sock args} {
    variable procInfo
    variable serviceArr
    variable mode

    switch -exact -- $mode {
        embedded {
            # Get the data dict as argument behind call flag "-data"
            set embeddedServerDataDict [lindex $args\
                    [lsearch -exact $args "-data"]+1]
            set inXML [dict get $embeddedServerDataDict query]
        }
        wibble {
            set requestDict [lindex $args 0]
            upvar 1 [lindex $args 1] responseDict
            set inXML [dict get $requestDict post xml ""]
        }
        default {
            upvar #0 Httpd$sock data
            set inXML $data(query)
        }
    }

    # decide if SOAP or REST mode should be used.
    set flavor "soap"
    if {[lsearch -exact $args "-rest"] != -1} {
        set flavor "rest"
    }

    # check if a version number was passed
    set version {}
    set versionIdx [lsearch -exact $args "-version"]
    if {$versionIdx != -1} {
        set version [lindex $args [expr {$versionIdx + 1}]]
    }

    ::log::logsubst debug {In ::WS::Server::callOperation {$serviceName $sock $args}}
    set serviceData $serviceArr($serviceName)
    ::log::logsubst debug {\tDocument is {$inXML}}

    set ::errorInfo {}
    set ::errorCode {}
    set ns $serviceName

    set inTransform [dict get $serviceData -intransform]
    set outTransform [dict get $serviceData -outtransform]
    if {$inTransform ne {}} {
        set inXML [$inTransform REQUEST $inXML]
    }

    # Get a reference to the error callback
    set errorCallback [dict get $serviceData -errorCallback]

    ##
    ## Parse the input and determine the name of the method being invoked.
    ##
    switch -exact -- $flavor {
        rest {
            package require yajltcl ;   # only needed for rest, not soap.

            set operation [lindex $inXML 0]
            set contentType "application/json"
            set doc ""

            array set rawargs [lindex $inXML 1]
            if {[info exists rawargs(jsonp_callback)]} {
                if {![regexp {^[a-zA-Z_0-9]+$} $rawargs(jsonp_callback)]} {
                    # sanitize the JSONP callback function name for security.
                    set rawargs(jsonp_callback) JsonpCallback
                }
                set contentType "text/javascript"
            }
        }
        soap {
            # skip any XML header
            set first [string first {<} $inXML]
            if {$first > 0} {
                set inXML [string range $inXML $first end]
            }
            # parse the XML request
            dom parse $inXML doc
            $doc documentElement top
            ::log::logsubst debug {$doc selectNodesNamespaces \
                    [list ENV http://schemas.xmlsoap.org/soap/envelope/ \
                    $serviceName http://[dict get $serviceData -host][dict get $serviceData -prefix]]}
            $doc selectNodesNamespaces \
                [list ENV http://schemas.xmlsoap.org/soap/envelope/ \
                     $serviceName http://[dict get $serviceData -host][dict get $serviceData -prefix]]
            $doc documentElement rootNode

            # extract the name of the method
            set top [$rootNode selectNodes /ENV:Envelope/ENV:Body/*]
            catch {$top localName} requestMessage
            set legacyRpcMode 0
            if {$requestMessage == ""} {
                # older RPC/Encoded clients need to try nodeName instead.
                # Python pySoap needs this.
                catch {$top nodeName} requestMessage
                set legacyRpcMode 1
            }
            ::log::logsubst debug {requestMessage = {$requestMessage} legacyRpcMode=$legacyRpcMode}
            if {[string match {*Request} $requestMessage]} {
                set operation [string range $requestMessage 0 end-7]
            } else {
                # broken clients might not have sent the correct Document Wrapped name.
                # Python pySoap and Perl SOAP::Lite need this.
                set operation $requestMessage
                set legacyRpcMode 1
            }
            ::log::logsubst debug {operation = '$operation' legacyRpcMode=$legacyRpcMode}
            set contentType "text/xml"
        }
        default {
            if {$errorCallback ne {}} { $errorCallback "BAD_FLAVOR No supported protocol" {} "UnknownMethod" $flavor }
            error "bad flavor"
        }
    }

    ##
    ## Check that the method exists and version checks out.
    ##
    if {![dict exists $procInfo $serviceName op$operation argList] ||
            ([dict exists $procInfo $serviceName op$operation version] &&
            ![::WS::Utils::check_version [dict get $procInfo $serviceName op$operation version] $version])} {
        set msg "Method $operation not found"
        ::log::log error $msg
        set ::errorInfo {}
        set ::errorCode [list Server UNKNOWN_METHOD $operation]
        set response [generateError \
                    [dict get $serviceData -traceEnabled] \
                    CLIENT \
                    $msg \
                    [list "errorCode" $::errorCode "stackTrace" $::errorInfo] \
                    $flavor]
        catch {$doc delete}
        set httpStatus 404
        if {$errorCallback ne {}} { $errorCallback "UNKNOWN_METHOD $msg" httpStatus $operation $flavor }
        ::log::logsubst debug {Leaving @ error 1::WS::Server::callOperation $response}

        # wrap in JSONP
        if {$flavor == "rest" && [info exists rawargs(jsonp_callback)]} {
            set response "$rawargs(jsonp_callback)($response)"
        }

        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            embedded {
                return [list "$contentType; charset=UTF-8" $response $httpStatus]
            }
            rivet {
                headers type "$contentType; charset=UTF-8"
                headers numeric $httpStatus
                puts $response
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict "$contentType; charset=UTF-8" $response $httpStatus
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }
    set baseName $operation
    set cmdName op$baseName
    set methodName "${ns}::$baseName"

    ##
    ## Parse the arguments for the method.
    ##
    set argInfo [dict get $procInfo $ns $cmdName argList]
    if {[catch {
        # Check that all supplied arguments are valid
        set methodArgs {}
        foreach arg [dict get $procInfo $ns $cmdName argOrder] {
            lappend methodArgs $arg
        }
        if {$flavor eq "rest"} {
            lappend methodArgs jsonp_callback "_"
        }
        if {[dict get $serviceData -verifyUserArgs]} {
            foreach {key value} [array get rawargs] {
                if {[lsearch -exact $methodArgs $key] == -1} {
                    error "Invalid argument '$key' supplied"
                }
            }
        }
        switch -exact -- $flavor {
            rest {
                set tclArgList {}
                foreach argName $methodArgs {
                    if {$argName eq "jsonp_callback" || $argName eq "_"} {
                        continue
                    }
                    set argType [string trim [dict get $argInfo $argName type]]
                    set typeInfoList [::WS::Utils::TypeInfo Server $serviceName $argType]

                    if {[dict exists $procInfo $ns $cmdName argList $argName version] &&
                        ![::WS::Utils::check_version [dict get $procInfo $ns $cmdName argList $argName version] $version]} {
                        ::log::log debug "user supplied argument not supported in the requested version"
                        lappend tclArgList {}
                        continue
                    }
                    if {![info exists rawargs($argName)]} {
                        ::log::logsubst debug {did not find argument for $argName, leaving blank}
                        lappend tclArgList {}
                        continue
                    }

                    switch -exact -- $typeInfoList {
                        {0 0} {
                            ## Simple non-array
                            lappend tclArgList $rawargs($argName)
                        }
                        {0 1} {
                            ## Simple array
                            lappend tclArgList $rawargs($argName)
                        }
                        {1 0} {
                            ## Non-simple non-array
                            error "TODO JSON"
                            #lappend tclArgList [::WS::Utils::convertTypeToDict Server $serviceName $node $argType $top]
                        }
                        {1 1} {
                            ## Non-simple array
                            error "TODO JSON"
                            #set tmp {}
                            #set argType [string trimright $argType {()?}]
                            #foreach row $node {
                            #    lappend tmp [::WS::Utils::convertTypeToDict Server $serviceName $row $argType $top]
                            #}
                            #lappend tclArgList $tmp
                        }
                        default {
                            ## Do nothing
                        }
                    }

                }
            }
            soap {
                foreach pass [list 1 2 3] {
                    set tclArgList {}
                    set gotAnyArgs 0
                    set argIndex 0
                    foreach argName $methodArgs {
                        set argType [string trim [dict get $argInfo $argName type]]
                        set typeInfoList [::WS::Utils::TypeInfo Server $serviceName $argType]
                        if {$pass == 1} {
                            # access arguments by name using full namespace
                            set path $serviceName:$argName
                            set node [$top selectNodes $path]
                        } elseif {$pass == 2} {
                            # legacyRpcMode only, access arguments by unqualified name
                            set path $argName
                            set node [$top selectNodes $path]
                        } else {
                            # legacyRpcMode only, access arguments by index
                            set path "legacy argument index $argIndex"
                            set node [lindex [$top childNodes] $argIndex]
                            incr argIndex
                        }
                        if {[dict exists $procInfo $ns $cmdName argList $argName version] &&
                            ![::WS::Utils::check_version [dict get $procInfo $ns $cmdName argList $argName version] $version]} {
                            ::log::log debug "user supplied argument not supported in the requested version"
                            lappend tclArgList {}
                            continue
                        }
                        if {[string equal $node {}]} {
                            ::log::logsubst debug {did not find argument for $argName using $path, leaving blank (pass $pass)}
                            lappend tclArgList {}
                            continue
                        }
                        ::log::logsubst debug {found argument $argName using $path, processing $node}
                        set gotAnyArgs 1
                        switch -exact -- $typeInfoList {
                            {0 0} {
                                ## Simple non-array
                                lappend tclArgList [$node asText]
                            }
                            {0 1} {
                                ## Simple array
                                set tmp {}
                                foreach row $node {
                                    lappend tmp [$row asText]
                                }
                                lappend tclArgList $tmp
                            }
                            {1 0} {
                                ## Non-simple non-array
                                set argType [string trimright $argType {?}]
                                lappend tclArgList [::WS::Utils::convertTypeToDict Server $serviceName $node $argType $top]
                            }
                            {1 1} {
                                ## Non-simple array
                                set tmp {}
                                set argType [string trimright $argType {()?}]
                                foreach row $node {
                                    lappend tmp [::WS::Utils::convertTypeToDict Server $serviceName $row $argType $top]
                                }
                                lappend tclArgList $tmp
                            }
                            default {
                                ## Do nothing
                            }
                        }
                    }
                    ::log::logsubst debug {gotAnyArgs $gotAnyArgs, legacyRpcMode $legacyRpcMode}
                    if {$gotAnyArgs || !$legacyRpcMode} break
                }
            }
            default {
                if {$errorCallback ne {}} { $errorCallback "BAD_FLAVOR No supported protocol" {} $operation $flavor }
                error "invalid flavor"
            }
        }
        # If the user passed a version, pass that along
        if {$version ne {}} {
            lappend tclArgList $version
        }

        ::log::logsubst debug {finalargs $tclArgList}
    } errMsg]} {
        ::log::log error $errMsg
        set localerrorCode $::errorCode
        set localerrorInfo $::errorInfo
        set response [generateError \
                    [dict get $serviceData -traceEnabled] \
                    CLIENT \
                    "Error Parsing Arguments -- $errMsg" \
                    [list "errorCode" $localerrorCode "stackTrace" $localerrorInfo] \
                    $flavor]
        catch {$doc delete}
        set httpStatus 400
        if {$errorCallback ne {}} { $errorCallback "INVALID_ARGUMENT $errMsg" httpStatus $operation $flavor }
        ::log::logsubst debug {Leaving @ error 3::WS::Server::callOperation $response}

        # wrap in JSONP
        if {$flavor == "rest" && [info exists rawargs(jsonp_callback)]} {
            set response "$rawargs(jsonp_callback)($response)"
        }

        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            embedded {
                return [list "$contentType; charset=UTF-8" $response $httpStatus]
            }
            channel {
                ::WS::Channel::ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            rivet {
                headers type "$contentType; charset=UTF-8"
                headers numeric $httpStatus
                puts $response
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict "$contentType; charset=UTF-8" $response $httpStatus
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }

    ##
    ## Run the premonitor hook, if necessary.
    ##
    if {[dict exists $serviceData -premonitor] && "" ne [dict get $serviceData -premonitor]} {
        set precmd [dict get $serviceData -premonitor]
        lappend precmd PRE $serviceName $operation $tclArgList
        catch $precmd
    }

    ##
    ## Convert the HTTP request headers.
    ##
    set headerList {}
    foreach headerType [dict get $serviceData -inheaders] {
        if {[string equal $headerType {}]} {
            continue
        }
        foreach node [$top selectNodes data:$headerType] {
            lappend headerList [::WS::Utils::convertTypeToDict Server $serviceName $node $headerType $top]
        }
    }

    ##
    ## Actually execute the method.
    ##
    if {[catch {
        set cmd [dict get $serviceData -checkheader]
        if {$cmd ne ""} {
            switch -exact -- $mode {
                wibble  {
                    lappend cmd \
                        $ns \
                        $baseName \
                        [dict get $requestDict peerhost] \
                        [dict keys [dict get $requestDict header]] \
                        $headerList
                }
                embedded {
                    lappend cmd $ns $baseName [dict get $embeddedServerDataDict ipaddr] [dict get $embeddedServerDataDict headers] $headerList
                }
                default {
                    lappend cmd $ns $baseName $data(ipaddr) $data(headerlist) $headerList
                }
            }
            # This will return with error, if the header is not ok
            eval $cmd
        }
        set results [eval \$methodName $tclArgList]
        # generate a reply packet
        set response [generateReply $ns $baseName $results $flavor]

        # wrap in JSONP
        if {$flavor == "rest" && [info exists rawargs(jsonp_callback)]} {
            set response "$rawargs(jsonp_callback)($response)"
        }

        # mangle the XML declaration
        if {$flavor == "soap"} {
            # regsub "<!DOCTYPE\[^>\]+>\n" $response {} response
            set response [string map {{<?xml version="1.0"?>} {<?xml version="1.0"  encoding="utf-8"?>}} $response]
        }

        catch {$doc delete}
        if {![string equal $outTransform  {}]} {
            set response [$outTransform REPLY $response $operation $results]
        }
        if {[dict exists $serviceData -postmonitor] &&
            "" ne [dict get $serviceData -postmonitor]} {
            set precmd [dict get $serviceData -postmonitor]
            lappend precmd POST $serviceName $operation OK $results
            catch $precmd
        }
    } msg]} {
        ##
        ## Handle errors
        ##
        set localerrorCode $::errorCode
        set localerrorInfo $::errorInfo
        if {[dict exists $serviceData -postmonitor] &&
            "" ne [dict get $serviceData -postmonitor]} {
            set precmd [dict get $serviceData -postmonitor]
            lappend precmd POST $serviceName $operation ERROR $msg
            catch $precmd
        }
        catch {$doc delete}
        set httpStatus 500
        if {$errorCallback ne {}} { $errorCallback $msg httpStatus $operation $flavor }
        set response [generateError \
                    [dict get $serviceData -traceEnabled] \
                    CLIENT \
                    $msg \
                    [list "errorCode" $localerrorCode "stackTrace" $localerrorInfo] \
                    $flavor]
        ::log::logsubst debug {Leaving @ error 2::WS::Server::callOperation $response}

        # Check if the localerrorCode is a valid http status code
        set validHttpStatusCodes {100 101 102
                200 201 202 203 204 205 206 207 208 226
                300 301 302 303 304 305 306 307 308
                400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 421 422 423 424 425 426 427 428 429 430 431 451
                500 501 502 503 504 505 506 507 508 509 510 511
            }
        if {[lsearch -exact $validHttpStatusCodes $localerrorCode] > -1} {
            set httpStatus $localerrorCode
        }

        # wrap in JSONP
        if {$flavor == "rest" && [info exists rawargs(jsonp_callback)]} {
            set response "$rawargs(jsonp_callback)($response)"
        }

        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            embedded {
                return [list "$contentType; charset=UTF-8" $response $httpStatus]
            }
            channel {
                ::WS::Channel::ReturnData $sock "$contentType; charset=UTF-8" $response $httpStatus
            }
            rivet {
                if {[lindex $localerrorCode 0] == "RIVET" && [lindex $localerrorCode 1] == "ABORTPAGE"} {
                    # if we caught an abort_page, then re-trigger it.
                    abort_page
                }
                headers type "$contentType; charset=UTF-8"
                headers numeric $httpStatus
                puts $response
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock $contentType $response $httpStatus
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict "$contentType; charset=UTF-8" $response $httpStatus
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }
    
    ##
    ## Return result
    ##

    ::log::logsubst debug {Leaving ::WS::Server::callOperation $response}
    switch -exact -- $mode {
        tclhttpd {
            ::Httpd_ReturnData $sock "$contentType; charset=UTF-8" $response 200
        }
        embedded {
            return [list "$contentType; charset=UTF-8" $response 200]
        }
        channel {
            ::WS::Channel::ReturnData $sock "$contentType; charset=UTF-8" $response 200
        }
        rivet {
            headers type "$contentType; charset=UTF-8"
            headers numeric 200
            puts $response
        }
        aolserver {
            ::WS::AOLserver::ReturnData $sock "$contentType; charset=UTF-8" $response 200
        }
        wibble  {
            ::WS::Wibble::ReturnData responseDict "$contentType; charset=UTF-8" $response 200
        }
        default {
            ## Do nothing
        }
    }
    
    return 0
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateError
#
# Description : Generate a standard error packet
#
# Arguments :
#    includeTrace       - Boolean indicate if the trace is to be included.
#    faultcode          - The code describing the error
#    faultstring        - The string describing the error.
#    detail             - Optional details of error.
#    flavor             - Output mode: "soap" or "rest"
#
# Returns : XML formatted standard error packet
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateError {includeTrace faultcode faultstring detail flavor} {
    ::log::logsubst debug {Entering ::WS::Server::generateError $faultcode $faultstring {$detail}}
    set code [lindex $detail 1]
    switch -exact -- $code {
        "VersionMismatch" {
            set code "SOAP-ENV:VersionMismatch"
        }
        "MustUnderstand" {
            set code "SOAP-ENV:MustUnderstand"
        }
        "Client" {
            set code "SOAP-ENV:Client"
        }
        "Server" {
            set code "SOAP-ENV:Server"
        }
        default {
            ## Do nothing
        }
    }

    switch -exact -- $flavor {
        rest {
            set doc [yajl create #auto]
            $doc map_open string "error" string $faultstring map_close
            set response [$doc get]
            $doc delete
        }
        soap {
            dom createDocument "SOAP-ENV:Envelope" doc
            $doc documentElement env
            $env setAttribute  \
                "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
                "xmlns:xsi"      "http://www.w3.org/1999/XMLSchema-instance" \
                "xmlns:xsd"      "http://www.w3.org/1999/XMLSchema" \
                "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/"
            $env appendChild [$doc createElement "SOAP-ENV:Body" bod]
            $bod appendChild [$doc createElement "SOAP-ENV:Fault" flt]
            $flt appendChild [$doc createElement "faultcode" fcd]
            $fcd appendChild [$doc createTextNode $faultcode]
            $flt appendChild [$doc createElement "faultstring" fst]
            $fst appendChild [$doc createTextNode $faultstring]

            if { $detail != {} } {
                $flt appendChild [$doc createElement "SOAP-ENV:detail" dtl0]
                $dtl0 appendChild [$doc createElement "e:errorInfo" dtl]
                $dtl setAttribute "xmlns:e" "urn:TclErrorInfo"

                foreach {detailName detailInfo} $detail {
                    if {!$includeTrace && $detailName == "stackTrace"} {
                        continue
                    }
                    $dtl appendChild [$doc createElement $detailName err]
                    $err appendChild [$doc createTextNode $detailInfo]
                }
            }

            # serialize the DOM document and return the XML text
            append response \
                {<?xml version="1.0"  encoding="utf-8"?>} \
                "\n" \
                [$doc asXML -indent none -doctypeDeclaration 0]
            $doc delete
        }
        default {
            error "unsupported flavor"
        }
    }
    ::log::logsubst debug {Leaving (error) ::WS::Server::generateError $response}
    return $response
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : :WS::Server::generateReply
#
# Description : Generate the reply packet for an operation
#
# Arguments :
#    serviceName         - The name of the service
#    operation           - The name of the operation
#    results             - The results as a dictionary object
#    flavor              - Output mode: "soap" or "rest"
#    version             - Requested service version
#
#
# Returns : The results as an XML formatted packet.
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
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Server::generateReply {serviceName operation results flavor {version {}}} {
    ::log::logsubst debug {Entering ::WS::Server::generateReply $serviceName $operation {$results}}

    variable serviceArr

    set serviceData $serviceArr($serviceName)


    switch -exact -- $flavor {
        rest {
            set doc [yajl create #auto -beautify [dict get $serviceData -beautifyJson]]

            $doc map_open
            ::WS::Utils::convertDictToJson Server $serviceName $doc $results ${serviceName}:${operation}Results [dict get $serviceData -enforceRequired] $version
            $doc map_close

            set output [$doc get]
            $doc delete
        }
        soap {
            if {[info exists ::Config(docRoot)] && [file exists [file join $::Config(docRoot) $serviceName $operation.css]]} {
                # *** Bug: the -hostprotocol in -hostprotocol=server is not
                # *** corrected, if no WSDL is required before this call.
                set replaceText [format {<?xml-stylesheet type="text/xsl" href="%s://%s/css/%s/%s.css"?>}\
                                     [dict get $serviceData -hostprotocol] \
                                     [dict get $serviceData -hostlocation] \
                                     $serviceName \
                                     $operation]
                append replaceText "\n"
            } else {
                set replaceText {}
            }

            dom createDocument "SOAP-ENV:Envelope" doc
            $doc documentElement env
            $env setAttribute  \
                "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
                "xmlns:xsi"      "http://www.w3.org/1999/XMLSchema-instance" \
                "xmlns:xsd"      "http://www.w3.org/1999/XMLSchema" \
                "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/" \
                xmlns:$serviceName "http://[dict get $serviceData -host][dict get $serviceData -prefix]"
            if {0 != [llength [dict get $serviceData -outheaders]]} {
                $env appendChild [$doc createElement "SOAP-ENV:Header" header]
                foreach headerType [dict get $serviceData -outheaders] {
                    #$header appendChild [$doc createElement ${serviceName}:${headerType} part]
                    #::WS::Utils::convertDictToType Server $serviceName $doc $part $results $headerType
                    ::WS::Utils::convertDictToType Server $serviceName $doc $header $results $headerType 0 [dict get $serviceData -enforceRequired] $version
                }
            }
            $env appendChild [$doc createElement "SOAP-ENV:Body" body]
            $body appendChild [$doc createElement ${serviceName}:${operation}Results reply]

            ::WS::Utils::convertDictToType Server $serviceName $doc $reply $results ${serviceName}:${operation}Results 0 [dict get $serviceData -enforceRequired] $version

            append output \
                {<?xml version="1.0"  encoding="utf-8"?>} \
                "\n" \
                [$doc asXML -indent none -doctypeDeclaration 0]
            #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] $replaceText xml
            $doc delete
        }
        default {
            error "Unsupported flavor"
        }
    }

    ::log::logsubst debug {Leaving ::WS::Server::generateReply $output}
    return $output

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceData      -- Service information dict
#       menuList         -- html menu
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateGeneralInfo {serviceData menuList} {
    variable procInfo

    ::log::log debug "\tDisplay Service General Information"
    set service [dict get $serviceData -service]

    ::html::init
    ::html::author [dict get $serviceData -author]
    if {"" eq [dict get $serviceData -description]} {
        ::html::description  "Automatically generated human readable documentation for '$service'"
    } else {
        ::html::description [dict get $serviceData -description]
    }
    if {[dict get $serviceData -stylesheet] ne ""} {
        ::html::headTag "link rel=\"stylesheet\" type=\"text/css\" href=\"[dict get $serviceData -stylesheet]\""
    }
    set head [dict get $serviceData -htmlhead]
    set msg [::html::head $head]
    append msg [::html::bodyTag]

    dict unset serviceData -service
    if {[dict exists $serviceData -description]} {
        dict set serviceData -description [::html::nl2br [dict get $serviceData -description]]
    }
    set wsdl [format {<a href="%s/%s">WSDL(xml)</a>} [dict get $serviceData -prefix] wsdl]
    append msg [::html::openTag center] [::html::h1 "$head -- $wsdl"] [::html::closeTag] \
               [::html::openTag table {border="2"}]

    foreach key [lsort -dictionary [dict keys $serviceData]] {
        if {"" eq [dict get $serviceData $key]} {
            append msg [::html::row [string range $key 1 end] {<i>N/A</i>}]
        } else {
            append msg [::html::row [string range $key 1 end] [dict get $serviceData $key]]
        }
    }
    append msg [::html::closeTag] \
               "\n<hr/>\n"

    return $msg
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceData      -- Service option dict
#       menuList         -- html menu
#       version         - Requested service version
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Server::generateTocInfo {serviceData menuList version} {
    variable procInfo

    ##
    ## Display TOC
    ##
    ::log::log debug "\tTOC"
    set service [dict get $serviceData -service]
    append msg [::html::h2 {<a id='TOC'>List of Operations</a>}]

    set operList {}
    foreach oper [lsort -dictionary [dict get $procInfo $service operationList]] {
        if {$version ne {} && [dict exists $procInfo $service op$oper version]} {
            if {![::WS::Utils::check_version [dict get $procInfo $service op$oper version] $version]} {
                ::log::log debug "Skipping operation '$oper' because version is incompatible"
                continue
            }
        }
        lappend operList $oper "#op_$oper"
    }
    append msg [::html::minorList $operList]

    append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
    append msg "\n<hr/>\n"

    return $msg
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceData      -- Service option dict
#       menuList         -- html menu
#       version         - Requested service version
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Server::generateOperationInfo {serviceData menuList version} {
    variable procInfo

    ##
    ## Display Operations
    ##
    ::log::log debug "\tDisplay Operations"
    set service [dict get $serviceData -service]
    set operList {}
    foreach oper [lsort -dictionary [dict get $procInfo $service operationList]] {
        lappend operList $oper "#op_$oper"
    }
    append msg [::html::h2 {<a id='OperDetails'>Operation Details</a>}]

    set docFormat [dict get $serviceData -docFormat]
    set opCount 0
    foreach {oper anchor} $operList {
        if {$version ne {} && [dict exists $procInfo $service op$oper version]} {
            if {![::WS::Utils::check_version [dict get $procInfo $service op$oper version] $version]} {
                ::log::log debug "Skipping operation '$oper' because version is incompatible"
                continue
            }
        }
        ::log::logsubst debug {\t\tDisplaying '$oper'}
        incr opCount
        append msg [::html::h3 "<a id='op_$oper'>$oper</a>"]

        append msg [::html::h4 {Description}] "\n"

        append msg [::html::openTag div {style="margin-left: 40px;"}]
        switch -exact -- $docFormat {
            "html" {
                append msg [dict get $procInfo $service op$oper docs]
            }
            "text" -
            default {
                append msg [::html::nl2br [::html::html_entities [dict get $procInfo $service op$oper docs]]]
            }
        }
        append msg [::html::closeTag]

        append msg "\n"

        append msg [::html::h4 {Inputs}] "\n"

        append msg [::html::openTag div {style="margin-left: 40px;"}]

        set inputCount 0
        if {[llength [dict get $procInfo $service op$oper argOrder]]} {
            foreach arg [dict get $procInfo $service op$oper argOrder] {
                if {$version ne {} && [dict exists $procInfo $service op$oper argList $arg version]} {
                    if {![::WS::Utils::check_version [dict get $procInfo $service op$oper argList $arg version] $version]} {
                        ::log::log debug "Skipping field '$arg' because version is incompatible"
                        continue
                    }
                }
                ::log::logsubst debug {\t\t\tDisplaying '$arg'}
                if {!$inputCount} {
                    append msg [::html::openTag {table} {border="2"}]
                    append msg [::html::hdrRow Name Type Description]
                }
                incr inputCount
                if {[dict exists $procInfo $service op$oper argList $arg comment]} {
                    set comment [dict get $procInfo $service op$oper argList $arg comment]
                } else {
                    set comment {}
                }
                append msg [::html::row \
                                $arg \
                                [displayType $service [dict get $procInfo $service op$oper argList $arg type]] \
                                $comment \
                               ]
            }
            if {$inputCount} {
                append msg [::html::closeTag]
            }
        }
        if {!$inputCount} {
            append msg "No inputs."
        }
        append msg [::html::closeTag]

        ::log::log debug "\t\tReturns"
        append msg [::html::h4 {Returns}] "\n"

        append msg [::html::openTag div {style="margin-left: 40px;"}]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Type Description]
        if {[dict exists $procInfo $service op$oper returnInfo comment]} {
            set comment [dict get $procInfo $service op$oper returnInfo comment]
        } else {
            set comment {}
        }
        append msg [::html::row \
                        [displayType $service [dict get $procInfo $service op$oper returnInfo type]] \
                        $comment \
                   ]
        append msg [::html::closeTag]
        append msg [::html::closeTag]

        append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
        append msg "\n<hr/>\n"
    }

    if {!$opCount} {
        append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
        append msg "\n<hr/>\n"
    }

    return $msg

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceData      -- Service option dict
#       menuList         -- html menu
#       version         - Requested service version
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Server::generateCustomTypeInfo {serviceData menuList version} {
    variable procInfo

    ##
    ## Display custom types
    ##
    ::log::log debug "\tDisplay custom types"
    set service [dict get $serviceData -service]
    append msg [::html::h2 {<a id='CustomTypeDetails'>Custom Types</a>}]

    set localTypeInfo [::WS::Utils::GetServiceTypeDef Server $service]
    foreach type [lsort -dictionary [dict keys $localTypeInfo]] {
        if {$version ne {} && [dict exists $localTypeInfo $type version]} {
            set typeVersion [dict get $localTypeInfo $type version]
            if {![::WS::Utils::check_version $typeVersion $version]} {
                ::log::log debug "Skipping type '$type' because version is incompatible"
                continue
            }
        }
        ::log::logsubst debug {\t\tDisplaying '$type'}
        set href_type [lindex [split $type :] end]
        set typeOverloadArray($type) 1
        append msg [::html::h3 "<a id='type_${href_type}'>$type</a>"]
        set typeDetails [dict get $localTypeInfo $type definition]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Field Type Comment]
        foreach part [lsort -dictionary [dict keys $typeDetails]] {
            if {$version ne {} && [dict exists $typeDetails $part version]} {
                if {![::WS::Utils::check_version [dict get $typeDetails $part version] $version]} {
                    ::log::log debug "Skipping field '$part' because version is incompatible"
                    continue
                }
            }
            ::log::logsubst debug {\t\t\tDisplaying '$part'}
            if {[dict exists $typeDetails $part comment]} {
                set comment [dict get $typeDetails $part comment]
            } else {
                set comment {}
            }
            append msg [::html::row \
                            $part \
                            [displayType $service [dict get $typeDetails $part type]] \
                            $comment \
                       ]
        }
        append msg [::html::closeTag]
    }

    append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
    append msg "\n<hr/>\n"

    return $msg
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateSimpleTypeInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceData      -- Service option dict
#       menuList         -- html menu
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateSimpleTypeInfo {serviceData menuList} {
    variable procInfo

    ##
    ## Display list of simple types
    ##
    ::log::log debug "\tDisplay list of simply types"
    set service [dict get $serviceData -service]
    append msg [::html::h2 {<a id='SimpleTypeDetails'>Simple Types</a>}]

    append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
    set localTypeInfo [::WS::Utils::GetServiceSimpleTypeDef Server $service]
    foreach typeDetails [lsort -dictionary -index 0 $localTypeInfo] {
        set type [lindex $typeDetails 0]
        ::log::logsubst debug {\t\tDisplaying '$type'}
        set typeOverloadArray($type) 1
        append msg [::html::h3 "<a id='type_$type'>$type</a>"]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Attribute Value]
        foreach part [lsort -dictionary [dict keys [lindex $typeDetails 1]]] {
            ::log::logsubst debug {\t\t\tDisplaying '$part'}
            append msg [::html::row \
                            $part \
                            [dict get [lindex $typeDetails 1] $part] \
                       ]
        }
        append msg [::html::closeTag]
    }
    append msg "\n<hr/>\n"

    return $msg
}
