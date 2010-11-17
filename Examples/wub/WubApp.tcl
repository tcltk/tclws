#! /usr/bin/env tclsh
lappend ::auto_path [file dirname [info script]]
lappend ::auto_path ../Wub/Wub/	;# add Wub's directory to auto_path
package require WS::Server

##
## Define the service
##
::WS::Server::Service -mode wub \
    -htmlhead {Wub Based Web Services} \
    -service wsEchoExample \
    -description {Echo Example - Tcl Web Services} \
    -ports 8080

##
## Define any special types
##
::WS::Utils::ServiceTypeDef Server wsEchoExample echoReply {
    echoBack     {type string}
    echoTS       {type dateTime}
}

##
## Define the operations available
##
::WS::Server::ServiceProc \
    wsEchoExample \
    {SimpleEcho {type string comment {Requested Echo}}} \
    {
        TestString      {type string comment {The text to echo back}}
    } \
    {Echo a string back} {

    return [list SimpleEchoResult $TestString]
}

::WS::Server::ServiceProc \
    wsEchoExample \
    {ComplexEcho {type echoReply comment {Requested Echo -- text and timestamp}}} \
    {
        TestString      {type string comment {The text to echo back}}
    } \
    {Echo a string and a timestamp back} {

    set timeStamp [clock format [clock seconds] -format {%Y-%m-%dT%H:%M:%SZ} -gmt yes]
    return [list ComplexEchoResult [list echoBack $TestString echoTS $timeStamp]  ]
}

#### Wub specific application start
package require Site		;# load up the site

# Initialize Site
Site init home [file normalize [file dirname [info script]]] ini site.ini debug 10

# this defines the mapping from URL to Wsdl interface objects
package require Nub
package require WS::Wub
Nub domain /service/wsEchoExample Wsdl -service wsEchoExample
Nub domain /service/wsEchoExample2 Wsdl -service wsEchoExample	;# you can have multiple Wsdl instances

# Start Site Server(s)
Site start
