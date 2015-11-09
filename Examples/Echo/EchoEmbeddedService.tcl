###############################################################################
##                                                                           ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
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

set auto_path [linsert $auto_path 0 [file join [file dirname [info script]] ../..]]
package require WS::Server
package require WS::Utils
package require WS::Embeded
catch {console show}

##
## Define the service
##
::WS::Server::Service \
    -service wsEchoExample \
    -description  {Echo Example - Tcl Web Services} \
    -host         localhost:8015 \
    -mode         embedded \
    -ports        [list 8015]

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
::log::lvSuppressLE debug 0
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

set ::errorInfo {}
set SocketHandle [::WS::Embeded::Listen 8015]
set ::errorInfo {}

proc x {} {
    close $::SocketHandle
    exit
}

puts stdout {Server started. Press x and Enter to stop}
flush stdout
fileevent stdin readable {set QuitNow 1}
vwait QuitNow
x
