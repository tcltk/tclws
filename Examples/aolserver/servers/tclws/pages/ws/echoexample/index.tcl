
package require WS::AOLserver

::WS::AOLserver::Init

::WS::Server::Service \
    -mode aolserver \
    -prefix $prefix \
    -service $service \
    -description  {Tcl Example Web Services} \
    -host         $host \
    -ports        $port

##
## Define any special types
##
::WS::Utils::ServiceTypeDef Server $service echoReply {
    echoBack     {type string}
    echoTS       {type dateTime}
}

##
## Define the operations available
##
::WS::Server::ServiceProc \
    $service \
    {SimpleEcho {type string comment {Requested Echo}}} \
    {
	TestString      {type string comment {The text to echo back}}
    } \
    {Echo a string back} {
	return [list SimpleEchoResult $TestString]
}


::WS::Server::ServiceProc \
    $service \
    {ComplexEcho {type echoReply comment {Requested Echo -- text and timestamp}}} \
    {
	TestString      {type string comment {The text to echo back}}
    } \
    {Echo a string and a timestamp back} {
	set timeStamp [clock format [clock seconds] -format {%Y-%m-%dT%H:%M:%SZ} -gmt yes]
	return [list ComplexEchoResult [list echoBack $TestString echoTS $timeStamp]  ]
}

::WS::AOLserver::Return