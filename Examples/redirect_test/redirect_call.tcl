# Call redirect server
# 2015-11-09 Harald Oehlmann
# Start the redirect_server.tcl and the embedded echo sample to test.
set auto_path [linsert $auto_path 0 [file join [file dirname [info script]] ../..]]
package require WS::Utils
package require WS::Client
catch {console show}
::log::lvSuppressLE debug 0
::WS::Client::GetAndParseWsdl http://localhost:8014/service/wsEchoExample/wsdl
