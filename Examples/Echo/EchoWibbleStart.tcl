############# tclws.tcl, start script for wibble 24 (not included) ########
# Adjust auto_path to your needs
lappend auto_path [file dir [info script]] lib
source wibble.tcl
# Set the root directory.
set root html
set ::Config(host) 127.0.0.1
set ::Config(port) 8015

source EchoWebService.tcl

# Define zone handlers.
::wibble::handle /vars vars
::wibble::handle / dirslash root $root
::wibble::handle / indexfile root $root indexfile index.html
::wibble::handle / static root $root
::wibble::handle / template root $root
::wibble::handle / script root $root
::wibble::handle / dirlist root $root
::wibble::handle / notfound


# Start a server and enter the event loop.
catch {
  ::wibble::listen 8015
  vwait forever
}
