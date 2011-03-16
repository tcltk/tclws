The file EchoWebService.tcl defines a service and should be sourced in by the web services server.  Normally this is done by placing it in the custom directory.


The file CallEchoWebService.tcl is a Tcl script (i.e. non-GUI)  designed to show how calls can be made.

The file htdocs/service/index.tml should be copied to service/index.tml under the document root of the web server (normally htdocs).  This page when displayed in a browser as http://localhost:8015/service/ will show a nice page listing what services you have available.  The page will dynamically generate links to the info and wsdl pages that the server generates and also to status pages (located in http://localhost:8015/servicestatus/$serviceName.tml) and form pages (located in http://localhost:8015/serviceforms/$serviceName.tml).

This would allow you to auto generate, or hand generate, forms to call your service operations and also status pages to monitor and control your services.

Alternatively, the following could be done from the current directory (assuming that httpd.tcl is in the current PATH):
       httpd.tcl  -docRoot ./tclhttpd/htdocs -port 8015  -library ./tclhttpd/custom/
