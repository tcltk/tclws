# Directory Listing File.
set request [ns_conn request]
set url [lindex [split $request] 1]
set path $url
set full_path [ns_url2file $url]
ns_log Notice "Running file-not-found.tcl for $request"
if {![string equal "/" "$path"] && [file isdirectory "$full_path"]} {
    css_dirlist $full_path $path 
} else {
    ns_returnnotice 404 "Not Found" "File $path Not Found"
}
