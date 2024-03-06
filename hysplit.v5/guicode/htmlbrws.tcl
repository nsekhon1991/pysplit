#!/bin/sh
# the next line restarts using wish \
exec wish8.0 "$0" "$@"

# here is a sample html viewer to demonstrate the library usage
# Copyright (c) 1995 by Sun Microsystems
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

#if {$tk_version < 4.0 || [regexp {b[123]} $tk_patchLevel] } {
#  puts stderr "This library requires TK4.0, this is only $tk_version, \
#	patchlevel $tk_patchLevel"
#  exit 1
#}

#  Namespace featurs added by A.D. Taylor July-Aug 2002.  A calling procedure
#  may include code similar to
#
#  namespace eval html {
#    source [file join $tcl_dir .. browser htmlbrws.tcl ]
#    namespace export load_html
#  }
#  namespace import ::html::load_html
#
#  And when displaying html, may include
#         load_html [file join $html_dir some.html ]



#if {[catch {array get env *}]} {
#  puts stderr "This library requires tcl7.4, this version is too old!"
#  exit 1
#}
set src_dir [file dirname [info script]]
source [file join $src_dir htmllib.tcl]
# set initial values
variable Size 6                 ;# font size adjustment
variable Indent 1.0	           ;# tab spacing (cm)
variable Home $src_dir/help.htm ;# home document
variable Url $Home                 ;# current file
variable Running busy			;# page status
variable message ""				;# message line

proc load_html {home {flag 0}} {
# opens a browser window and loads the file "home" into it.
# if flag == 1, entire application closes when closing browser,
# if flag == 0 (default), clicking on quit only closes browser
  variable Size
  variable Indent
  variable Home
  if {[file isfile $home]} {
    set Home $home
  }
  catch {destroy .html}
  toplevel .html
# make the interface and render the home page
  setup .html $flag            ;# the catch lets us re-source this file
  HMinit_win .html.text
  HMset_state .html.text -size $Size
  HMset_indent .html.text $Indent
  render $Home
}

proc Html_Back {} {
   variable Url
   variable Url2_List
   variable Running
   if { $Running != "ready" } {return}
   set Dest [lindex $Url2_List end-1 ]
   if {$Dest == {} } {
      foreach {file scrollTo textIndex} [list $Url 0.0 1.0] {break}
   } else {
      foreach {file scrollTo textIndex} $Dest {break}
   }
   set Url2_List [lreplace $Url2_List end-1 end ]

   render $file $scrollTo $textIndex
   return
}

# flag == 1 if quit kills wish, 0 otherwise
proc setup {c flag} {
  variable Size
  variable Indent
	frame $c.frame
	menubutton $c.menu -relief raised -text Options... -menu $c.menu.m
    if {$flag == 1} {
      button $c.quit  -command "catch {destroy .html}; exit" -text Quit
    } else {
      button $c.quit  -command "catch {destroy .html}" -text Quit
    }
  button $c.back -command [namespace code {Html_Back} ] -text Back
	entry $c.entry  -textvariable [namespace current]::Url -width 35
	label $c.file  -text file:
	label $c.status -textvariable [namespace current]::Running -width 6 -relief ridge \
			-bd 2 -padx 9 -pady 3
	label $c.msg -textvariable [namespace current]::message
	scrollbar $c.scrollbar  -command "$c.text yview"  -orient v
	option add *Text.height 40 startup
	option add *Text.width 80 startup
  text $c.text -yscrollcommand "$c.scrollbar set" -padx 20 -takefocus 0

	pack $c.frame -side top -fill x
#	pack $c.msg -side top
  pack $c.scrollbar -side right -expand 0 -fill y
	pack $c.text -side left -fill both -expand 1
	pack $c.quit $c.menu $c.back -in $c.frame -side left -anchor w -fill both
  pack $c.file $c.entry $c.status -in $c.frame -side left -anchor w

  $c.text configure -cursor hand2

# set up some sample keyboard bindings for the text widget
	bind $c.entry <Return> [namespace code {render $Url} ]
	bind $c <End> "$c.text yview end"
	bind $c <Home> "$c.text yview 0.0"
	bind $c <Next> "$c.text yview scroll 1 page"
	bind $c <Prior> "$c.text yview scroll -1 page"
	bind $c <Down> "$c.text yview scroll 1 unit"
	bind $c <Up> "$c.text yview scroll -1 unit"

# I'm constantly being criticized for never using menus.
# so here's a menu.  So there.
	menu $c.menu.m
	$c.menu.m add command -label "option menu"
	$c.menu.m add separator
	$c.menu.m add command -label "font size" -foreground red
	$c.menu.m add radiobutton -label tiny -value 0  \
     -variable [namespace current]::Size \
	  -command [namespace code {HMset_state .html.text -size $Size ; render $Url} ]
	$c.menu.m add radiobutton -label small -value 3 \
     -variable [namespace current]::Size \
		-command [namespace code {HMset_state .html.text -size $Size ; render $Url} ]
	$c.menu.m add radiobutton -label medium -value 6 \
     -variable [namespace current]::Size \
		-command [namespace code {HMset_state .html.text -size $Size ; render $Url} ]
	$c.menu.m add radiobutton -label large -value 12 \
     -variable [namespace current]::Size \
	  -command [namespace code {HMset_state .html.text -size $Size ; render $Url} ]
	$c.menu.m add separator
	$c.menu.m add command -label "indent level" -foreground red
	$c.menu.m add radiobutton -label small -value 0.5 \
     -variable [namespace current]::Indent \
	  -command [namespace code {HMset_indent .html.text $Indent } ]
	$c.menu.m add radiobutton -label medium -value 1.0 \
     -variable [namespace current]::Indent \
	  -command [namespace code {HMset_indent .html.text $Indent } ]
	$c.menu.m add radiobutton -label large -value 2.0 \
     -variable [namespace current]::Indent \
	  -command [namespace code {HMset_indent .html.text $Indent } ]
}

proc normalFile {fileName} {

# new code to remove unnecessary pathname elements of the form abc/../
# from file names.  Albion Taylor, 8/15/02
#  Input:  fileName, either an absolute or relative filename
#  returns: same filename, without unnecessary xxx/../ terms.

  set fileNameList [file split $fileName]
  set fileNamePathType [file pathtype $fileName]
  switch $fileNamePathType {
    relative {
      set Head {}
       #do nothing to fileNameList
    }
    volumerelative -
    absolute {
      set Head [lindex $fileNameList 0]
      set fileNameList [lrange $fileNameList 1 end]
      if {$Head == "/" } {set fileNamePathType absolute }
    }
  }
  set outList {}
  foreach member $fileNameList {
    switch $member {
      . {
        #do nothing - self referencing directory
      }
      .. {
        switch [lindex $outList end] {
          {} -
          .. {
            lappend outList ..
          }
          default {
            set outList [lrange $outList 0 end-1]
          }
        }
      }
      default {
        lappend outList $member
      }
    }
  }
  switch $fileNamePathType {
    absolute {
      while { [lindex $outList 0] == ".." } {
        set outList [lrange $outList 1 end]
      }
    }
  }
  return [eval file join $Head $outList ]
}

# Go render a page.  We have to make sure we don't render one page while
# still rendering the previous one.  If we get here from a recursive
# invocation of the event loop, cancel whatever we were rendering when
# we were called.
# If we have a fragment name, try to go there.

proc render {file {textScroll 0.0} {textIndex 1.0 }} {
	variable HM.text
   variable Url
	variable Running
   variable message
   variable Url2_List
   if {! [info exists Url2_List] } {set Url2_List {} }

  .html.text configure -state normal

   foreach {filepart fragment} [split $file # ] {break}

# Portion before (first!) # in filepart, portion after # (to next #!) in fragment.
# if $filepart=={} && $fragment != {}, using current file & no need to reload
# if $filepart != {}, neeed to HMload $filepart.

   if {$filepart == {} } {
      set filepart $Url
   } elseif {$filepart != $Url} {
     .html.text config -bg white

     HMreset_win .html.text
	  set Running busy
	  set message "Displaying $filepart"
	  update idletasks
	  set Url $filepart

#Arthur: swapped order
     HMparse_html [get_html $filepart] {HMrender .html.text}

	   set Running ready
	   HMset_state .html.text -stop 1	;# stop rendering previous page if busy
	   set message ""
   }

   if {$textIndex != {} } {
      .html.text yview moveto $textScroll
      .html.text see $textIndex
      .html.text mark set current $textIndex
   } elseif {$fragment != {} } {
      HMgoto .html.text $fragment
   }

#Arthur: added
   .html.text configure -state disabled
   lappend Url2_List [list $filepart [lindex [.html.text yview] 0]\
   [.html.text index current] ]
   return
}

# given a file name, return its html, or invent some html if the file can't
# be opened.

proc get_html {file} {
	variable Home
	if {[catch {set fd [open $file]} msg]} {
		return "
			<title>Bad file $file</title>
			<h1>Error reading $file</h1><p>
			$msg<hr>
			<a href=$Home>Go home</a>
		"
	}
	set result [read $fd]
	close $fd
	return $result
}

# Override the library link-callback routine for the sample app.
# It only handles the simple cases.

proc HMlink_callback {win href} {
	variable Url
   variable Url2_List

   if [string match -nocase http:* $href ] {
     tk_messageBox -icon error -message "Unable to fetch Internet References.\n
\"$href\"\n\n is an Internet Reference.  Use a different browser."
     return
   }

   set currentLoc [lindex $Url2_List end]
   set currentLoc [lreplace $currentLoc end-1 end \
     [lindex [.html.text yview] 0]\
   [.html.text index current ]  ]
   set Url2_List [lreplace $Url2_List end end $currentLoc ]

	if {[string match #* $href]} {
      set Url_ref $Url$href
	} elseif {[string match /* $href]} {
      set Url_ref $href
   } elseif {[string match ?:* $href]} {
		set Url_ref $href
	} else {
		set Url_ref [file dirname $Url]/$href
	}
	update
	render [normalFile $Url_ref ]
}


# Supply an image callback function
# Read in an image if we don't already have one
# callback to library for display

proc HMset_image {win handle src} {
	variable Url
   variable message
	if {[string match /* $src]} {
		set image $src
	} else {
		set image [file dirname $Url]/$src
	}
	set message "fetching image $image"
	update
	if {[string first " $image " " [image names] "] >= 0} {
		HMgot_image $handle $image
	} else {
		set type photo
		if {[file extension $image] == ".bmp"} {set type bitmap}
		catch {image create $type $image -file $image} image
		HMgot_image $handle $image
	}
}

# Handle base tags.  This breaks if more than 1 base tag is in the document

proc HMtag_base {win param text} {
	variable Url
	upvar #0 HM$win var
	HMextract_param $param href Url
}

# downloading fonts can take a long time.  We'll override the default
# font-setting routine to permit better user feedback on fonts.  We'll
# keep our own list of installed fonts on the side, to guess when delays
# are likely

proc HMset_font {win tag font} {
	variable message
   variable Fonts
	if {![info exists Fonts($font)]} {
		set Fonts($font) 1
		.html.msg configure -fg blue
		set message "downloading font $font"
		update
	}
	.html.msg configure -fg black
	set message ""
	catch {$win tag configure $tag -font $font} message
}

# Lets invent a new HTML tag, just for fun.
# Change the color of the text. Use html tags of the form:
# <color value=blue> ... </color>
# We can invent a new tag for the display stack.  If it starts with "T"
# it will automatically get mapped directly to a text widget tag.

#proc HMtag_color {win param text} {
#	upvar #0 HM$win var
#	set value bad_color
#	HMextract_param $param value
#	$win tag configure $value -foreground $value
#	HMstack $win "" "Tcolor $value"
#}

#proc HMtag_/color {win param text} {
#	upvar #0 HM$win var
#	HMstack $win / "Tcolor {}"
#}

# Add a font size manipulation primitive, so we can use this sample program
# for on-line presentations.  sizes prefixed with + or - are relative.
#  <font size=[+-]3>  ..... </font>.  Note that this is not the same as
# Netscape's <font> tag.

proc HMtag_font {win param text} {
	upvar #0 HM$win var
	set size 0; set sign ""
	HMextract_param $param size
	regexp {([+-])? *([0-9]+)} $size dummy sign size
	if {$sign != ""} {
		set size [expr [lindex $var(size) end] $sign $size]
	}
	HMstack $win {} "size $size"
}

# This version is closer to what Netscape does
proc HMtag_font {win param text} {
	upvar #0 HM$win var
	set size 0; set sign ""
	set f_found [HMextract_param $param size]
	regexp {([+-])? *([0-9]+)} $size dummy sign size
	if {$sign != ""} {
		set size [expr [lindex $var(size) end] $sign $size*2]
		HMstack $win {} "size $size"
	} elseif {$f_found != 0} {
		HMstack $win {} "size [expr {10 + 2 * $size}]"
	} else {
      HMstack $win {} "size [lindex $var(size) end]"
   }
   set value bad_color
   HMextract_param $param color value
   if {$value != "bad_color"} {
     $win tag configure $value -foreground $value
     HMstack $win "" "Tcolor $value"
   } else {
     # Set the color to itself, but we don't necessarily already have a color,
     # so currently this is a non-op.
     HMstack $win "" "Tcolor [lindex $var(size) end]"
   }
}

proc HMtag_/font {win param text} {
	upvar #0 HM$win var
	HMstack $win / "size {}"
   HMstack $win / "Tcolor {}"
}

proc HMtag_body {win param text} {
	upvar #0 HM$win var
   set bgcolor bad_color
   HMextract_param $param bgcolor
   if {$bgcolor != "bad_color"} {
     $win configure -bg $bgcolor
   }
}

#Arthur::Overwriting original (for green)
#
proc HMwent_to {win where {count 0} {color green}} {
	upvar #0 HM$win var
	if {$count > 7} return
	catch {$win tag configure N:$where -foreground $color}
	update
	after 200 [list [namespace current]::HMwent_to $win $where [incr count] \
				[expr {$color=="green" ? "" : "green"}]]
}

if {($argc != 0) && (! [info exists HTMLBRWS_NOT_STANDALONE])} {
  wm withdraw .
  set flag 0
  set filename ""
    foreach arg $argv {
	if {[string index $arg 0] == "-"} {
	    if {$arg == "-html_standalone"} {
              set flag 1
            }
        } else {
          set filename $arg
	}
    }
    if {$filename == ""} {
       tk_messageBox -message "Was not given a filename"
       exit
    }
    if {[file isfile $filename] != 1} {
       tk_messageBox -message "Initial file $filename is not a file"
       exit
    }
    set Home $filename
    load_html $filename $flag
}
