#! /bin/env tclsh8.0

# sage : one who knows all about your Tcl program
#
# this utility will monitor the execution of your Tcl/Tk script and note:
#  1. the amount of time spent in a proc
#  2. the amount of time spent in a proc and its decendents
#  3. the number of times a proc is called.
#  4. total execution time of program


namespace eval ::sage {
  variable VERSION 1.0
  variable activetimes
  variable totaltimes
  variable proccounts
  variable totaltime 
  variable absolutetime
  variable stack ""
  variable timers
  variable outfile "./sage.out"
  variable watch 0
  variable watchstart
  variable sagewatch 0
  variable sagewatchstart
  variable sageview 0
  variable wish 0
  variable coreprocs 0
  
  proc main {argv} {
    variable VERSION
    
    # parse the command line options
    for {set argc 0} {[string match -* [lindex $argv $argc]]} {incr argc} {
      switch -exact -- [lindex $argv $argc] {
        -V -
        -version {
          puts "sage version $VERSION"
          exit 0
        }
        
        -h -
        -help {
          puts "
syntax: sage ?sage options? program ?options?

sage options:

   -V | -version
      Prints the version of sage and exits.

   -d | -data <data output file>
      Specifies the data file to send the output to. Default ./sage.out

   -sv
      Start up the sageviewer on the data when the program is completed.
      
   -w
      Specifies that the program is a wish (Tk) program.
      
   -c
      Instructs sage to collect data for the Tcl/Tk core procs already defined.
"
          exit 0
        }
        
        -d -
        -data {
          incr argc
          variable outfile
          set outfile [lindex $argv $argc]
        }
        
        -sv {
          variable sageview 1
        }
        
        -w {
          variable wish 1
        }
        
        -c {
          variable coreprocs 1
        }

        default {
          puts stderr "unknown option: '[lindex $argv $argc]'"
          exit 1
        }
      }
    }

    # create support for the scope .wait.
    rename ::vwait ::original_vwait
    interp alias {} ::vwait {} [namespace current]::sagevwait
    createArrays .wait.

    variable wish
    if { $wish } {
      # do this hack to keep wish from scarfing our options
      set ::argv [linsert $::argv 0 "--"]
      incr ::argc
      
      # load in the Tk support
      package require Tk

      # create the wrapper around the tkwait command
      rename ::tkwait ::original_tkwait
      interp alias {} ::tkwait {} [namespace current]::sagetkwait
    }
    
    rename ::exit ::original_exit
    interp alias {} ::exit {} [namespace current]::sageexit

    rename ::proc ::original_proc
    interp alias {} ::proc {} [namespace current]::sageproc

#     # example code of wrapping a widget returned from canvas
#     rename ::canvas ::original_canvas
#     interp alias {} ::canvas {} [namespace current]::sagecanvas

    sage_program [lindex $argv $argc] [lrange $argv [incr argc] end]
  }

# this code may be a prototype for an option to wrap tk widgets in order
# to analyze the widget performance; then again it may not be worth it.
#   proc sagecanvas {args} {
#     set rc [catch {uplevel ::original_canvas $args} msg]
#     if { $rc == 0 } {
#       # create a wrapper around the widget command
#       rename ::$msg ::original_$msg
#       uplevel #0 proc $msg args [list "uplevel original_$msg \$args"]
#       return $msg
#     } else {
#       return -code $rc -errorcode $::errorCode -errorinfo $::errorInfo $msg
#     }
#   }
  
  proc sage_program {prog arglist} {
    variable timers
    variable program_name $prog
    variable program_args $arglist
    variable wish

    # reset the argv list and argc
    global argv0 argv argc
    set argv0 [list $prog]
    set argv $arglist
    set argc [llength $argv]

    # create the arrays for the .global. level
    createArrays .global.
    
    # start the absolute timer
    variable absolute
    set absolute [clock clicks]
    
    # start the overall stop watch
    startWatch
    
    # do we want to redefine the core procs?
    variable coreprocs
    if { $coreprocs } {
      foreach proc [uplevel #0 info procs] {
        uplevel #0 proc [list $proc] [list [info args $proc]] [list [info body $proc]]
      }
      
      # reset the watch
      variable watch
      set watch 0
    }
    
    # pretend we're entering the .global. proc
    procEntry .global.
    
    # now crank it up
    if { [set rccode [catch {uplevel #0 source $prog} r]] } {
      puts "\nApplication terminated with uncaught error:\n\n'$r'"
      global errorInfo
      puts $errorInfo
      original_exit 1
    }

    # if we're running Tk we need to wait for all windows to be destroyed.
    # this may be redundant, but some programs rely on the tk_init() code
    # to do this wait for them, but if sage is started with tclsh, this
    # implied wait is not done automatically.
    # We need to check if the . window is still around, and we have to use
    # the info command instead of winfo exists because if the Tk app has
    # been destroyed, no Tk commands will work.
    if { $wish && [info command .] != "" } {
      # I think we can get by with just waiting for the root window.
      tkwait window .
    }
    
    exit 0
  }

  proc sagetkwait {args} {
    # simulate going into the .wait. proc
    procEntry .wait.
    
    uplevel ::original_tkwait $args
    
    # simulate the exiting of this proc
    procExit .wait.
  }
  
  proc sagevwait {args} {
    # simulate going into the .wait. proc
    procEntry .wait.
    
    uplevel ::original_vwait $args
    
    # simulate the exiting of this proc
    procExit .wait.
  }
  
  proc sageexit {{value 0}} {
    # stop the stopwatch
    stopWatch
    
    startSageWatch
    
    variable absolute
    
    # unwind the stack and turn off everyone's timers
    stackUnwind
    
    variable totaltime
    variable totaltimes
    variable proccounts
    variable timers
    variable program_name
    variable program_args
    
    set totaltime [getWatch]
    
    # disengage the proc callbacks
    ::original_proc procEntry {name} {}
    ::original_proc procExit {name args} {}
    
    # stop the absolute timer
    set stop [clock clicks]

    stopSageWatch

    set absolute [expr {$stop - $absolute}]
    
    # get the sage overhead time
    set sagetime [getSageWatch]
    
    # calculate the core time (Tcl/Tk core)
    set core [expr {$absolute - $totaltime - $sagetime}]
    
    # save the data
    variable outfile
    variable VERSION
    set f [open $outfile w]
    puts $f "set VERSION {$VERSION}"
    puts $f "set program_name {$program_name}"
    puts $f "set program_args {$program_args}"
    puts $f "set absolute $absolute"
    puts $f "set sagetime $sagetime"
    puts $f "set totaltime $totaltime"
    puts $f "set coretime $core"
    puts $f "array set totaltimes {[array get totaltimes]}"
    puts $f "array set proccounts {[array get proccounts]}"
    puts $f "array set timers {[array get timers]}"
    close $f
    
    variable sageview
    if { $sageview } {
      puts "(starting up sageviewer in background)"
      exec sageview -data $outfile &
    }
    
    original_exit $value
  }
  
  proc createArrays {name} {
    variable totaltimes
    variable proccounts
    variable timers

    # initialize the arrays for this proc
    set totaltimes($name) 0
    set proccounts($name) 0
    set timers($name) 0
  }
  
  proc sageproc {name args body} {
    # stop the watch
    stopWatch
    
    # start the sage watch
    startSageWatch
    
    # get the namespace this proc is being defined in
    set ns [uplevel namespace current]
    
    # update the name to include the namespace
    if { $ns == "::" } {
      set ns ""
    }
    set name ${ns}::$name

    createArrays $name
    
    # create the callbacks for proc entry and exit
    set extra "[namespace current]::procEntry $name;"
    append extra "set __.__ {};trace variable __.__ u {[namespace current]::procExit $name};"

    stopSageWatch
    
    startWatch
    
    # define the proc with our extra stuff snuck in
    uplevel ::original_proc $name [list $args] [list [concat $extra $body]]
  }
  
  proc push {v} {
    variable stack
    lappend stack $v
  }
  
  proc pop {} {
    variable stack
    set v [lindex $stack end]
    set stack [lreplace $stack end end]
    return $v
  }
  
  proc look {} {
    variable stack
    set s [lindex $stack end]
    return $s
  }
  
  proc stackUnwind {} {
    variable stack
    variable activetimes
    variable totaltimes
    variable timers
    
    # now unwind all the stacked procs by calling procExit on each
    while { $stack != "" } {
      # stop the timer of the proc
      set procname [look]
      
      # procExit assumes the sage watch isn't going
      stopSageWatch
      startWatch

      procExit $procname

      stopWatch
      startSageWatch
    }
  }
  
  proc startWatch {} {
    variable watchstart
    set watchstart [clock clicks]
  }
  
  proc stopWatch {} {
    set stop [clock clicks]
    variable watch
    variable watchstart
    incr watch [expr {$stop - $watchstart}]
    
    return $watch
  }
  
  proc getWatch {} {
    variable watch
    return $watch
  }
  
  proc startSageWatch {} {
    variable sagewatchstart
    set sagewatchstart [clock clicks]
  }
  
  proc stopSageWatch {} {
    set stop [clock clicks]
    variable sagewatch
    variable sagewatchstart
    incr sagewatch [expr {$stop - $sagewatchstart}]
    
    return $sagewatch
  }
  
  proc getSageWatch {} {
    variable sagewatch
    return $sagewatch
  }
  
  proc startTimer {v} {
    variable timers
    
    if { $v == "" } return

    if { [llength $timers($v)] != 1 } {
      puts "sage warning: timer for $v is already going; timers($v): $timers($v)"
      return
    }
    
    set start [getWatch]
    lappend timers($v) $start
  }
  
  proc stopTimer {v} {
    variable timers
    
    if { $v == "" } return

    if { [llength $timers($v)] != 2 } {
      puts "sage warning: timer for $v is not going; timers($v): $timers($v)"
      return
    }
    
    foreach {total start} $timers($v) {}
    set stop [getWatch]
    set timers($v) [expr {$total + ($stop - $start)}]
  }
  
  proc procEntry {procname} {
    set watch [stopWatch]

    # start the sage overhead watch
    startSageWatch
    
    # stop the timer of the caller
    set caller [look]
    stopTimer $caller
    
    variable activetimes
    variable proccounts
    
    incr proccounts($procname)
    
    # push this proc on the stack
    push $procname
    
    # start the timer for this
    startTimer $procname
    
    # register the start time of this proc
    lappend activetimes($procname) $watch
    
    # stop the sage overhead watch
    stopSageWatch
    
    startWatch
  }
  
  # we need the args because this is called from a vartrace handler
  proc procExit {procname args} {
    set watch [stopWatch]
    
    # start the sage overhead watch
    startSageWatch
    
    # stop the timer of the proc
    stopTimer [pop]
    
    variable activetimes
    variable totaltimes
    
    # if there are more than one active times, then this call is recursive
    # and therefore it will be handled in the descendents time
    if { [llength $activetimes($procname)] == 1 } {
      set totaltimes($procname) [expr {$totaltimes($procname) + ($watch - $activetimes($procname))}]
    }
    
    # remove the last one
    set activetimes($procname) [lreplace $activetimes($procname) end end]

    # now restart the timer of the caller
    startTimer [look]
    
    # stop the sage overhead watch
    stopSageWatch
    
    startWatch
  }
}

::sage::main $argv
