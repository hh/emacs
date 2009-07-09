#!/usr/bin/perl -w
# compiles a task list and a todo list from Emacs planner files and
# publishes them as a html file
# ToDos are recognized in a case-insensitive fashion, but they have to be
# sections, like in:
# ** ToDo
#
# Invocation: tasklist.pl [-e date] [-h] [-m t|f] [-o outpath] [-p planner-dir] [-r status] [-s date] [-u baseurl]
# The start and end dates must be in ISO notation: YYYY-MM-DD
#
# What you need:
# Perl to run this script
# find, grep, sort to extract tasks (not the broken Windows versions please)
# planner.el http://sacha.free.net.ph/notebook/wiki/WelcomePage.php
# taskmm.pl to create mindmaps (optional)
#
# (c) Markus Hoenicka 2003-2007 <markus AT mhoenicka DOT de>
# 2005-01-16
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



## use this module to read command line options
use Getopt::Std;

### user-customizable stuff

# the CSS file that should be used to beautify the HTML output
my $cssfile = "planner-muse.css";

# the email address of someone who doesn't get enough mail anyway
my $made = "mailto:webmaster\@PC51997";

# the name of your homepage
my $home = "WelcomePage.html";

# the name of the index file created by emacs-wiki
my $index = "WikiIndex.html";

# a text button used to link to the task
my $button = "[-&gt;]";

# the suffix used by planner/muse files. The default is ".muse"
my $muse_suffix = ".muse";

# The command that generates the mind map. If the taskmm.pl script
# is in your path, "taskmm.pl" is all you need. Otherwise specify
# the full path to the script
my $taskmm = "/home/markus/prog/tasklist/taskmm/taskmm.pl";

# the character encoding of the resulting html page. Should be
# utf-8 for the current planner versions
my $charset = "utf-8";

# the following two-dimensional array contains regular expressions
# to find files with certain properties. The first column of each row
# contains the name of the table to be generated. The second column
# contains a regexp which is passed to grep. This will return a list
# of filenames that will be displayed as a table
# Please note that characters in the regexps may need to be escaped
# twice (first in Perl, then in grep)
my @filelist = (
		["todo", "\** [tT][oO][dD][oO]"],
#		["pending", "\** [pP][eE][nN][dD][iI][nN][gG]"]
);

# the following two-dimensional array contains regular expressions
# to extract additional lines of information from your project files.
# The first column contains the name of the table to be generated. The
# second column contains a regexp which is passed to grep. This will
# return a list of matching lines that will be printed in a table 
# together with the filenames
# If the string used to match the line follows the pattern "[.*]", the
# beginning of the line up to (and including) the string will be
# stripped off before the line is added to the table. Use this to prefix
# lines with a tag in order to add them to one of the tables.
# Please note that characters in the regexps may need to be escaped
# twice (first in Perl, then in grep)
my @linelist = (
#		["organbath", " \\[organbad\\] "],
#		["elisa", " \\[ELISA\\] "],
#		["crfreezer", " \\[crf\\] "],
);
 
### end user-customizable stuff


##
## the real code starts here
##


## this hash will receive the command line options
my %opts;

## the switches are:
## -e date: optional end date
## -h: prints help
## -m: include mindmap
## -o: output path
## -p: planner directory
## -r: restrict status
## -s: optional start date
## -u: the base URL

# and these are the corresponding variables
my $enddate = "2037-12-31";
my $mindmap = 'f';
my $outpath = undef;
my $out_basepath = undef;
my $plannerdir = "~/Plans";
my $restrictstatus = '_X>PoC';
my $startdate = "1970-01-01";
my $baseurl = undef;

getopts('e:hm:o:p:r:s:u:', \%opts);

## loop over all command line options
while (($key, $value) = each %opts) {
    if ($key eq "e") {
	$enddate = $value;
    }
    elsif ($key eq "h") {
	print "tasklist.pl creates a table of tasks from your Emacs planner files\n";
	print "Usage: [perl] tasklist.pl [-e date] [-h] [-m t|f] [-o outpath] [-p planner-dir] [-r status] [-s date] [-u baseurl]\nThe script scans the planner files in planner-dir and writes its output to outpath\nOptions: -e date        optional end date\n         -h             print this help and exits\n         -m             create mind map (t|f)\n         -o outpath     send output to outpath\n         -p planner-dir path to planner files\n         -r status      restrict status (default: all)\n         -s date        start date\n         -u baseurl     base URL for generated web pages\n";
	exit(0);
    }
    elsif ($key eq "m") {
	$mindmap = $value;
    }
    elsif ($key eq "o") {
	$outpath = $value;
    }
    elsif ($key eq "p") {
	$plannerdir = $value;
    }
    elsif ($key eq "r") {
	$restrictstatus = $value;
    }
    elsif ($key eq "s") {
	$startdate = $value;
    }
    elsif ($key eq "u") {
	$baseurl = $value;
    }
}

# mangle the output paths
# outpath is a unix-style output path

if (defined($outpath)) {
    # open the output file for writing
    unless (open TASKLIST, ">$outpath") {
	die "cannot open output file";
    }
    select TASKLIST;

    $out_basepath = $outpath;

    # remove filename
    if ($out_basepath =~ m/\//) {
	$out_basepath =~ s/\/[^\/]*$//;
    }
    else {
	$out_basepath = "/";
    }

    unless (defined($baseurl)) {
	$baseurl = "file://" . $out_basepath;
    }
}
else {
    # output goes to stdout
    $out_basepath = `pwd`;
    chomp $out_basepath;

    unless (defined($baseurl)) {
	$baseurl = "file://" . $out_basepath;
    }
}

# extract all tasks from the planner day files. Using the full paths for the
# find and sort calls avoids running the broken Windoze versions if we're
# on Cygwin
my @tasks = `cd $plannerdir; /usr/bin/find ./ -name "20??.*[^~]" | xargs grep -H "^#[ABC][ 0-9]" | /usr/bin/sort`;


# check whether the tasks lines contain a task number
my $number = shift @tasks;
my $print_number = 'f';
if ($number =~ /.*:\#[ABC](\d+).*/) {
    $print_number = 't';
}
unshift(@tasks, $number);

# the current time in YYYY-MM-DD format
my $ltime = &get_ltime();

# write HTML header
print '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
  <head>
    <title>Task List</title>
    <meta name="generator" content="tasklist.pl">
    <meta http-equiv="Content-Type"
	  content="text/html; charset=' . $charset . '">
    <link rev="made" href="' . $made . '">
    <link rel="home" href="' . $home . '">
    <link rel="index" href="' . $index . '">
    <link rel="stylesheet" type="text/css" href="' . $cssfile . '">
  </head>
  <body>
    <h1>Task List</h1>
    <table class="ewiki-tasklist-table" border="2" cellpadding="5">
      <tbody>
        <tr>
          <th>Date</th>';

if ($print_number eq 't') {
    print '          <th>Number</th>';
}

print '          <th>Priority</th>
          <th>Status</th>
          <th>Task</th>
          <th>Project</th>
        </tr>';

# this is used to suppress the date in subsequent rows of the same date
my $prevdate = "1970-01-01";

# loop over all tasks
foreach $task (@tasks) {
    # remove newline
    chomp $task;

    # remove leading dotslash from file name
    $task =~ s%\./%%;

    # extract the date (using dot notation)
    my $date = $task;
    $date =~ s%(.*):\#.*%$1%;

    # remove trailing muse suffix
    if (length($muse_suffix) > 0) {
	$date =~ s%$muse_suffix%%;
    }

    # get the date (using dash notation)
    my $taskdate = $date;
    $taskdate =~ s%\.%-%g;

    # print only tasks due after the start date and before the end date
    if ($taskdate lt $startdate || $taskdate gt $enddate) {
	# nothing to do
	next;
    }

    # extract the task number
    my $number = $task;
    $number =~ s%.*:\#[ABC](\d*).*%$1%;

    # extract the priority
    my $priority = $task;
    $priority =~ s%.*:\#([ABC]).*%$1%;

    # extract the status
    my $status = $task;
    $status =~ s%.*:\#[ABC]\d* +(.).*%$1%;

    # see whether the status was requested for output
    if (index($restrictstatus, $status) == -1) {
	next;
    }

    # extract the task description
    my $the_task = $task;
    $the_task =~ s%.*:\#[ABC]\d* +. (.*) \(.*\)$%$1%;

    # extract the task ref
    my $ref = $task;
    $ref =~ s%.*:\#([ABC]\d*).*%$1%;
 
    # extract the project name
    my $project = $task;

    if ($project =~ s%.*\((.*)\)%$1%) {
	$project =~ s%.*\((.*)\)%$1%;
    }
    else {
	$project = $date;
    }


    # remove double brackets, if any
    $project =~ s%\[\[(.*)\]\]%$1%;

    # start a new table row
    if ($taskdate eq $ltime) {
	if ($priority eq "A") {
	    print "<tr class=\"atoday\">";
	}
	elsif ($priority eq "B") {
	    print "<tr class=\"btoday\">";
	}
	else {
	    print "<tr class=\"ctoday\">";
	}
    }
    else {
	if ($priority eq "A") {
	    print "<tr class=\"a\">";
	}
	elsif ($priority eq "B") {
	    print "<tr class=\"b\">";
	}
	else {
	    print "<tr class=\"c\">";
	}
    }

    # print date only if different from previous row
    if ($date eq $prevdate) {
	print "<td></td>";
    }
    else {
	print "<td><a href=\"$date.html\">$taskdate</a></td>";
    }

    if ($print_number eq 't') {
	print "<td>$number</td>";
    }

    # allow for color coding according to priority
    if ($priority eq "A") {
	print "<td class=\"prioritya\">$priority</td>";
    }
    elsif ($priority eq "B") {
	print "<td class=\"priorityb\">$priority</td>";
    }
    elsif ($priority eq "C") {
	print "<td class=\"priorityc\">$priority</td>";
    }
    else {
	print "<td>$priority</td>";
    }

    print "<td>$status</td>";

    # allow for color coding according to status
    if ($taskdate lt $ltime && $status ne "X") {
	print "<td class=\"taskoverdue\"><a href=\"$date.html#$ref\">$button</a> $the_task</td>";
    }
    elsif ($status eq "X") {
	print "<td class=\"taskdone\"><a href=\"$date.html#$ref\">$button</a> $the_task</td>";
    }
    elsif ($status eq "_") {
	print "<td class=\"taskpending\"><a href=\"$date.html#$ref\">$button</a> $the_task</td>";
    }
    elsif ($status eq ">") {
	print "<td class=\"taskdelegate\"><a href=\"$date.html#$ref\">$button</a> $the_task</td>";
    }
    else {
	print "<td><a href=\"$date.html#$ref\">$button</a>$the_task</td>";
    }

    print "<td><a href=\"$project.html\">$project</a></td></tr>\n";
    
    # save date for next iteration
    $prevdate = $date;
}

# finish table
print "</tbody></table>";

# print filename tables
foreach my $filelistitem (@filelist) {
    my $tablename = ${@{$filelistitem}}[0];
    my $tableregex = ${@{$filelistitem}}[1];
    my @files = `cd $plannerdir; grep -l \"$tableregex\" *`;
    print "<h1>$tablename</h1>\n";

    foreach $file (@files) {
	# skip backup files
	unless ($file =~ /~$/) {
	    print "<p><a href=\"$file.html\">$file</a></p>";
	}
    }
}

# print extra line tables
foreach my $linelistitem (@linelist) {
    my $linename =${@{$linelistitem}}[0];
    my $lineregex =${@{$linelistitem}}[1];
    my @lines = `cd $plannerdir; grep -H \"$lineregex\" *`;

    print '  <h1>' . $linename . '</h1>
    <table class="ewiki-tasklist-table" border="2" cellpadding="5">
      <tbody>
        <tr>
          <th>Task</th>
          <th>Project</th>
        </tr>';

# loop over all entries
    foreach $entry (@lines) {
	my @line = split(/:/, $entry);

	my $file = $line[0];
	my $text = $line[1];

	$text =~ s/.*\[.*\] //;

	# skip backup files
	unless ($file =~ /~$/) {
	    print "<tr><td>$text</td><td><a href=\"$file.html\">$file</a></td></tr>";
	}
    }
    print '</tbody></table>';
}

print "<br>\n";

if ($mindmap eq 't') {
    # generate legend
    print "<h1>Mind Map</h1><p>The following icons mean that the node contains at least one of:</p><p><img src=\"icons/messagebox_warning.png\" alt=\"messagebox_warning\">open tasks <img src=\"icons/button_ok.png\" alt=\"button_ok\">finished tasks <img src=\"icons/forward.png\" alt=\"forward\">delegated tasks <img src=\"icons/flag.png\" alt=\"flag\">pending tasks <img src=\"icons/ksmiletris.png\" alt=\"ksmiletris\">tasks in progress <img src=\"icons/button_cancel.png\" alt=\"button_cancel\">cancelled tasks <img src=\"icons/idea.png\" alt=\"idea\">todo sections</p>";

    # generate mind map
    system ("$taskmm -p $plannerdir -o $out_basepath/tasklist.mm -u $baseurl");

    # insert mind map
    if (defined($mindmap)) {
	print "  <applet code=\"freemind.main.FreeMindApplet.class\"
          archive=\"freemindbrowser.jar\" width=\"100%\" height=\"100%\">
  <param name=\"type\" value=\"application/x-java-applet;version=1.4\">
  <param name=\"scriptable\" value=\"false\">
  <param name=\"modes\" value=\"freemind.modes.browsemode.BrowseMode\">
  <param name=\"browsemode_initial_map\"
         value=\"$baseurl/tasklist.mm\">
  <param name=\"initial_mode\" value=\"Browse\">
  <param name=\"selection_method\" value=\"selection_method_direct\">
  <param name=\"mapysize\" value=\"6400\">
  </applet>
";

    }
}

# output html footer
print '<br>

    <div class="navfoot">
      <hr>
      <table width="100%" border="0" summary="Footer navigation">
	<tr>
	  <td width="33%" align="left">
	    <span class="footdate">Updated: ' . $ltime . '</span>
	  </td>
	  <td width="34%" align="center">

	    <span class="foothome">
	      <a href="' . $index . '">Index</a>
	    </span>
	  </td>
	  <td width="33%" align="right">
	    
	  </td>
	</tr>
      </table>

    </div>
  </body>
</html>';

exit 0;

# return current local time in YYYY-MM-DD format
sub get_ltime() {
    my %months = (
	0 => "01",
	1 => "02",
	2 => "03",
	3 => "04",
	4 => "05",
	5 => "06",
	6 => "07",
	7 => "08",
	8 => "09",
	9 => "10",
	10 => "11",
	11 => "12");

    my($sec, $min, $hour, $day, $mon, $year, $wday, $yday, $isdst) = localtime(time);
    my $fullday;

    if ($day < 10) {
	$fullday = "0" . $day;
    }
    else {
	$fullday = $day;
    }
    
    my $fullyear = 1900 + $year;

    my $datestring = $fullyear . "-" . $months{$mon} . "-" . $fullday;
}
