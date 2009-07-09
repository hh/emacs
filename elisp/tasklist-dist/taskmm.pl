#!/usr/bin/perl -w
# compiles a mind map from Emacs planner files
# The mind map is an XML file as used by FreeMind
# (http://freemind.sourceforge.net)
#
# Invocation: taskmm.pl [-h] [-o outpath] [-p planner-dir] [-u baseurl]
#
# The code assumes that there is exactly one planner project in
# planner-directory, otherwise results are unpredictable.
#
# What you need:
# Perl to run this script
# find, grep to extract tasks (not the broken Windows versions please)
# planner.el http://sacha.free.net.ph/notebook/wiki/WelcomePage.php
#
# (c) Markus Hoenicka 2005 <markus AT mhoenicka DOT de>
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

# the start tag of the mind map, should include version information
my $mapstart = "<map version=\"0.7.1\">\n";

# the icons to be used
$open_icon = "<icon BUILTIN=\"messagebox_warning\"/>";
$done_icon = "<icon BUILTIN=\"button_ok\"/>";
$delegated_icon = "<icon BUILTIN=\"forward\"/>";
$pending_icon = "<icon BUILTIN=\"flag\"/>";
$inprogress_icon = "<icon BUILTIN=\"ksmiletris\"/>";
$cancelled_icon = "<icon BUILTIN=\"button_cancel\"/>";
$todo_icon = "<icon BUILTIN=\"idea\"/>";

# the colors to be used for task priorities
$priority_a = " COLOR=\"#FF0000\"";
$priority_b = " COLOR=\"#00FF00\"";
$priority_c = " COLOR=\"#000000\"";

# whether (t) or not to use averaging to color nodes by priority
$avg = 't';

# the suffix used by planner files
my $suffix = ".muse";

### end user-customizable stuff

## this hash will receive the command line options
my %opts;

## the switches are:
## -h: prints help
## -o: output path
## -p: planner directory
## -u: the base URL

# and these are the corresponding variables
my $outpath = undef;
my $out_basepath = undef;
my $plannerdir = "~/Plans";
my $baseurl = undef;

getopts('ho:p:u:', \%opts);

## loop over all command line options
while (($key, $value) = each %opts) {
    if ($key eq "h") {
	print "tasklist.pl creates a table of tasks from your Emacs planner files\n";
	print "Usage: [perl] taskmm.pl [-h] [-o outpath] [-p planner-dir] [-u baseurl]\nThe script scans the planner files in planner-dir and writes its output to outpath, using baseurl to prefix the filenames in hyperlinks\nOptions: -h             print this help and exits\n         -o outpath     send output to outpath\n         -p planner-dir path to planner files\n         -u baseurl     base URL for generated web pages\n";
	exit(0);
    }
    elsif ($key eq "o") {
	$outpath = $value;
    }
    elsif ($key eq "p") {
	$plannerdir = $value;
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

unless ($baseurl =~ m/\/$/) {
    $baseurl .= "/";
}

# extract all project file headers from the planner non-day files.
# Using the full paths for the find and sort calls avoids running 
# the broken Windoze versions if we're on Cygwin
# the regexp means: all files with no dot in the fifth position (planner
# day files look like '2005.01.03') and with no trailing tilde (as in
# Emacs backup files). The grep command looks for '**/ ' or '**/*'which
# is the starting sequence of the backlink in the planner files. The
# result is a list of strings of the format './filename:<header>'

#my @headers = `cd $plannerdir; /usr/bin/find ./ -regex '\\./....[^\\.].*[^~]' -o -regex '\\./.[^~]' -o -regex '\\./..[^~]' -o -regex '\\./...[^~]' -o -regex '\\./....[^~]' | xargs grep \'\\*\\*/[ *]\'`;
my @headers = `cd $plannerdir; /usr/bin/find ./ -regex '\\./....[^\\.].*[^~]' -o -regex '\\./.[^~]' -o -regex '\\./..[^~]' -o -regex '\\./...[^~]' -o -regex '\\./....[^~]' | xargs grep \'backlinks: /\\[\\[\'`;

# this array will receive the normalized node strings
my @nodes;

# mangle headers and add to list
foreach my $header (@headers) {

    # remove whitespace
    $header =~ s/\s*//g;

    # remove muse suffix and backlinks string, but retain the colon
    $header =~ s/$suffix:backlinks//;

    # remove leading directory
    $header = substr($header, 2);

    # remove asterisks
    $header =~ s/\*\*//g;

    # remove double square brackets
    $header =~ s/\[\[//g;
    $header =~ s/\]\]//g;

    # rearrange sequence
    $header =~ s%^(.*):(.*)%$2/$1%;

    # remove consecutive slashes (occurs in top-level file only)
    $header =~ s%^//%/%;

    push (@nodes, $header);
}

# sort the list to prearrange the structure
@nodes = sort @nodes;

# start output
print $mapstart;

# initialize variables used in the loop
my $prevroot;
my $prevnumslash = 0;
my $rootnumslash = 0;

# guess the main project page and insert it at the beginning of the nodes
# stack. I'm afraid this is a bad hack
my $mainnode = $nodes[0];
$mainnode =~ s%(.*)/[^/]*$%$1%;
unshift(@nodes, $mainnode);

# loop over all nodes
foreach my $node (@nodes) {
    # split node into a root part (up to the last /) and the node name proper
    my $root = $node;

    $root =~ s%(.*)/[^/]*$%$1%;
    $node =~ s%.*/([^/]*)$%$1%;

    # check node for unfinished tasks and todos
    # the following task states are recognized:
    # _ open
    # X done
    # > delegated
    # P pending
    # o in progress
    # C cancelled
    my $open = 0;
    my $done = 0;
    my $delegated = 0;
    my $pending = 0;
    my $inprogress = 0;
    my $cancelled = 0;
    my $todo = 0;
    
    # the task priorities
    my $pa = 0;
    my $pb = 0;
    my $pc = 0;

    # open the file corresponding to node
    my $file = "$plannerdir/$node$suffix";
    open IN, "< $file";
    while (<IN>) {
	# scan each line for tasks and todos
	if (/\#[ABC][ 0-9][ 0-9][ 0-9]_/) {
	    $open++;
	}
	elsif (/\#[ABC][ 0-9][ 0-9][ 0-9]X/) {
	    $done++;
	}
	elsif (/\#[ABC][ 0-9][ 0-9][ 0-9]>/) {
	    $delegated++;
	}
	elsif (/\#[ABC][ 0-9][ 0-9][ 0-9]P/) {
	    $pending++;
	}
	elsif (/\#[ABC][ 0-9][ 0-9][ 0-9]o/) {
	    $inprogress++;
	}
	elsif (/\#[ABC][ 0-9][ 0-9][ 0-9]C/) {
	    $cancelled++;
	}
	elsif (/\*\* [tT][oO][dD][oO]/) {
	    $todo++;
	}

	if (/\#A[ 0-9][ 0-9][ 0-9]/) {
	    $pa++;
	}
	elsif (/\#B[ 0-9][ 0-9][ 0-9]/) {
	    $pb++;
	}
	elsif (/\#C[ 0-9][ 0-9][ 0-9]/) {
	    $pc++;
	}
    }
    close IN;

    # the priority color in this node
    my $priority_color;

    if ($avg) {
	# priority c does not increase color
	# a turns on red, b turns on green
	my $fractiona;
	my $fractionb;

	if ($pa+$pb+$pc == 0) {
	    $fractiona = $fractionb = 0;
	}
	else {
	    $fractiona = ($pa*255)/($pa+$pb+$pc);
	    $fractionb = ($pb*255)/($pa+$pb+$pc);
	}

	$priority_color = sprintf(" COLOR=\"#%02x%02x00\"", $fractiona, $fractionb);
    }
    else {
	# use the highest task priority as the node priority
	if ($pa) {
	    $priority_color = $priority_a;
	}
	elsif ($pb) {
	    $priority_color = $priority_b;
	}
	else {
	    $priority_color = $priority_c;
	}
    }

    if (defined($prevroot)) {
	# subsequent cycles

	# count the slashes in the root
	my @chars = split(/ */, $root);

	$rootnumslash = 0;
	foreach my $char (@chars) {
	    if ($char eq "/") {
		$rootnumslash++;
	    }
	}

	if ($root eq $prevroot) {
	    # close previous node and start sibling node
	    print "</node>\n";
	    print "<node LINK=\"$baseurl$node.html\"$priority_color TEXT=\"$node\">\n";
	}
	elsif ($rootnumslash < $prevnumslash) {
	    # end of a node group. close as many previous node levels
	    # as necessary
	    for ($rootnumslash .. $prevnumslash) {
		print "</node>\n";
	    }

	    # start a fresh node
	    print "<node LINK=\"$baseurl$node.html\"$priority_color TEXT=\"$node\">\n";
	}
	else {
	    # child node
	    print "<node LINK=\"$baseurl$node.html\"$priority_color TEXT=\"$node\">\n";
	}
    }
    else {
	# first cycle
	print "<node LINK=\"$baseurl$node.html\"$priority_color TEXT=\"$node\">\n";
    }
    
    # add icons and color to the most recent node
    if ($open) {
	print $open_icon;
    }
    if ($done) {
	print $done_icon;
    }
    if ($delegated) {
	print $delegated_icon;
    }
    if ($pending) {
	print $pending_icon;
    }
    if ($inprogress) {
	print $inprogress_icon;
    }
    if ($cancelled) {
	print $cancelled_icon;
    }
    if ($todo) {
	print $todo_icon;
    }

    # save for next cycle
    $prevroot = $root;
    $prevnumslash = $rootnumslash;
}

# close remaining nodes and map
for (0 .. $prevnumslash) {
    print "</node>\n";
}
print "</map>\n";

exit 0;
