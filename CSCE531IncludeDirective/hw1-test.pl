#!/usr/bin/perl -w

# Perl script for testing CSCE 531 Homework 1 submissions on pluto

# Usage (must be run under the bash shell):
# $ hw1-test.pl --self-test <your_hw1_directory>
#
# For example:
# $ hw1-test.pl --self-test hw1

# Appends to file "comments.txt" in your submission directory

# This script must be run under the bash shell!

# edit as needed:

# directory holding the test files
$test_dir = "/home/fuzzley/git_repos/CSCE531IncludeDirective";
# $test_dir = "/Users/steve/courses/531handouts/hw1/test";

$classdir_prefix = "/class/csce531-";

$timeout = 11;			# seconds

# @simTestFiles = ("bigDFA", "biggerDFA", "handoutDFA", "randomDFA1",
#		 "randomDFA2", "randomDFA3", "randomDFA4", "randomDFA5");

############ You should not have to edit below this line ##############

# Check existence of test directory
die "No test directory $test_dir\n"
    unless -d $test_dir;

#sub main
{
    if (!@ARGV) {
	print "Usage:\n  hw1-test.pl --self-test <your_hw1_directory>\n\n";
	print "For example:\n  \$ ./hw1-test.pl --self-test hw1\n\n";
	print "This program must be invoked from the bash shell.\n";
	print "When in doubt, run \"bash\" first, then \"exit\" after.\n\n";
    }
    elsif ($ARGV[0] eq '--self-test') {
	shift @ARGV;
	die "hw1-test.pl: missing directory name\n"
	    unless @ARGV;

	$hw1_dir = shift @ARGV;

	if ($hw1_dir !~ /^(~|\/)/) {
	    # relative path name -- prepend pwd
	    $pwd = `pwd`;
	    chomp $pwd;
	    $hw1_dir = "$pwd/$hw1_dir";
	}
	# convert home directory-relative pathname
	$hw1_dir =~ s/^~/$ENV{HOME}/e;
	# strip off final slash, if any
	$hw1_dir =~ s/\/$//;

	$uname = 'self-test';
	process_user();
    }
    else {
	# using for grading -- give section and optionally user name
	$section = shift @ARGV;
	$submission_root = "$classdir_prefix$section/submissions";
	if (@ARGV) {
	    # process individually specified user(s)
	    while (@ARGV) {
		$uname = shift @ARGV;
		$hw1_dir = "$submission_root/$uname/hw1";
		die "No such directory: $hw1_dir\n"
		    unless -d $hw1_dir;
		process_user();
	    }
	}
	else {
	    opendir DIR, $submission_root
		or die "Cannot open submission directory $submission_root ($!)\n";
	    @usernames = readdir DIR;
	    closedir DIR;

	    while (@usernames) {
		$uname = shift @usernames;
		next if $uname =~ /^\./;
		$hw1_dir = "$submission_root/$uname/hw1";
		next unless -d $hw1_dir;
		process_user();
	    }
	}
    }
}


sub process_user {

    print(STDERR "Processing $uname\n\n");

    open(COMMENTS, ">> $hw1_dir/comments.txt")
	or die "Cannot open file $hw1_dir/comments.txt for appending ($!)\n";

    cmt("Comments for $uname -------- " . now() . "\n");

    print(STDERR "Changing to directory '$hw1_dir' ...");
    chdir $hw1_dir
	or die "\nCannot change to directory '$hw1_dir'\n";
    print(STDERR " okay\n");

    # Percent; start with 100 and decrease ...
    local $score = 100;

    # Check for illegal files
    cmt("Checking for automatically generated (illegal) files ...");
    opendir DIR, $hw1_dir
	or die "Cannot open hw1 directory $hw1_dir ($!)\n";
    @program_files = readdir DIR;
    closedir DIR;
    $flag = 0;
    while (@program_files) {
	$file = shift @program_files;
	if ($file eq 'babycpp' || $file eq 'lex.yy.c' || $file =~ /\.o$/) {
	    cmt("\n  removing automatically generated file: $file");
	    unlink $file;
	    $flag = 1;
	}
    }
    if ($flag) {
	cmt("\nIllegal files were found (-3%)\n");
	$score -= 3;
    }
    else {
	cmt(" none (OK)\n");
    }

    # try to compile
    cmt("Building the executable with \"make\" ...");
    $rc = system('make');
    if ($rc >> 8) {
	cmt(" failed (-20%)\n");
	$score -= 20;
    }
    else {
	cmt(" succeeded\n");
    }

    # Executable should exist.  Now test it.
    if (-e 'babycpp') {
	test_babycpp();
    }
    else {
	cmt("No executable found (-50%)\n");
	$score -= 50;
    }

    # try to clean up without error-checking
    cmt("Cleaning up ...");
    system('make clean');
    if (-e 'babycpp') {
	# if clean didn't at least remove the executable
	cmt("\n  removing executable by hand (-0%)\n");
	# $score -= 0;
	unlink 'babycpp';
    }
    else {
	cmt(" done\n");
    }

    $score = 0
	if $score < 0;

    report_summary();
    close COMMENTS;
    print(STDERR "\nDone.\nComments appended to $hw1_dir/comments.txt\n");
}


sub test_babycpp {

#    print(STDERR "Errors msgs for $uname:\n");
#    print(STDOUT "System msgs for $uname:\n");

    cmt("\nTesting babycpp with test files in $test_dir:\n\n");

#    $error_count = 0;

    for ($i = 1; (-e "$test_dir/test${i}.c"); $i++) {
	cmt("Running babycpp on test${i}.c; output sent to test${i}.i ...");
#	cmt("   ./babycpp $test_dir/test${i}.c > test${i}.i 2> test${i}-err.txt");
	eval {
	    local $SIG{ALRM} = sub { die "timed out\n" };
	    alarm $timeout;
	    $rc = system("./babycpp $test_dir/test${i}.c > test${i}.i 2> test${i}-err.txt");
	    alarm 0;
	};
	if ($@ && $@ eq "timed out\n") {
	    cmt(" $@ (-50%)");       # program timed out before finishing
	    $score -= 50;
	    unlink "test${i}-err.txt"
		if -e "test${i}-err.txt";
	    unlink "test${i}.i"
		if -e "test${i}.i";
	    next;
	}
	if ($rc >> 8) {
	    cmt(" finished abnormally (-0%)\n");
	    # $score -= 0;
	}
	else {
	    cmt(" finished normally\n");
	}
	if (-e "test${i}-err.txt") {
	    cmt("  Standard error output:\n  vvvvv\n");
	    $report = `cat test${i}-err.txt`;
	    unlink "test${i}-err.txt";
	    chomp $report;
	    cmt(inleft($report, '    ') . "\n  ^^^^^\n");
	}

	if (!(-e "test${i}.i")) {
	    cmt("  output file test${i}.i does not exist (-10%)\n");
	    $score -= 10;
	    next;
	}

	# compare 
	cmt("  test${i}.i exists; comparing with sol'n file with \"diff\":\n");
	cmt("    diff test${i}.i $test_dir/test${i}.i\n");
	$report = `diff test${i}.i $test_dir/test${i}.i`;
	unlink "test${i}.i";
	chomp $report;
	if ($report eq '') {
	    cmt("  [output files match -- OK]\n");
	}
	else {
	    cmt("  OUTPUT FILES DIFFER (-5%):\n  vvvvv\n" .
		inleft($report,'    ') . "  \n  ^^^^^\n");
	    $score -= 5;
	}
    }
}


sub report_summary {
    cmt("######################################################\n");
    cmt("Summary for $uname:\n");

#     foreach $base (@simTestFiles) {
# 	cmt("  $base: ");
# 	$errors = '';
# 	if (-e "${base}-err.txt") {
# 	    $errors = `cat ${base}-err.txt`;
# 	    chomp $errors;
# 	}
# 	elsif ($errors eq '') {
# 	    cmt(" ok");
# 	}
# 	else {
# 	    cmt(" error message(s) (check appropriateness):\n");
# 	    cmt($errors);
# 	}
# 	cmt("\n");
#     }
    cmt("Your current score is $score percent.\n");
    cmt("######################################################\n");
}


sub cmt {
    my ($str) = @_;
#    print $str;
    print(COMMENTS $str);
}

sub inleft
{
    my ($src,$inleftstr) = @_;
    $src =~ s/^/$inleftstr/mego;
    return $src;
}


sub now {
    my $ret;

    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
    $ret = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat')[$wday];
    $ret .= " ";
    $ret .= ('Jan','Feb','Mar','Apr','May','Jun','Jul',
	     'Aug','Sep','Oct','Nov','Dec')[$mon];
    $ret .= " $mday, ";
    $ret .= $year + 1900;
    $ret .= " at ${hour}:${min}:${sec} ";
    if ( $isdst ) {
	$ret .= "EDT";
    } else {
	$ret .= "EST";
    }
    return $ret;    
}
