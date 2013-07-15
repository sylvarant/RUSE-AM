#! /usr/bin/perl -w

use 5.010;
use strict;

my @input = `ls | egrep ".scm"`;


foreach (@input){
    my $file = $_;
    open(my $fh,$file) or die("ERROR:: $!");
    my $first = <$fh>; #  no multiline answers supported yet
    chomp($first);
    close $fh;
    $first =~ s|\s+$||;
    $first =~ m|;(?<answ>.*)|;
    my $an = $+{answ};
    (my $solution = $file) =~ s/\.scm$/\.out/;
    open (MYFILE, ">$solution");
    print MYFILE $an."\n";
    close (MYFILE); 
}
