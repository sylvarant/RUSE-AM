#! /usr/bin/perl -w

use 5.010;
use strict;

my %results = ();

# Start
say "Testing ...";

my @input = `ls test | egrep ".scm"`;

foreach (@input){
    my $file = $_;
    my $ex = `./ruse test/$file`;
    chomp($ex);
    open(my $fh,"test/".$file) or die("ERROR:: $!");
    my $first = <$fh>; #  no multiline answers supported yet
    chomp($first);
    close $fh;
    $first =~ s|\s+$||;
    $first =~ m|;(?<answ>.*)|;
    my $an = $+{answ};
    if($an ~~ $ex){
        $results{$file} = "OK";
    }else{
        $results{$file} = "\033[1;31mFAIL\033[0m : ". $ex;
    }
}

say "====================";
# evaluation
my $i = 0;
for my $key ( keys %results ) {
    if ( $results{$key} ~~ "OK"){
        $i++;
    }
}
if ($i == (scalar keys %results)) {
    say "\033[1;33m". $i. " out of " . (scalar keys %results) . " succeeded\033[0m";
}else{
    say "\033[1;31m". $i. " out of " . (scalar keys %results) . " succeeded\033[0m";
}
say "====================";
for my $key ( sort (keys %results) ) {
    say "$key => $results{$key}";
}


