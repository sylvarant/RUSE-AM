#! /usr/bin/perl -w

use 5.010;
use strict;


# Multi Language support
my %langs = ("scheme",".scm","ml",".ml");
my $counter = 0;


#################################
# Function :: getresult
# retrieve the answer of a test
#################################
sub getresult{

    my $content;
    open(my $fh, $_[0]) or die "cannot open file $_[0]";
    {
        local $/;
        $content = <$fh>;
    }
    close($fh);
    $content =~ s|\s+$||;
    $content;
}

# Start
say "Testing ...";

# New test series for each language
while ( my ($key, $value) = each(%langs)){
    
    # input files
    my @input = `ls test/$key/ | egrep "$value"`;

    #results
    my %problems = ();

    foreach (@input){
        my $file = $_;
        my $ex = `./ruse -l test/$key/$file`;
        chomp($ex);

        (my $solution = $file) =~ s/$value$/\.out/;
        my $an =  &getresult("./test/$key/".$solution); 

        # Test evaluation
        unless ($an ~~ $ex){
            $problems{$file} = "\033[1;31mFAIL\033[0m : ". $ex;
        }
    }

    # Language evaluation
    say "====================";

    say " => \033[1;33m". uc($key) ."\033[0m";
    if ((scalar keys %problems) == 0) {
        $counter++;
        say "\033[1;33m". (scalar @input). " out of " . (scalar @input) . " succeeded\033[0m";
    }else{
        say "\033[1;31m".((scalar @input ) - (scalar keys %problems)). " out of " . (scalar @input) . " succeeded\033[0m";
        for my $kkey ( (keys %problems) ) {
            say "@ $kkey => $problems{$kkey}";
        }
    }
    say "====================";
}

# Total evaluation
if($counter == (scalar keys %langs)){
    say "\033[1;33m"." RUSE is working !\033[0m";
}else{
    say "\033[1;31m"." RUSE is failing !\033[0m";
}

