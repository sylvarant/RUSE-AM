#! /usr/bin/perl -w

use 5.010;
use strict;
use Getopt::Long;

#ignore no get opts options
Getopt::Long::Configure("pass_through");
 
my ($secure);

# By default output insecure conversion
GetOptions('secure' => \$secure);

if($secure){
    say "#ifdef SECURE";
}else{
    say "#ifndef SECURE";
}
say "#include \"global.h\"";

say "char N(input_byte)[]={";


open(my $fh,$ARGV[0]) or die("ERROR:: $!");
my @array = <$fh>; 

my $tot_c=0;
for my $i (0 .. $#array){
    my $str = $array[$i];
    my @char_array = split(//,"$str");
    for my $x (0 .. $#char_array){
        my $char = $char_array[$x];
        $tot_c++;
        printf ("0x%02x",ord($char));
        unless(($i == $#array) and  ($x == $#char_array)){
            print ", ";
        }
        if(($tot_c % 8) == 0){
            print "\n";
        }
    }
}

say "};";
say "#endif";

close $fh; 
