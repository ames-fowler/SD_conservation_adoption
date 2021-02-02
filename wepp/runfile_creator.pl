#!/usr/bin/perl
# script to create seperate hillslope run files for WEPP 
# so that each hillslope can be run seperatly in DOS
# BC 8-16-06


$climate_file = "cli/wepp_cli_rev.cli"; # Enter the name of climate file
$years = 30.0;	#  Enter the number of years in the simulation
$suffix_1 = "_pass.txt";
$suffix_2 = "_loss.txt";
$suffix_3 = "_wat.txt";
#$suffix_4 = "_dist.txt";
#$suffix_5 = "_yld.txt";
$suffix_6 = "_crop.txt";


open(INPUT, "<input2runfile_WW_B_F_MT.txt") || die("Cannot Open File");
	chop($_);
	
	while (<INPUT>) {
	($topazid,$weppid,$ofes,$area_cells,$soil,$management,$slope) = split(' ', $_);

open(OUTPUT, ">>H$weppid.run") || die("Cannot Create File");
	print OUTPUT "m\n"; 
	print OUTPUT "y\n"; 
	print OUTPUT "1\n"; 
	print OUTPUT "1\n"; 
	print OUTPUT "y\n"; 
	print OUTPUT "$weppid$suffix_1\n"; # name of hillslope pass output file
	print OUTPUT "1\n"; # abbreviated annual results
	print OUTPUT "n\n"; # no, don't create an initial scenario output file
	print OUTPUT "$weppid$suffix_2\n"; # name of hillslope soil loss output file 
	print OUTPUT "y\n"; # yes, create a hillslope water balance output file
	print OUTPUT "$weppid$suffix_3\n"; # name of hillslope water balance output file
	print OUTPUT "n\n"; # no don't create a crop output file
#	print OUTPUT "$weppid$suffix_6\n"; # name of crop output file
	print OUTPUT "n\n"; # no don't create a soil property output file
	print OUTPUT "n\n"; # no don't create a distance and sediment loss output file
	print OUTPUT "n\n"; # no don't create a large graphics output file
	print OUTPUT "n\n"; # no don't create an event by event output file
	print OUTPUT "n\n"; # no don't create an element output file
	print OUTPUT "n\n"; # no don't create a final summary output file
	print OUTPUT "n\n"; # no don't create a daily winter output file
	print OUTPUT "n\n"; # no don't create a plant yield output file
	print OUTPUT "$management\n"; # name of hillslope management file
	print OUTPUT "$slope\n"; # name of hillslope slope file
	print OUTPUT "$climate_file\n"; # name of hillslope climate file
	print OUTPUT "$soil\n"; # name of hillslope soil file
	print OUTPUT "0\n"; # no irrigation will be simulated
	print OUTPUT "$years\n"; # number of years to simulate
	print OUTPUT "0\n";

close OUTPUT;

} 
close INPUT;
