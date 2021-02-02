#!/usr/bin/perl

#  This program takes output files from WEPP hillslope simulations and 
#  creates watershed daily water balance and sediment balance factors
#  for each stream segment in a watershed

#  The program requires as input "*_wat.txt" and "*_pass.txt" from each hillslope

#  Output from the program will be "*.wat" water balance files from each hillslope,
#  "*.sed" sediment detachment, transport, and delivery files for each hillslope,
#  and totalwatsed.txt file which merges both the *.wat and the *.sed file.

#  Erin Brooks 4/6/2006

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#  CREATE DAILY WATER (*.wat) OUTPUT FILES FOR EACH HILLSLOPE


#  This subroutine takes WEPP water output files for hillslopes with multiple OFEs and converts it
#  to a WEPP water output file for the entire hillslope.

#  The program looks for all files ending in "_wat.txt" and converts them to "*.wat" files
#  Caution the program deletes all files ending in "*.wat" before it starts and .sed
print `rm water_file_list`;
print `rm *.sed`;
print `ls *_wat.txt >water_file_list`;
print `rm *.wat`;


print "Creating water output files (*.out) for each hillslope\n";

open (LIST, "<water_file_list") || die("Can't open file\n");

while (<LIST>) {
	chop($_);
	($water_file) = split;
	($slope_number) = split(/_wat.txt/, $water_file);
#	($slope_number) = substr($junk, 1);
#	print "$slope_number\n";

	open(OUT, ">>$slope_number.wat") || die("Cannot Open File");

	open (WATER, "<$water_file") || die("Can't open file\n");
	chop($_);
	$doy_last = 1.0;
	$ofe_last = 1.0;
	$year_last = 1989.0;
	$precip_total = 0.0;
	$rain_melt_total = 0.0;
	$transpiration_total = 0.0;
	$evap_total = 0.0;
	$perc_total = 0.0;

	$junk1=<WATER>;
	$junk2=<WATER>;
	$junk3=<WATER>;
	$junk4=<WATER>;
	$junk5=<WATER>;
	$junk6=<WATER>;
	$junk7=<WATER>;
	$junk8=<WATER>;
	$junk9=<WATER>;
	$junk10=<WATER>;
	$junk11=<WATER>;
	$junk12=<WATER>;
	$junk13=<WATER>;
	$junk14=<WATER>;
	
	while (<WATER>) {
	($ofe,$doy,$year,$precip,$rain_melt,$runoff_out,$transpiration,$evap,$perc,$runoff_in,$lateral,$storage) = split(' ', $_);
			if ($doy_last == $doy) {
				$precip_total = $precip_total + $precip;
				$rain_melt_total = $rain_melt_total + $rain_melt;
				$transpiration_total = $transpiration_total + $transpiration;
				$evap_total = $evap_total + $evap;
				$perc_total = $perc_total + $perc;
				$runoff_out_last = $runoff_out;
				$lateral_last = $lateral;
				$storage_last = $storage;
				$year_last = $year;
				$ofe_last = $ofe;
			}
			else {
				$precip_avg = $precip_total / $ofe_last;
				$rain_melt_avg = $rain_melt_total / $ofe_last;
				$transpiration_avg = $transpiration_total / $ofe_last;
				$evap_avg = $evap_total / $ofe_last;
				$perc_avg = $perc_total / $ofe_last;
				$runoff_avg = $runoff_out_last / $ofe_last;
				$lateral_avg = $lateral_last / $ofe_last;
				print OUT "$doy_last $year_last $ofe_last $precip_avg $rain_melt_avg $transpiration_avg $evap_avg $perc_avg $runoff_avg $lateral_avg $storage_last\n";
				$precip_total = $precip;
				$rain_melt_total = $rain_melt;
				$transpiration_total = $transpiration;
				$evap_total = $evap;
				$perc_total = $perc;
				$runoff_out_last = $runoff_out;
				$lateral_last = $lateral;
				$storage_last = $storage;
				$doy_last = $doy;
				$ofe_last = $ofe;
				$year_last = $year;
			};
		};
	$precip_avg = $precip_total / $ofe_last;
	$rain_melt_avg = $rain_melt_total / $ofe_last;
	$transpiration_avg = $transpiration_total / $ofe_last;
	$evap_avg = $evap_total / $ofe_last;
	$perc_avg = $perc_total / $ofe_last;
	$runoff_avg = $runoff_out_last / $ofe_last;
	$lateral_avg = $lateral_last / $ofe_last;
	print OUT "$doy_last $year_last $ofe_last $precip_avg $rain_melt_avg $transpiration_avg $evap_avg $perc_avg $runoff_avg $lateral_avg $storage_last\n";
	close OUT;
	close WATER; 
};
close LIST;

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#  CREATE DAILY SEDIMENT LOAD (*.sed) OUTPUT FILES FOR EACH HILLSLOPE

#  This subroutine takes WEPP hillslope pass output files and converts them
#  to WEPP sed output files "*.sed" with simulated sediment load for each day of the simulation.

#  The program looks for all files ending in "_pass.txt" and converts them to "*.sed" files
#  Caution the program deletes all files ending in "*.sed" before it starts

print `rm pass_file_list`;

print `ls *_pass.txt >pass_file_list`;
print `rm *.sum`;

print "Creating sediment output files (*.sed) for each hillslope\n";

open (LIST, "<pass_file_list") || die("Can't open file\n");

while (<LIST>) {
	chop($_);
	($pass_file) = split;
	($slope_number) = split(/_pass.txt/, $pass_file);
#	($slope_number) = substr($junk, 1);

	$year_previous = 1989.0; # ENTER THE STARTING YEAR OF THE SIMULATION
	$lat_cuml_mm = 0.0;
	$ro_cuml_mm = 0.0;
	$sdet_cuml_kg = 0.0;
	$sdep_cuml_kg = 0.0;
	$sdel_cuml_kg = 0.0;
	$sdel_cuml_kg_c1 = 0.0;
	$sdel_cuml_kg_c2 = 0.0;
	$sdel_cuml_kg_c3 = 0.0;
	$sdel_cuml_kg_c4 = 0.0;
	$sdel_cuml_kg_c5 = 0.0;

#	print "$slope_number\n";

	open(OUT, ">>$slope_number.sed") || die("Cannot Open File");

	open (SED, "<$pass_file") || die("Can't open file\n");
	$junk1=<SED>;
	$junk2=<SED>;
	$junk3=<SED>;
	($area,$junk4) = split(/\s/, $junk3);
	$junk5=<SED>;
	while (<SED>) {
	($desc,$v1,$v2,$v3,$v4,$v5,$v6,$v7,$v8,$v9,$v10,$v11,$v12,$v13,$v14,$v15,$v16,$v17,$v18,$v19,$v20,$v21,$v22) = split(' ', $_);
		if ($desc eq "SUBEVENT") {
			$year = $v1;
			$doy = $v2;
			$lateral_m3 = $v4;
			$runoff_m3 = 0.0;
			$sed_det_kg = 0.0;
			$sed_dep_kg = 0.0;
			$sed_del_kg = 0.0;
			$sed_del_kg_c1 = 0.0;
			$sed_del_kg_c2 = 0.0;
			$sed_del_kg_c3 = 0.0;
			$sed_del_kg_c4 = 0.0;
			$sed_del_kg_c5 = 0.0;
			}
		elsif ($desc eq "NO") {
			$year = $v2;
			$doy = $v3;
			$lateral_m3 = 0.0;
			$runoff_m3 = 0.0;
			$sed_det_kg = 0.0;
			$sed_dep_kg = 0.0;
			$sed_del_kg = 0.0;
			$sed_del_kg_c1 = 0.0;
			$sed_del_kg_c2 = 0.0;
			$sed_del_kg_c3 = 0.0;
			$sed_del_kg_c4 = 0.0;
			$sed_del_kg_c5 = 0.0;
			}
		else	{
			$year = $v1;
			$doy = $v2;
			$runoff_m3 = $v7;
			$lateral_m3 = $v9;
			$sed_det_kg = $v11;
			$sed_dep_kg = $v12;
			$sed_del_kg_c1 = $runoff_m3 * $v13;
			$sed_del_kg_c2 = $runoff_m3 * $v14;
			$sed_del_kg_c3 = $runoff_m3 * $v15;
			$sed_del_kg_c4 = $runoff_m3 * $v16;
			$sed_del_kg_c5 = $runoff_m3 * $v17;
			$sed_del_kg = $sed_del_kg_c1 + $sed_del_kg_c2 + $sed_del_kg_c3 + $sed_del_kg_c4 + $sed_del_kg_c5;
		};
	if ($year == $year_previous) {  }
#	open(OUT2, ">>hill_summary_$year_previous.sum") || die("Cannot Open File");
	else {
	open(OUT2, ">>hill_summary.sum") || die("Cannot Open File");
	print OUT2 "$slope_number $year_previous $area $lat_cuml_mm $ro_cuml_mm $sdet_cuml_kg $sdep_cuml_kg $sdel_cuml_kg $sdel_cuml_kg_c1 $sdel_cuml_kg_c2 $sdel_cuml_kg_c3 $sdel_cuml_kg_c4 $sdel_cuml_kg_c5\n";
	$year_previous=$year;
	$lat_cuml_mm = 0.0;
	$ro_cuml_mm = 0.0;
	$sdet_cuml_kg = 0.0;
	$sdep_cuml_kg = 0.0;
	$sdel_cuml_kg = 0.0;
	$sdel_cuml_kg_c1 = 0.0;
	$sdel_cuml_kg_c2 = 0.0;
	$sdel_cuml_kg_c3 = 0.0;
	$sdel_cuml_kg_c4 = 0.0;
	$sdel_cuml_kg_c5 = 0.0;
	close OUT2;
	};
	$lat_cuml_mm = $lat_cuml_mm + $lateral_m3 / $area * 1000.0;
	$ro_cuml_mm = $ro_cuml_mm + $runoff_m3 / $area * 1000.0;
	$sdet_cuml_kg = $sdet_cuml_kg + $sed_det_kg;
	$sdep_cuml_kg = $sdep_cuml_kg + $sed_dep_kg;
	$sdel_cuml_kg = $sdel_cuml_kg + $sed_del_kg;
	$sdel_cuml_kg_c1 = $sdel_cuml_kg_c1 + $sed_del_kg_c1;
	$sdel_cuml_kg_c2 = $sdel_cuml_kg_c2 + $sed_del_kg_c2;
	$sdel_cuml_kg_c3 = $sdel_cuml_kg_c3 + $sed_del_kg_c3;
	$sdel_cuml_kg_c4 = $sdel_cuml_kg_c4 + $sed_del_kg_c4;
	$sdel_cuml_kg_c5 = $sdel_cuml_kg_c5 + $sed_del_kg_c5;

	print OUT "$doy $year $area $lateral_m3 $runoff_m3 $sed_det_kg $sed_dep_kg $sed_del_kg $sed_del_kg_c1 $sed_del_kg_c2 $sed_del_kg_c3 $sed_del_kg_c4 $sed_del_kg_c5\n";
	};
	close OUT;
	close SED;

	open(OUT2, ">>hill_summary.sum") || die("Cannot Open File");
	print OUT2 "$slope_number $year_previous $area $lat_cuml_mm $ro_cuml_mm $sdet_cuml_kg $sdep_cuml_kg $sdel_cuml_kg $sdel_cuml_kg_c1 $sdel_cuml_kg_c2 $sdel_cuml_kg_c3 $sdel_cuml_kg_c4 $sdel_cuml_kg_c5\n";
	close OUT2;
	 
};
close LIST;


use strict;

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#  CREATE DAILY WATER AND SEDIMENT LOAD (*.wsed) OUTPUT FILES FOR EACH STREAM SEGMENT

#  This routine takes water (*.wat) and sediment (*.sed) files for hillslopes and creates
#  A totals file.

#  Written by Loridee Wetzel
#  April 2009
##########################################################################################


#get list of files  - for now we will use Erin's method of having a txt dir

#open NAMELIST, "<watlist.txt" or die "Can't open NAMELIST $!\n";

#my @fnames = <NAMELIST>;
#close NAMELIST;

#print "I'm working ";  #For use with the count in the while loop
#glob filenames into array @openwats and @openseds
my @openwats = glob "*.wat";
my @openseds = glob "*.sed";
my $cnt = @openwats;
my @info;

#Open data files
my $i = 0;

while ($i <= $#openwats) {
#    print "- ";  #Allows a count of how many file pairs were used
    open WAT, "$openwats[$i]" or die "Can't open WAT $!\n";
    my @new = <WAT>;
    close WAT;

    open SED, "$openseds[$i]" or die "Can't open SED $!\n";
    my @sed = <SED>;
    close SED;

    #combine WAT and SED files to only the fields we want
    my $length = @new;
    my $x = 0;


    while ($x < $length) {
        my @newdaily = split " ", $new[$x];
        my @seddaily = split " ", $sed[$x];
        splice @newdaily, 2,1,$seddaily[2];
        push @newdaily, $seddaily[5], $seddaily[6], $seddaily[7], $seddaily[8], $seddaily[9], $seddaily[10], $seddaily[11], $seddaily[12];
        
        #perform necessary maths on each field
        splice @newdaily, 3,1, ($newdaily[3]*$newdaily[2]/1000.0);
        splice @newdaily, 4,1, ($newdaily[4]*$newdaily[2]/1000.0);
        splice @newdaily, 5, 1, ($newdaily[5]*$newdaily[2]/1000.0);
        splice @newdaily, 6,1, ($newdaily[6]*$newdaily[2]/1000.0);
        splice @newdaily, 7,1, ($newdaily[7]*$newdaily[2]/1000.0);
        splice @newdaily, 8,1, ($newdaily[8]*$newdaily[2]/1000.0);
        splice @newdaily, 9,1, ($newdaily[9]*$newdaily[2]/1000.0);
        splice @newdaily, 10,1, ($newdaily[10]*$newdaily[2]/1000.0);
        
        #use array of arrays to store data
            if ($i == 0) {
                $info[$x] = [@newdaily];
            }
            else {
                for my $each (2...$#newdaily){
                    splice @{$info[$x]}, $each, 1, ($info[$x][$each] + $newdaily[$each]);
                }
            }
        $x++;  
    }
    $i++;
}

#Print total information to file.
open INFOFILE, ">", "totalwatsed.txt", or die "Can't open INFOFILE $!\n";   
    print INFOFILE "DOY Year Area_sq_m Precip_m3 Rain_melt_m3 Transpiration_m3 Evaporation_m3 Percolation_m3 Runoff_m3 Lateral_m3 Soil_Water_Storage_m3 Detached_sediment_kg Deposited_sediment_kg Delivered_sediment delivered_clay_kg delivered_silt_kg delivered_small_aggregates_kg delivered_large_aggregates_kg delivered_sand_kg streamflow\n";
    for my $done (0...$#info){
       print INFOFILE "@{$info[$done]} \n";
    }
close INFOFILE;

print `rm *.sed`;
print `rm *.wat`;

#print "I've Finished!"  #a nice closure when using the while count

