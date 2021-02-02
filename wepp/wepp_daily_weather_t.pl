#!/usr/bin/perl

#  This program takes a WEPP climate file "*.cli" and an input file "slope_prism.inp"
#  then shifts tmin and tmax using the average prism value relative to the tmin and tmax at the input climate station
#  Precipitation is calculated as the ratio of the map at the hillslope and the map at the input climate station
#  After changes a new climate file called "wepp_cli_rev.cli" is created
#  Erin Brooks 8/19/08


# ************** ENTER THE NAME OF THE SLOPE_MONTHLY_PRISM_*.TXT FILE BELOW ******************

open (INPUT, "<./cli/slope_monthly_prism_ward_t.txt") || print"Can't open file\n";
	chop($_);
	$map_list = <INPUT>;
	$tmax_list = <INPUT>;
	$tmin_list = <INPUT>;
	@precip_ws = split(' ', $map_list);
	@tmax_ws = split(' ', $tmax_list);
	@tmin_ws = split(' ', $tmin_list);

	$labels = <INPUT>;
	
while (<INPUT>) {
	($slope_id,$map,$ma_tmax,$ma_tmin) = split(' ', $_);
	$map_slope_list = <INPUT>;
	$tmax_slope_list = <INPUT>;
	$tmin_slope_list = <INPUT>;
	@precip_slope = split(' ', $map_slope_list);
	@tmax_slope = split(' ', $tmax_slope_list);
	@tmin_slope = split(' ', $tmin_slope_list);
	
open(OUT, ">./cli/wepp_cli_rev.cli") || die("Cannot Open File\n");


#  **************  ENTER THE NAME OF THE BASE CLIMATE FILE BELOW ***********************


open (CLIMATE, "<./cli/ST_john_obs.cli") || die(print"Can't open file\n");
	$line_1 = <CLIMATE>;
	$line_2 = <CLIMATE>;
	$line_3 = <CLIMATE>;
	$line_4 = <CLIMATE>;
	$line_5 = <CLIMATE>;
	$line_6 = <CLIMATE>;
	$line_7 = <CLIMATE>;
	$line_8 = <CLIMATE>;
	$line_9 = <CLIMATE>;
	$line_10 = <CLIMATE>;
	$line_11 = <CLIMATE>;
	$line_12 = <CLIMATE>;
	$line_13 = <CLIMATE>;
	$line_14 = <CLIMATE>;
	$line_15 = <CLIMATE>;
	print OUT "$line_1";
	print OUT "$line_2";
	print OUT "$line_3";
	print OUT "$line_4";
	print OUT "$line_5";
	print OUT "$line_6";
	print OUT "$line_7";
	print OUT "$line_8";
	print OUT "$line_9";
	print OUT "$line_10";
	print OUT "$line_11";
	print OUT "$line_12";
	print OUT "$line_13";
	print OUT "$line_14";
	print OUT "$line_15";
while (<CLIMATE>) {
	($day,$month,$year,$precip,$duration,$tp,$ip,$tmax,$tmin,$rad,$w_vel,$w_dir,$tdew,$tmax_snotel,$tmin_snotel,$precip_snotel) = split(' ', $_);

#print "@precip_ws[$month] @tmax_ws[$month] @tmin_ws[$month]\n";

		$tmax_rev = $tmax - @tmax_ws[$month] + @tmax_slope[$month];
		$tmin_rev = $tmin - @tmin_ws[$month] + @tmin_slope[$month];
		$precip_rev = $precip * @precip_slope[$month] / @precip_ws[$month];
	print OUT "$day  $month  $year  $precip_rev  $duration $tp  $ip  $tmax_rev  $tmin_rev  $rad  $w_vel  $w_dir  $tdew\n";
};
close OUT;
close CLIMATE;


=head 
open(OUT2, ">./man/rev_$man_file") || die("Cannot Open File\n");
open (MANAGE, "<./man/$man_file") || die("Can't open file\n");
#	chop($_);
	$line_1 = <MANAGE>;
	$line_2 = <MANAGE>;
	$line_3 = <MANAGE>;
	$line_4 = <MANAGE>;
	$line_5 = <MANAGE>;
	$line_6 = <MANAGE>;
	$line_7 = <MANAGE>;  # number of ofes
	$line_8 = <MANAGE>;  # number of years
	$line_9 = <MANAGE>;
	$line_10 = <MANAGE>;
	$line_11 = <MANAGE>;
	$line_12 = <MANAGE>;
	$line_13 = <MANAGE>;
	$line_14 = <MANAGE>;
	$line_15 = <MANAGE>;
	$line_16 = <MANAGE>;
	$line_17 = <MANAGE>;
	$line_18 = <MANAGE>;
	$line_19 = <MANAGE>;
	$line_20 = <MANAGE>;
	$line_21 = <MANAGE>;
	$line_22 = <MANAGE>;
	$line_23 = <MANAGE>;
($canopy_cover_coeff,$canopy_ht,$biomass_energy,$base_temp,$param_flat_residue,$dd_emergance,$critical_biomass,$residue_ht,$cc_senescence,$dia_plant) = split(' ', $line_23);
	$line_24 = <MANAGE>;
	($heat_index,$biomass_senescence,$rad_xtinct,$reside_adj,$DW_ff,$dd_growing,$HI,$max_canopy_ht) = split(' ', $line_24);
	$line_25 = <MANAGE>;
	$line_26 = <MANAGE>;
	($decomp_abv,$decomp_below,$optimal_temp,$drought_tol,$plant_spacing,$max_root_depth,$root_shoot_ratio,$max_root_mass,$senescence_period,$max_temp) = split(' ', $line_26);
	$line_27 = <MANAGE>;
	($t_freeze,$max_lai,$opt_yield) = split(' ', $line_27);
	$line_28 = <MANAGE>;
	$line_29 = <MANAGE>;
	$line_30 = <MANAGE>;
	$line_31 = <MANAGE>;
	$line_32 = <MANAGE>;
	$line_33 = <MANAGE>;
	$line_34 = <MANAGE>;
	$line_35 = <MANAGE>;
	$line_36 = <MANAGE>;
	$line_37 = <MANAGE>;
	$line_38 = <MANAGE>;
	$line_39 = <MANAGE>;
	$line_40 = <MANAGE>;
	$line_41 = <MANAGE>;
	$line_42 = <MANAGE>;
	$line_43 = <MANAGE>;
	$line_44 = <MANAGE>;
	$line_45 = <MANAGE>;
	$line_46 = <MANAGE>;
	$line_47 = <MANAGE>;
	$line_48 = <MANAGE>;
	$line_49 = <MANAGE>;
	$line_50 = <MANAGE>;
	($BD_after_tillage,$initial_canopy_cov,$days_since_tillage,$days_since_harvest,$initial_frost,$interrill_cov) = split(' ', $line_50);
	$line_51 = <MANAGE>;
	$line_52 = <MANAGE>;
	$line_53 = <MANAGE>;
	($cuml_rain,$ridge_ht,$rill_cov,$roughness,$rill_spacing) = split(' ', $line_53);
	$line_54 = <MANAGE>;
	$line_55 = <MANAGE>;
	($snow_depth,$thaw_depth,$depth_second_tillage,$depth_primary_tillage,$rill_width) = split(' ', $line_55);
	$line_56 = <MANAGE>;
	($dead_root_mass,$submerged_residue_mass) = split(' ', $line_56);

	print OUT2 "$line_1";
	print OUT2 "$line_2";
	print OUT2 "$line_3";
	print OUT2 "$line_4";
	print OUT2 "$line_5";
	print OUT2 "$line_6";
	print OUT2 "$line_7";
	print OUT2 "$line_8";
	print OUT2 "$line_9";
	print OUT2 "$line_10";
	print OUT2 "$line_11";
	print OUT2 "$line_12";
	print OUT2 "$line_13";
	print OUT2 "$line_14";
	print OUT2 "$line_15";
	print OUT2 "$line_16";
	print OUT2 "$line_17";
	print OUT2 "$line_18";
	print OUT2 "$line_19";
	print OUT2 "$line_20";
	print OUT2 "$line_21";
	print OUT2 "$line_22";
	$canopy_cover_coeff = $pct_cover;
	print OUT2 "$canopy_cover_coeff  $canopy_ht  $biomass_energy  $base_temp  $param_flat_residue  $dd_emergance  $critical_biomass  $residue_ht  $cc_senescence  $dia_plant\n";
	$biomass_senescence = $pct_cover * 0.10 + 0.78;
	print OUT2 "$heat_index  $biomass_senescence  $rad_xtinct  $reside_adj  $DW_ff  $dd_growing  $HI  $max_canopy_ht\n";
	print OUT2 "$line_25";
	print OUT2 "$decomp_abv  $decomp_below  $optimal_temp  $drought_tol  $plant_spacing  $max_root_depth  $root_shoot_ratio  $max_root_mass  $senescence_period  $max_temp\n";
	print OUT2 "$t_freeze  $max_lai  $opt_yield\n";
	print OUT2 "$line_28";
	print OUT2 "$line_29";
	print OUT2 "$line_30";
	print OUT2 "$line_31";
	print OUT2 "$line_32";
	print OUT2 "$line_33";
	print OUT2 "$line_34";
	print OUT2 "$line_35";
	print OUT2 "$line_36";
	print OUT2 "$line_37";
	print OUT2 "$line_38";
	print OUT2 "$line_39";
	print OUT2 "$line_40";
	print OUT2 "$line_41";
	print OUT2 "$line_42";
	print OUT2 "$line_43";
	print OUT2 "$line_44";
	print OUT2 "$line_45";
	print OUT2 "$line_46";
	print OUT2 "$line_47";
	print OUT2 "$line_48";
	print OUT2 "$line_49";
	print OUT2 "$BD_after_tillage  $pct_cover  $days_since_tillage  $days_since_harvest  $initial_frost  $interrill_cov\n";
	print OUT2 "$line_51";
	print OUT2 "$line_52";
	print OUT2 "$cuml_rain  $ridge_ht  $rill_cov  $roughness  $rill_spacing\n";
	print OUT2 "$line_54";
	print OUT2 "$snow_depth  $thaw_depth  $depth_second_tillage  $depth_primary_tillage  $rill_width\n";
	print OUT2 "$dead_root_mass  $submerged_residue_mass\n";

while (<MANAGE>) {
#	chop($_);
	($line) = split('xxx', $_);
#		$line = <MANAGE>;
		print OUT2 "$line";
};
close OUT2;
close MANAGE;
=cut
print $slope_id;
print `./wepp1000.exe <./run/H$slope_id.run>> error_1_300.err`;
print `rm ds*.txt\n`;
};

close INPUT;

#print `wepp_UI_post_processing.pl\n`;

