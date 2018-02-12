<?php
	if(isset($_POST["submit"])) {
		$n = $_POST["name"];
		$id = $_POST["id"];
		
		$f_y = $_POST["years_from"];
		$f_m = $_POST["months_from"];
		$f_d = $_POST["days_from"];
		$t_y = $_POST["years_to"];
		$t_m = $_POST["months_to"];
		$t_d = $_POST["days_to"];
		
		$su = $_POST["sunday"];
		$m = $_POST["monday"];
		$t = $_POST["tuesday"];
		$w = $_POST["wednesday"];
		$r = $_POST["thursday"];
		$f = $_POST["friday"];
		$sa = $_POST["saturday"];
		
		$rate = $_POST["rate"];
		
		$weekstart = $f_y."-".$f_m."-".$f_d;
		$weekend = $t_y."-".$t_m."-".$t_d;
		
		
		$output = shell_exec("TS_TOT_SU="$su" TS_TOT_MO="$m" TS_TOT_TU="$t" \
								TS_TOT_WE="$w" TS_TOT_TH="$r" TS_TOT_FR="$f" \
								TS_TOT_SA="$sa" TS_RATE="$rate" TS_NAME="$n" \
								TS_ID="$id" ./make-timsheet.sh");	
		
		
		header( "Location: $output" );
		}

?>
	