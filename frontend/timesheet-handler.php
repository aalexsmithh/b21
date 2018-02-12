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
		
		echo $f_y;
		echo $f_m;
		echo $f_d;
		
// 		$output = shell_exec("./main.sh $entry");	
		
		}

?>
	