<?php
require_once "../func/init.php";
try{
	if ($METHOD === 'POST') {
		try {
		if($_POST['morada'] != "" && ctype_alpha($_POST['morada'])) {
			  $query = $db->prepare("INSERT INTO aluga VALUES (:morada, :codigo, :data_incio, :nif)");
			  $query->bindParam(':morada', $morada);
			  $query->bindParam(':codigo', $codigo);
			  $query->bindParam(':data_inicio', $data_inicio);
			  $query->bindParam(':nif', $nif);
			  $morada = $_POST['morada'];
			  $codigo = $_POST['codigo'];
			  $data_inicio = $_POST['data_inicio'];
			  $nif = $_POST['nif'];
			  $query->execute();
		 }
			else {
			  echo "error";
		 }
		 }
		 catch(Exception $ex) {
		echo "<html><h1>ERROR</h1></html>";
		 } 	
	} else if ($METHOD === 'PUT') {
		 //TODO: editar um edificio
		 try {

		 } catch(Exception $e) {
			 echo "Error: Failed to complete the deletion...";
			 exit();
		 }
	} else if ($METHOD === 'DELETE') {

		try {
		if($_POST['morada'] != "" && ctype_alpha($_POST['morada'])) {
			  $query = $db->prepare("DELETE FROM aluga WHERE morada=:morada AND codigo:codigo AND data_incio=:data_incio AND nif=:nif)");
			  $query->bindParam(':morada', $morada);
			  $query->bindParam(':codigoi', $codigo);
			  $query->bindParam(':data_inicio', $data_inicio);
			  $query->bindParam(':nif', $nif);
			  $morada = $_POST['morada'];
			  $codigo_espaco = $_POST['codigo'];
			  $data_inicio = $_POST['data_inicio'];
			  $nif = $_POST['nif'];
			  $query->execute();
		 }
			else {
			  echo "error";
		 }
		 }
		 catch(Exception $ex) {
		echo "<html><h1>ERROR</h1></html>";
		 }
		
	}
	 else if ($METHOD === 'GET') {
		 echo "invalid request";
	} else {
		 echo "unknown request";
	}

	if (isset($_GET['callback']) && !empty($_GET['callback'])) {
		header('Location: '.$_GET['callback']);
		exit();
	}
}
catch(Exception $ex) {
	echo $ex-getMessage();
}
?>
