<?php
require_once "../func/init.php";
try{
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO oferta VALUES (:morada, :codigo, :data_inicio, :data_fim, :tarifa)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':data_inicio', $data_inicio);
		$query->bindParam(':data_fim', $data_fim);
		$query->bindParam(':tarifa', $tarifa);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];        
		$data_incio = $_POST['data_incio'];
		$data_fim = $_POST['data_fim'];
		$tarifa = $_POST['tarifa'];
		$query->execute();
		if($query != 0) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
		 //TODO: editar um edificio
		 try {

		 } catch(Exception $e) {
			 echo "Error: Failed to complete the deletion...";
			 exit();
		 }
	} else if ($METHOD === 'DELETE') {

		$query = $db->prepare("DELETE FROM oferta WHERE morada=:morada AND codigo=:codigo AND  data_inicio=:data_inicio)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':data_inicio', $data_inicio);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];        
		$data_incio = $_POST['data_incio'];
		$data_fim = $_POST['data_fim'];
		$tarifa = $_POST['tarifa'];
		$query->execute();
		if($query != 0) {
			throw new Exception("Could not insert");
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
