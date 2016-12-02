<?php
require_once "../func/init.php";
try{
	//begin transaction for rollback
	$db->beginTransaction();

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
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
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
		$result = $query->execute();
		if(!$result) {
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

	//begin transaction for rollback
	$db->commit();

	if (isset($_GET['callback']) && !empty($_GET['callback'])) {
		header('Location: '.$_GET['callback']);
		exit();
	}
}
catch(Exception $ex) {
	$db->rollBack();
	echo $ex->getMessage();
}
?>
