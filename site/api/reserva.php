<?php
require_once "../func/init.php";
try{
	//begin transaction for rollback
	$db->beginTransaction();
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO reserva VALUES (:numero)");
		$query->bindParam(':numero', $numero);
		$numero = $_POST['numero'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}

		$query = $db->prepare("INSERT INTO estado VALUES (:numero, :time_stamp, :estado)");
		$query->bindParam(':numero', $numero);
		$query->bindParam(':time_stamp', $time_stamp);
		$query->bindParam(':estado', $estado);
		$time_stamp = date('Y/m/d H:i:s');
		$estado = $_POST['estado'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}

		$query = $db->prepare("INSERT INTO aluga VALUES (:morada, :codigo, :data_inicio, :nif, :numero)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':data_inicio', $data_inicio);
		$query->bindParam(':nif', $nif);
		$query->bindParam(':numero', $numero);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];
		$data_inicio = $_POST['data_inicio'];
		$nif = $_POST['nif'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
		$query = $db->prepare("INSERT INTO paga VALUES (:numero, :data, :metodo)");
		$query->bindParam(':numero', $numero);
		$query->bindParam(':data', $data);
		$query->bindParam(':metodo', $metodo);
		$numero = $_POST['numero'];
		$data = $_POST['data'];
		$metodo = $_POST['metodo'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
		$query = $db->prepare("INSERT INTO estado VALUES (:numero, :time_stamp, 'Paga')");
		$query->bindParam(':numero', $numero);
		$query->bindParam(':time_stamp', $time_stamp);
		$time_stamp = date('Y/m/d H:i:s');
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}

	} else if ($METHOD === 'DELETE') {
		throw new Exception("Invalid request");
	} else if ($METHOD === 'GET') {
		throw new Exception("Invalid request");
	} else {
		throw new Exception("Unknown request");
	}

	//commit the changes
	$db->commit();
	if (isset($_GET['callback']) && !empty($_GET['callback'])) {
		header('Location: '.$_GET['callback']);
		exit();
	}
}
catch(Exception $ex) {
	http_response_code(412);
	$db->rollBack();
	if($ex->getCode() == 23000){
		echo "Nao pode criar reservas duplicadas"
	} else if($ex->getCode() == 42000){
		echo "Nao pode pagar numa data antes do ultimo estado";
	} else{
		echo $ex->getMessage();
	}
}
?>
