<?php
require_once "../func/init.php";
try{
	//begin transaction for rollback
	$db->beginTransaction();
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO reserva VALUES (:numero)");
		$query->bindParam(':numero', $numero);
		$morada = $_POST['numero'];

		$query = $db->prepare("INSERT INTO estado VALUES (:numero, UNIX_TIMESTAMP(), :estado)");
		$query->bindParam(':time_stamp', $time_stamp);
		$query->bindParam(':estado', $estado);
		$time_stamp = $_POST['time_stamp'];
		$estado = $_POST['estado'];

		$query = $db->prepare("INSERT INTO aluga VALUES (:morada, :codigo, :data_incio, :nif, :numero)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':data_inicio', $data_inicio);
		$query->bindParam(':nif', $nif);
		$query->bindParam(':numero', $numero);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];
		$data_inicio = $_POST['data_inicio'];
		$nif = $_POST['nif'];
		$numero = $_POST['numero'];
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

		$query = $db->prepare("INSERT INTO estado VALUES (:numero, UNIX_TIMESTAMP(), 'Paga')");
		$query->bindParam(':numero', $numero);
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}

	} else if ($METHOD === 'DELETE') {
	} else if ($METHOD === 'GET') {
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
	http_response_code(412);
	$db->rollBack();
	echo $ex->getMessage();
}
?>
