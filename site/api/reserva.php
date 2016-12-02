<?php
require_once "../func/init.php";
try{
	//begin transaction for rollback
	$db->beginTransaction();
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO aluga VALUES (:morada, :codigo, :data_incio, :nif)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':data_inicio', $data_inicio);
		$query->bindParam(':nif', $nif);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];
		$data_inicio = $_POST['data_inicio'];
		$nif = $_POST['nif'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
		header("lixo:lixo");
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
		$query->bindParam(':time_stamp', $data);
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
	$db->rollBack();
	echo $ex->getMessage();
}
?>
