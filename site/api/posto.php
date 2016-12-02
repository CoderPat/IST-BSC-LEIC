<?php
require_once "../func/init.php";
try{
	//begin transaction for rollback
	$db->beginTransaction();
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO alugavel VALUES (:morada, :codigo, :foto)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':foto', $foto);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];
		$foto = $_POST['foto'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}

		$query = $db->prepare("INSERT INTO arrenda VALUES (:morada, :codigo, :nif)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':nif', $nif);
		$nif = $_POST['nif'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}

		$query = $db->prepare("INSERT INTO posto VALUES (:morada, :codigo, :codigo_espaco)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':codigo_espaco', $codigo_espaco);
		$codigo_espaco = $_POST['codigo_espaco'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
	} else if ($METHOD === 'DELETE') {
			  $query = $db->prepare("DELETE FROM alugavel WHERE morada=:morada AND codigo=:codigo");
			  $query->bindParam(':morada', $morada);
			  $query->bindParam(':codigo', $codigo);
			  $morada = $_POST['morada'];
			  $codigo = $_POST['codigo'];
			  $result = $query->execute();
			  if(!$result) {
				throw new Exception("Could not delete");
			  }
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
