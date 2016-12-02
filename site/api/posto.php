<?php
require_once "../func/init.php";
try{
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO posto VALUES (:morada, :codigo, :codigo_espaco)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':codigo_espaco', $codigo_espaco);
		$morada = $_POST['morada'];
		$codigo = $_POST('codigo');
		$codigo = $_POST('codigo_espaco');
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
	} else if ($METHOD === 'DELETE') {
			  $query = $db->prepare("DELETE FROM posto WHERE morada=:morada AND codigo=:codigo");
			  $query->bindParam(':morada', $morada);
			  $query->bindParam(':codigo', $codigo);
			  $morada = $_POST['morada'];
			  $codigo = $_POST('codigo');
			  $result = $query->execute();
			  if(!$result) {
				throw new Exception("Could not delete");
			  }
	} else if ($METHOD === 'GET') {
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
	echo $ex->getMessage();
}
?>
