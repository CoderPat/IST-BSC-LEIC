<?php
require_once "../func/init.php";
try{
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO espaco VALUES (:morada, :codigo)");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];
		$result = $query->execute();
		if(!$result != 0) {
			throw new Exception("Could not insert");
		}
		
	} else if ($METHOD === 'PUT') {
	} else if ($METHOD === 'DELETE') {
		$query = $db->prepare("DELETE FROM espaco WHERE morada=(:morada) AND codigo=(:codigo)");
		$query->bindParam(':morada', $morada);
		$query->bindParam('codigo', $codigo);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];
		$query->execute();
		if($query != 0) {
			throw new Exception("Could not delete");
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
	echo $ex->getMessage();
}
?>
