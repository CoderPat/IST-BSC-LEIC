<?php
require_once "../func/init.php";
try{
	//begin transaction for rollback
	$db->beginTransaction();
	if ($METHOD === 'POST') {
		$query = $db->prepare("INSERT INTO edificio VALUES (:morada)");
		$query->bindParam(':morada', $morada);
		$morada = $_POST['morada'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
	} else if ($METHOD === 'DELETE') {
		$query = $db->prepare("DELETE FROM edificio WHERE morada=(:morada)");
		$query->bindParam(':morada', $morada);
		$morada = $_POST['morada'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not delete");
		}
	}
	 else if ($METHOD === 'GET') {
		echo "invalid request";
	} else {
		echo "unknown request";
	}
	//commit everything
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
