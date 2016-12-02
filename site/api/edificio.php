<?php
require_once "../func/init.php";
try{
	if ($METHOD === 'POST') {
			  $query = $db->prepare("INSERT INTO edificio VALUES (:morada)");
			  $query->bindParam(':morada', $morada);
			  $morada = $_POST['morada'];
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
			  $query = $db->prepare("DELETE FROM edificio WHERE morada=(:morada)");
			  $query->bindParam(':morada', $morada);
			  $morada = $_POST['morada'];
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
	echo $ex-getMessage();
}
?>
