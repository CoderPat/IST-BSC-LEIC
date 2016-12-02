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
		$data_inicio = $_POST['data_inicio'];
		$data_fim = $_POST['data_fim'];
		$tarifa = $_POST['tarifa'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
		}
	} else if ($METHOD === 'PUT') {
	} else if ($METHOD === 'DELETE') {

		$query = $db->prepare("DELETE FROM oferta WHERE morada=:morada AND codigo=:codigo AND  data_inicio=:data_inicio");
		$query->bindParam(':morada', $morada);
		$query->bindParam(':codigo', $codigo);
		$query->bindParam(':data_inicio', $data_inicio);
		$morada = $_POST['morada'];
		$codigo = $_POST['codigo'];        
		$data_inicio = $_POST['data_inicio'];
		$result = $query->execute();
		if(!$result) {
			throw new Exception("Could not insert");
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
	http_response_code(412);
	$db->rollBack();
	if($ex->getCode() == 23000){
		echo "Oferta ja com esses atributos ja existe";
	} else if($ex->getCode == 42000){
		echo "Nao pode sobrepor ofertas sobre o mesmo alugavel";
	} else{
		echo $ex->getMessage();
	}
}
?>
