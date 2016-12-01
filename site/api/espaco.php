<?php
require_once "../func/init.php";

if ($METHOD === 'POST') {
    try {
    if($_POST['morada'] != "" && ctype_alpha($_POST['morada']) && $_POST['codigo'] != "null" && ctype_alpha($_POST['codigo'])) {
          $query = $db->prepare("INSERT INTO espaco VALUES (:morada, :codigo)");
          $query->bindParam(':morada', $morada);
          $query->bindParam(':codigo', $codigo);
	  $morada = $_POST['morada'];
          $codigo = $_POST['codigo'];
	  $query->execute();
     }
        else {
          echo "error: invalid params";
     }
     }
     catch(Exception $ex) {
	echo "<html><h1>" . $ex->getMessage() . "</h1></html>";
     } 	
} else if ($METHOD === 'PUT') {
     //TODO: editar um edificio
     try {

     } catch(Exception $e) {
         echo "Error: Failed to complete the deletion...";
         exit();
     }
} else if ($METHOD === 'DELETE') {
try {
    if($_POST['morada'] != "" && ctype_alpha($_POST['morada'])) {
          $query = $db->prepare("DELETE FROM espaco WHERE morada=(:morada) AND codigo=(:codigo)");
          $query->bindParam(':morada', $morada);
	  $query->bindParam('codigo', $codigo);
          $morada = $_POST['morada'];
	  $codigo = $_POST['codigo'];
          $query->execute();
     }
        else {
          echo "error";
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

if (isset($_GET['callback']) && !empty($_GET['callback'])) {
    header('Location: '.$_GET['callback']);
    exit();
}
?>
