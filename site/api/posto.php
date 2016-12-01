<?php
require_once "../func/init.php";

if ($METHOD === 'POST') {
    try {
    if($_POST['morada'] != "" && ctype_alpha($_POST['morada'])) {
          $query = $db->prepare("INSERT INTO posto VALUES (:morada, :codigo, :codigo_espaco)");
          $query->bindParam(':morada', $morada);
          $query->bindParam(':codigo', $codigo);
          $query->bindParam(':codigo_espaco', $codigo_espaco);
          $morada = $_POST['morada'];
	  $codigo = $_POST('codigo');
	  $codigo = $_POST('codigo_espaco');
          $query->execute();
     }
        else {
          echo "error";
     }
     }
     catch(Exception $ex) {
	echo "<html><h1>ERROR</h1></html>";
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
          $query = $db->prepare("DELETE FROM posto WHERE morada=:morada AND codigo=:codigo AND codigo_espaco=:codigo_espaco
          $query->bindParam(':morada', $morada);
          $query->bindParam(':codigo', $codigo);
          $query->bindParam(':codigo_espaco', $codigo_espaco);
          $morada = $_POST['morada'];
	  $codigo = $_POST('codigo');
	  $codigo = $_POST('codigo_espaco');
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
