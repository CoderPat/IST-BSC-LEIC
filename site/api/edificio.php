<?php
require_once "../func/init.php";

if ($METHOD === 'POST') {
    try {
    if($_POST['morada'] != "" && ctype_alpha($_POST['morada'])) {
          $query = $db->prepare("INSERT INTO edificio VALUES (:morada)");
          $query->bindParam(':morada', $morada);
          $morada = $_POST['morada'];
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
          $query = $db->prepare("DELETE FROM edificio WHERE morada=(:morada)");
          $query->bindParam(':morada', $morada);
          $morada = $_POST['morada'];
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
