<?php
require_once "../func/init.php";

if ($METHOD === 'POST') {
     //TODO: criar um edificio
     echo "ok";
} else if ($METHOD === 'PUT') {
     //TODO: editar um edificio
     try {

     } catch(Exception $e) {
         echo "Error: Failed to complete the deletion...";
         exit();
     }
} else if ($METHOD === 'DELETE') {
     //TODO: apagar um edificio, usa 2 parametros
     //fazer aqui a query:
     try {

     } catch(Exception $e) {
         echo "Error: Failed to complete the deletion...";
         exit();
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
?>