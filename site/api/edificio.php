<?php
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
     //TODO: criar um edificio
     echo "ok";
} else if ($_SERVER['REQUEST_METHOD'] === 'PUT') {
     //TODO: atualizar um edificio
     echo "ok";     
} else if ($_SERVER['REQUEST_METHOD'] === 'DELETE') {
     //TODO: apagar um edificio
     echo "ok";
} else if ($_SERVER['REQUEST_METHOD'] === 'GET') {
     echo "invalid request";
} else {
     echo "unknown request";
}
?>