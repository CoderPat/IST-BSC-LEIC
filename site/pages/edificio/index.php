<?php
require_once "func/init.php";
$title = "Listar Edifícios";

if(isset($_GET['edificio_id']) && !empty($_GET['edificio_id'])) {
    $edificio_id = int($_GET['edificio_id']);
    //Redirect to view.php
}

include "template/head.php"; 
include "template/navbar.php";
?>

Por aqui redirect para a pagina para criar um edificio
<BR/>

<?php
$table = edificio_getall();
view_table($table);
include "template/foot.php";
?>