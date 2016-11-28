<?php
$root = "../../"
require_once $root + "func/init.php";
$title = "Listar Edifícios";

if(isset($_GET['edificio_id']) && !empty($_GET['edificio_id'])) {
    $edificio_id = int($_GET['edificio_id']);
    //Redirect to view.php
}

include $root + "template/head.php"; 
include $root + "template/navbar.php";
?>

Por aqui redirect para a pagina para criar um edificio
<BR/>

<?php
$table = edificio_getall();
view_table($table);
include $root + "template/foot.php";
?>