<?php
require_once "../../func/init.php";
$title = "Listar Edifícios";

if(isset($_GET['edificio_id']) && !empty($_GET['edificio_id'])) {
    $edificio_id = int($_GET['edificio_id']);
    //Redirect to view.php
}

include $root . "template/head.php"; 
include $root . "template/navbar.php";

$table = edificio_getall($db);
view_table($table);
include $root . "template/foot.php";

?>
    <form action=<?= $root . "api/edificio.php" . "?redirect=" . $webroot . "pages/edificio/index"?> method="post">
        <p>Morada do novo edificio: <input type="text" name="morada"/></p>
        <p><input type="submit" value="Submit"/></p>
    </form>


