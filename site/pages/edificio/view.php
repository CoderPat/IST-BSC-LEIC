<?php
require_once "../../func/init.php";
$title = "Ver edifício";
include $root . "template/head.php"; 
include $root . "template/navbar.php";

if (isset($_GET['edificio_id']) && !empty($_GET['edificio_id'])) {
    $edificio_id = int($_GET['edificio_id']);
} else {
    $edificio_id = 0;
}


$edificio = edificio_get($edificio_id);
view_edificio($edificio);
$alugaveis = alugavel_getall_by_edificioid($edificio_id);
view_table($alugaveis);

include $root . "template/foot.php";
?>