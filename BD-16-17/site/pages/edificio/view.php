<?php
require_once "../../func/init.php";
$title = "Ver edifício";
include $root . "template/head.php"; 
include $root . "template/navbar.php";

include $root . "model/edificio.php";
include $root . "view/table.php";

if (isset($_GET['morada']) && !empty($_GET['morada'])) {
    $morada = $_GET['morada'];
} else {
    $morada = 0;
}

$edificio_total = edificio_get_alugaveis_total($db, $morada); 
?>

<div class="container">
<br>
<?php
draw_table($edificio_total, "Total por Espaco em $morada", null, ["Codigo", "Total"], [null, null], [["codigo"], ["total"]], null);
?>
</div>

<?php
include $root . "template/foot.php";
?>
