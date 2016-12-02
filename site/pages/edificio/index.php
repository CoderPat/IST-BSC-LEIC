<?php
require_once "../../func/init.php";
include $root . "model/edificio.php";
include $root . "view/table.php";
$title = "Listar Edifícios";

if(isset($_GET['edificio_id']) && !empty($_GET['edificio_id'])) {
    $edificio_id = int($_GET['edificio_id']);
    //Redirect to view.php
}

include $root . "template/head.php"; 
include $root . "template/navbar.php";
$table = edificio_getall($db);
?>


<link rel="stylesheet" type="text/css" href="<?= $webroot ?>/assets/css/tabler.css">

 <div class="jumbotron">
    <div class="container">  
        <h3>
            Criar Edifício
        </h3>
        <form class="betterform" action=<?= $webroot . "/api/edificio.php" . "?callback=" . $webroot . "/pages/edificio/"?> method="POST">
            <label for="input-morada">Morada do novo edificio:</label>
            <input type="text" name="morada" id="input-morada"/>
            <input type="submit" value="Submit"/>
        </form>
    </div>
</div>


<div class="container">

<?php
function make_request_btn($morada) {
    global $webroot;
    return '<td class="table-buttons" style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    '<a data-original-title="Ver edificio" data-placement="bottom" data-toggle="tooltip" class="tooltipper" '.
    "href='$webroot/pages/edificio/view.php?morada=".urlencode($morada)."' >".
        '<i class="glyphicon glyphicon-circle-arrow-right"></i></a>'.
    "<form action='$webroot/api/edificio.php?callback=".urlencode("$webroot/pages/edificio")."' method='POST'>".
    '<input type="hidden" name="_method" value="DELETE" />'.
    '<input type="hidden" name="morada" value="'.htmlspecialchars($morada, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Apagar edificio" data-placement="bottom" data-toggle="tooltip"'.'class="tooltipper" class="btn btn-xs btn-danger"> <span class="glyphicon glyphicon-trash"></span>&nbsp; </button>'.
    '</form></td>';
}
    draw_table($table, "Lista de edifícios", null, ["morada", "acções"], [null, 'make_request_btn'], [["morada"], ["morada"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>
