<?php
require_once "../../func/init.php";
include $root . "model/espaco.php";
include $root . "view/table.php";
$title = "Listar Espacos";

if(isset($_GET['edificio_id']) && !empty($_GET['edificio_id'])) {
    $edificio_id = int($_GET['edificio_id']);
    //Redirect to view.php
}

include $root . "template/head.php"; 
include $root . "template/navbar.php";
$table = espaco_getall($db);
?>


<link rel="stylesheet" type="text/css" href="<?= $webroot ?>/assets/css/tabler.css">

 <div class="jumbotron">
    <div class="container">  
        <h3>
            Criar Espaco
        </h3>
        <form action=<?= $webroot . "/api/espaco.php" . "?redirect=" . $webroot . "pages/espaco/"?> method="post">
            <p>Morada do edificio em que esta inserido: <input type="text" name="morada"/></p>
            <p>Codigo do Espaco: <input type="text" name="codigo"/></p>
            <p><input type="submit" value="Submit"/></p>
        </form>
    </div>
</div>


<div class="container">

<?php
function make_request_btn($morada, $codigo) {
    global $webroot;
    return '<td style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    '<button type="submit" data-original-title="Apagar espaco" data-placement="bottom" data-toggle="tooltip"'.'class="tooltipper" class="btn btn-xs btn-danger"> <span class="glyphicon glyphicon-trash"></span>&nbsp; </button>'.
    '</form></td>';
}
    draw_table($table, "Lista de alugaveis", null, ["Morada", "Codigo", "Accoes"], [null, null, 'make_request_btn'], [["morada"], ["codigo"], ["morada", "codigo"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>