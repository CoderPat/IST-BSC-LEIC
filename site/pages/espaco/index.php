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
        <form class="betterform" action=<?= $webroot . "/api/espaco.php" . "?callback=" . $webroot . "/pages/espaco/"?> method="POST">
            <label for="input-morada">Morada do edificio em que esta inserido:</label>
            <input type="text" name="morada" id="input-morada"/>
            <label for="input-codigo">Codigo do Espaco:</label>
            <input type="text" name="codigo" id="input-codigo"/>
            <input type="submit" value="Submit"/>
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