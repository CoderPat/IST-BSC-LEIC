<?php
require_once "../../func/init.php";
include $root . "model/posto.php";
include $root . "view/table.php";
$title = "Listar Postos";

include $root . "template/head.php"; 
include $root . "template/navbar.php";
$table = posto_getall($db);
?>


<link rel="stylesheet" type="text/css" href="<?= $webroot ?>/assets/css/tabler.css">

 <div class="jumbotron">
    <div class="container">  
        <h3>
            Criar Posto
        </h3>
        <form action=<?= $webroot . "/api/posto.php" . "?redirect=" . $webroot . "pages/posto/"?> method="post">
            <p>Morada do Edificio em que esta inserido: <input type="text" name="morada"/></p>
            <p>Codigo do Espaco em que esta inserido: <input type="text" name="codigo_espaco"/></p>
            <p>Codigo do Posto: <input type="text" name="codigo"/></p>
            <p><input type="submit" value="Submit"/></p>
        </form>
    </div>
</div>



<div class="container">

<?php
function make_request_btn($morada, $codigo) {
    global $webroot;
    return '<td style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    '<button type="submit" data-original-title="Apagar oferta" data-placement="bottom" data-toggle="tooltip"'.'class="tooltipper" class="btn btn-xs btn-danger"> <span class="glyphicon glyphicon-trash"></span>&nbsp; </button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Postos", null, ["Morada", "Codigo do Espaco", "Codigo do Posto"], [null, null, null, 'make_request_btn'], [["morada"], ["codigo"], ["codigo_espaco"], ["morada", "codigo"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>