<?php
require_once "../../func/init.php";
include $root . "model/oferta.php";
include $root . "view/table.php";
$title = "Listar Oferta";

include $root . "template/head.php"; 
include $root . "template/navbar.php";
$table = oferta_getall($db);
?>


<link rel="stylesheet" type="text/css" href="<?= $webroot ?>/assets/css/tabler.css">

 <div class="jumbotron">
    <div class="container">  
        <h3>
            Criar Oferta
        </h3>
        <form class="betterform" action=<?= $webroot . "/api/oferta.php" . "?redirect=" . $webroot . "pages/oferta/"?> method="post">
            <label for="input-morada">Morada do Alugavel:</label>
            <input type="text" name="morada" id="input-morada"/>
            <label for="input-codigo">Codigo do alugavel:</label>
            <input type="text" name="codigo" id="input-codigo"/>
            <label for="input-data_inicio">Data de inicio da Oferta: </label>
            <input type="text" name="data_inicio" id="input-data_inicio"/>
            <label for="input-data_fim">Data de fim da oferta:</label>
            <input type="text" name="data_fim" id="input-data_fim"/>
            <label for="input-tarifa">Tarifa:</label>
            <input type="text" name="tarifa" id="input-tarifa"/>
            <input type="submit" value="Submit"/>
        </form>
    </div>
</div>


<div class="container">

<?php
function make_request_btn($morada, $codigo, $data_inicio) {
    global $webroot;
    return '<td class="table-buttons" style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    "<form action='$webroot/api/edificio.php?callback=".urlencode("$webroot/pages/edificio")."' method='POST'>".
    '<input type="hidden" name="_method" value="DELETE" />'.
    '<input type="hidden" name="morada" value="'.htmlspecialchars($morada, ENT_QUOTES, 'UTF-8').'" />'.
    '<input type="hidden" name="codigo" value="'.htmlspecialchars($codigo, ENT_QUOTES, 'UTF-8').'" />'.
    '<input type="hidden" name="data_inicio" value="'.htmlspecialchars($data_inicio, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Apagar oferta" data-placement="bottom" data-toggle="tooltip"'.
    'class="tooltipper" class="btn btn-xs btn-danger"><i class="glyphicon glyphicon-trash"></i></button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Postos", null, ["Morada", "Codigo", "Data Inicial", "Data Final", "Tarifa", "Accoes"], [null, null, null, null, null, 'make_request_btn'], [["morada"], ["codigo"], ["data_inicio"], ["data_fim"], ["tarifa"], ["morada", "codigo", "data_inicio"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>