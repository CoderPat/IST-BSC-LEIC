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
        <form action=<?= $webroot . "/api/oferta.php" . "?redirect=" . $webroot . "pages/oferta/"?> method="post">
            <p>Morada do Alugavel: <input type="text" name="morada"/></p>
            <p>Codigo do Espaco: <input type="text" name="codigo"/></p>
            <p>Data inicial da Oferta: <input type="text" name="data_inicio"/></p>
            <p>Data final da Oferta: <input type="text" name="data_fim"/></p>
            <p>Tarifa: <input type="text" name="tarifa"/></p>
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
    draw_table($table, "Lista de Postos", null, ["Morada", "Codigo", "Data Inicial", "Data Final", "Tarifa", "Accoes"], [null, null, null, null, null, 'make_request_btn'], [["morada"], ["codigo"], ["data_inicio"], ["data_fim"], ["tarifa"], ["morada", "codigo", "data_inicio"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>